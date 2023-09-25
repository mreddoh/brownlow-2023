
# Load packages ----
library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
library(xgboost)
library(vip)

# speed up computation with parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Load data ----
version = list.files(here("model-files","model-versions")) %>% 
  tibble() %>% 
  filter(substr(.,1,1)=="v") %>% 
  mutate(value = as.numeric(substr(.,2,5))) %>% 
  summarise(max = paste0("v",format(max(value), nsmall = 2),"_model_gbm.RData")) %>% 
  pull(max)

load(file = here("model-files","model-versions",version)) #model_obj
#load(file = here("output","v1.24_model_gbm.RData")) #maybe better
load(file = here("data","player_data_2022.Rdata"))
load(file = here("model-files","preprocessing_recipe.RData"))

# Load model variables ----
model_vars <- c(
  "match_pct.goals",
  "match_pct.disposals",
  "match_pct.score_involvements",
  "match_pct.shots_at_goal",
  "match_pct.contested_possessions",
  "match_pct.inside_fifties",
  "match_pct.handballs",
  "match_pct.tackles",
  "match_pct.contested_marks",
  "marks_inside_fifty",
  "match_pct.hitouts_to_advantage",
  "match_pct.ground_ball_gets",
  "team_result",
  "match_pct.goal_assists",
  "match_pct.pressure_acts",
  "hitout_win_percentage",
  "match_pct.intercept_marks",
  "match_pct.effective_kicks",
  "match_pct.goal_assists",
  "match_pct.score_launches",
  "match_pct.clearances",
  "match_pct.metres_gained",
  "player_position.cln",
  "time_on_ground_percentage",
  "match_pct.rebounds",
  "team_pct.contest_def_one_on_ones"
)

# Apply to 2023 season data ----

## * Prep data ----
team_totals <- player_data_2022 %>% 
  group_by(match_id, player_team) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1:2],paste0('team.', names(.)[3:ncol(.)])))

match_totals <- player_data_2022 %>% 
  group_by(match_id) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1],paste0('match.', names(.)[2:ncol(.)])))

player_data_2022 %>% 
  left_join(.,team_totals,by=c("match_id","player_team")) %>% 
  left_join(.,match_totals,by=c("match_id")) ->
  player_data_2022

team_portions <- player_data_2022[27:78] / player_data_2022[,substr(names(player_data_2022),1,5)=="team."]
match_portions <- player_data_2022[27:78] / player_data_2022[,substr(names(player_data_2022),1,6)=="match."]

# assign new variable names
team_portions %>% setNames(object = ., nm = paste0('team_pct.', names(.)[1:ncol(.)])) -> team_portions
match_portions %>% setNames(object = ., nm = paste0('match_pct.', names(.)[1:ncol(.)])) -> match_portions

# combine
player_data_2022.cleaned <- cbind(player_data_2022,team_portions,match_portions) %>% 
  mutate(team_result = ifelse(match_winner==player_team, match_margin, -1*match_margin))

player_data_2022.cleaned$player_position.cln <- case_when(player_data_2022.cleaned$player_position %in% c('FF','CHF') ~ 'KEYFWD',
                                                          player_data_2022.cleaned$player_position %in% c('SUB','INT') ~ 'BENCH',
                                                          player_data_2022.cleaned$player_position %in% c('FB','CHB') ~ 'KEYBCK',
                                                          player_data_2022.cleaned$player_position %in% c('RK') ~ 'RUCK',
                                                          TRUE ~ "GENERAL")

ds_in <- player_data_2022.cleaned %>% 
  mutate(id = row_number()) %>% 
  dplyr::select(id, all_of(model_vars), brownlow_votes)

## * Apply model ----
oot_processed <- bake(preprocessing_recipe, new_data = ds_in)

oot_prediction <- predict(final_model_fit, oot_processed) %>%
  bind_cols(ds_in)

oot_out <- oot_prediction %>%
  dplyr::select(id,.pred) %>%
  rename(predicted_votes_raw = .pred) %>% 
  left_join(.,(player_data_2022.cleaned %>% mutate(id = row_number())),by=c("id")) %>% 
  dplyr::select(id:player_position,predicted_votes_raw) %>% 
  mutate(name = paste(player_first_name,player_last_name,sep=" "),
         season = substr(match_date,1,4))

# Calculate probability estimates ----

## * Load polynomial probability function fits ----
load(file = here("model-files","model-pr-adjustments","v1_fit.rdata"))
load(file = here("model-files","model-pr-adjustments","v2_fit.rdata"))
load(file = here("model-files","model-pr-adjustments","v3_fit.rdata"))

## * Apply to 2023 data ----
out_2022_matches <- oot_out %>% 
  mutate(v3.prob = predict(v3.fit,data.frame(x=.$predicted_votes_raw))) %>% 
  mutate(v3.prob = case_when(predicted_votes_raw > 3.5 ~ 0.975, predicted_votes_raw < -0.05 ~ 0, T ~ v3.prob) # that's probably fine
  ) %>% 
  dplyr::select(id,match_id,name,predicted_votes_raw,v3.prob)

# okay, so will need to normalise...
# Which should just be fine when doing the monte carlo simulation.

for (i in 1:250) {
  
  votes3 <- out_2022_matches %>% 
    uncount(round(v3.prob*100)) %>%
    mutate(rn = runif(nrow(.))) %>% 
    group_by(match_id) %>% 
    top_n(1, rn) %>%
    mutate(votes = 3) %>% 
    dplyr::select(match_id,name,votes)
    
  votes2 <- out_2022_matches %>% 
    filter(!paste0(match_id,name) %in% (votes3 %>% mutate(x = paste0(match_id,name)) %>% pull(x))) %>% 
    uncount(round(v3.prob*100)) %>%
    mutate(rn = runif(nrow(.))) %>% 
    group_by(match_id) %>% 
    top_n(1, rn) %>%
    mutate(votes = 2) %>% 
    dplyr::select(match_id,name,votes)
  
  votes1 <- out_2022_matches %>%
    filter(!paste0(match_id,name) %in% (votes3 %>% mutate(x = paste0(match_id,name)) %>% pull(x))) %>% 
    filter(!paste0(match_id,name) %in% (votes2 %>% mutate(x = paste0(match_id,name)) %>% pull(x))) %>% 
    uncount(round(v3.prob*100)) %>%
    mutate(rn = runif(nrow(.))) %>% 
    group_by(match_id) %>% 
    top_n(1, rn) %>%
    mutate(votes = 1) %>% 
    dplyr::select(match_id,name,votes) 
  
  votes <- rbind(votes1,votes2,votes3) %>% mutate(sim.no = i)
  
  if(i==1){votes_out <- votes}
  if(i>1){votes_out <- rbind(votes_out,votes)}
  
  if(i/10 == round(i/10)){print(paste0(i," simulations completed at: ",Sys.time()))}
  
}


monte.carlo.votesummary <- votes_out %>% 
  group_by(name,sim.no) %>% 
  summarise(total.votes = sum(votes)) %>% 
  group_by(sim.no) %>%
  mutate(win = ifelse(dense_rank(-total.votes)==1,1,0)) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  summarise(p05 = quantile(total.votes, probs = 0.05),
            mdn.votes = median(total.votes), 
            p95 = quantile(total.votes, probs = 0.95),
            win.perc = sum(win) / n())
  


# Calculate Votes ----

## * Apply votes using monte carlo method ----
player_match_combos <- out_2022_matches %>% dplyr::select(name,match_id) %>% mutate(votes = 0, n = NA)

n_sims = max(votes_out$sim.no, na.rm = T)

most_likely_321 <- votes_out %>%
  arrange(-votes) %>% 
  group_by(match_id,sim.no) %>%
  summarise(combo321 = paste(name, collapse=" > ")) %>%
  group_by(match_id,combo321) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  filter(row_number() == 1) %>%
  dplyr::select(-n)

  
### * Output graph like Channel 7 ----
top20 <- monte.carlo.votesummary %>% arrange(-mdn.votes) %>% filter(row_number() <= 20) %>% dplyr::select(name) %>% pull()
match_id2round <- oot_out %>% dplyr::select(match_id,match_round) %>% unique()
votesummary <- monte.carlo.votesummary %>% dplyr::select(name,Total = mdn.votes)

played_games <- oot_out %>% 
  dplyr::select(name,match_round) %>% 
  filter(name %in% top20)
  

output <- most_likely_321 %>% 
  left_join(.,match_id2round,by=c("match_id")) %>% 
  ungroup() %>% 
  separate(combo321, into = c('v3','v2','v1'), sep = '\\s>\\s') %>% 
  pivot_longer(v3:v1,names_to = "votes", values_to = "name") %>% 
  mutate(votes = as.integer(str_extract(votes,"\\d+"))) %>% 
  dplyr::select(name,match_round,votes) %>% 
  filter(name %in% top20) %>% 
  arrange(as.integer(match_round)) %>% 
  mutate(votes = as.character(votes)) %>% 
  left_join(played_games,.,by=c("name","match_round")) %>% 
  mutate(votes = ifelse(is.na(votes),0,votes)) %>% 
  pivot_wider(names_from = match_round, names_prefix = "R", values_from = votes, values_fill = "-") %>% 
  left_join(.,votesummary,by=c("name")) %>% 
  arrange(-Total)


