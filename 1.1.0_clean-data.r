
# Load packages ----
library(tidyverse)
library(here)

# Load data ----
load(file = here("2023 Model","data","player_data_full.Rdata"))

# Wrangle data into model-able dataset with normalised and cleaned variables ----
# Note. look at normalised values, for example, disposals as percentage of teams disposals...

## * Create team and match total variables ----
team_totals <- player_data_full %>% 
  group_by(match_id, player_team) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1:2],paste0('team.', names(.)[3:ncol(.) - 1])))

match_totals <- player_data_full %>% 
  group_by(match_id) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1],paste0('match.', names(.)[2:ncol(.) - 1])))

## * Join on variables ----
player_data_full %>% 
  left_join(.,team_totals,by=c("match_id","player_team")) %>% 
  left_join(.,match_totals,by=c("match_id")) ->
  player_data_full


team_portions <- player_data_full[27:78] / player_data_full[,substr(names(player_data_full),1,5)=="team."]
match_portions <- player_data_full[27:78] / player_data_full[,substr(names(player_data_full),1,6)=="match."]

## * Assign new variable names ----
team_portions %>% setNames(object = ., nm = paste0('team_pct.', names(.)[1:ncol(.)])) -> team_portions
match_portions %>% setNames(object = ., nm = paste0('match_pct.', names(.)[1:ncol(.)])) -> match_portions

## * Combine datasets ----
player_data_full.cleaned <- cbind(player_data_full,team_portions,match_portions)

# Add in new variables based result, i.e. was player in winning team? ----
player_data_full.cleaned$team_result <- ifelse(player_data_full.cleaned$match_winner==player_data_full.cleaned$player_team, 
                                               player_data_full.cleaned$match_margin, 
                                               -1*player_data_full.cleaned$match_margin)

# Add new variable for player position, FWD/BCK/MID/RUC
player_data_full.cleaned$player_position.cln <- case_when(player_data_full.cleaned$player_position %in% c('FF','CHF') ~ 'KEYFWD',
                                                          player_data_full.cleaned$player_position %in% c('SUB','INT') ~ 'BENCH',
                                                          player_data_full.cleaned$player_position %in% c('FB','CHB') ~ 'KEYBCK',
                                                          player_data_full.cleaned$player_position %in% c('RK') ~ 'RUCK',
                                                          TRUE ~ "GENERAL")

# Save data ----
save(player_data_full.cleaned, file = here("2023 Model","data","player_data_full.cleaned.Rdata"))
