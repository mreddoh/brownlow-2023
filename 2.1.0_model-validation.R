
# Load packages ----
library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
library(xgboost)
library(vip)

# check on test
test_processed  <- bake(preprocessing_recipe, new_data = testing(ds_split))

test_prediction <- final_model_fit %>%
  predict(new_data = test_processed) %>%
  bind_cols(testing(ds_split))

test_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

# SHAP values!


# Merge predictions back onto original dataset ----
full_processed <- bake(preprocessing_recipe, new_data = ds_in)

full_prediction <- predict(object = final_model_fit, new_data = full_processed) %>%
  bind_cols(ds_in)

full_out <- full_prediction %>%
  select(id,.pred) %>%
  rename(predicted_votes_raw = .pred) %>% 
  left_join(.,player_data_full.cleaned,by=c("id")) %>% 
  select(id:player_position) %>% 
  mutate(name = paste(player_first_name,player_last_name,sep=" "),
         season = substr(match_date,1,4))


score.2.votes <- full_out %>% 
  mutate(predicted_grp = cut(predicted_votes_raw, breaks = c(-Inf,seq(-1,2.75,0.05),Inf))) %>%
  group_by(predicted_grp) %>%
  mutate(min_score = round(min(predicted_votes_raw),2)) %>%
  group_by(min_score, predicted_grp,brownlow_votes) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from = brownlow_votes, names_prefix = "v", values_from = n) %>% 
  arrange(predicted_grp)

score.2.votes$Total <- rowSums(score.2.votes[,3:6], na.rm = T)

score.2.votes <- score.2.votes %>% 
  mutate(p.0v = v0 / Total,
         p.1v = v1 / Total,
         p.2v = v2 / Total,
         p.3v = v3 / Total)

ggplot(score.2.votes, aes(x=min_score, y=p.1v)) + geom_point()
ggplot(score.2.votes, aes(x=min_score, y=p.2v)) + geom_point()
ggplot(score.2.votes, aes(x=min_score, y=p.3v)) + geom_point()



# Calculate Votes ----

check <- full_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup()

accuracy.model.3v <- check %>% 
  group_by(season) %>%
  summarise(percentage.correct = sum(ifelse(votes==3 & brownlow_votes==3,1,0)) / sum(ifelse(brownlow_votes==3,1,0)))

accuracy.model.v <- check %>% 
  group_by(season) %>%
  summarise(percentage.correct = sum(ifelse(votes>0 & brownlow_votes>0,1,0)) / sum(ifelse(brownlow_votes>0,1,0)))


# What is the common thread of being WRONG!

check %>% 
  group_by(player_position) %>% 
  summarise(percentage.correct = sum(ifelse(votes>0 & brownlow_votes>0,1,0)) / sum(ifelse(brownlow_votes>0,1,0)))



## * Apply votes using dplyr method ----
brownlow_votes <- full_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  select(match_id,season,name,votes)


## * Check Brownlow Medal Tally ----
brownlow_votes %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)

## * Check Fyfe ----

actual.brownlow_results <- full_out %>% group_by(season,name) %>% summarise(actual_tally = sum(brownlow_votes))

comparison <- brownlow_votes %>% 
  group_by(season,name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  left_join(.,actual.brownlow_results,by=c("name","season")) %>% 
  filter(season<2022) %>% 
  mutate(error = actual_tally - medal_tally)


ggplot(comparison, aes(x=actual_tally, y=medal_tally)) + geom_point() + geom_smooth()



brownlow_votes %>% 
  filter(season==2017) %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  arrange(-medal_tally)

brownlow_votes %>% 
  filter(name == "Nat Fyfe") %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)

## * Check the Neale effect ----
brownlow_votes %>% 
  filter(name == "Lachie Neale") %>% 
  group_by(season, name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  filter(medal_tally == max(medal_tally)) %>% 
  arrange(season)

## * Check 2020 ----
brownlow_votes %>% 
  filter(season==2021) %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>%
  arrange(-medal_tally)
