
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
load(file = here("data","player_data_full.cleaned.Rdata"))

# Begin modeling ----

## * Prepare dataset specifically for modeling using gradient boosting algorithms ----
# Subset to potentially predictive variables, found in previous step ----
model_vars <- c(
  #"match_weather_type",
  #"player_position",
  "match_pct.goals",
  "match_pct.disposals",
  "match_pct.score_involvements",
  "match_pct.shots_at_goal",
  "match_pct.contested_possessions",
  "match_pct.inside_fifties",
  #"match_pct.kicks",
  "match_pct.handballs",
  #"match_pct.clangers",
  "match_pct.tackles",
  #"team_pct.metres_gained",
  "match_pct.contested_marks",
  # add new variables 
  "marks_inside_fifty",
  "match_pct.hitouts_to_advantage",
  "match_pct.ground_ball_gets",
  # more new variables
  "team_result",
  "match_pct.goal_assists",
  # and even more
  "match_pct.pressure_acts",
  "hitout_win_percentage",
  "match_pct.intercept_marks",
  "match_pct.effective_kicks",
  "match_pct.goal_assists",
  "match_pct.score_launches",
  "match_pct.clearances",
  # and try these too!
  #"disposal_efficiency_percentage",
  #"turnovers",
  "match_pct.metres_gained",
  "player_position.cln",
  "time_on_ground_percentage",
  "match_pct.rebounds",
  "team_pct.contest_def_one_on_ones"
)

ds_in <- player_data_full.cleaned %>%
  select(id, all_of(model_vars), brownlow_votes)

set.seed(3084)

## * Split into training and testing datasets ----
ds_split <- rsample::initial_split(data = ds_in, prop = 0.3, strata = brownlow_votes)

## * Use the recipes package to define these preprocessing steps, in what is called a “recipe” ----
preprocessing_recipe <- recipes::recipe(brownlow_votes ~ ., data = training(ds_split)) %>%
  recipes::step_string2factor(all_nominal()) %>% # convert categorical variables to factors
  recipes::step_other(all_nominal(), threshold = 0.01) %>% # combine low frequency factor levels
  recipes::step_rm(id) %>% # remove id variable
  recipes::step_nzv(all_nominal()) %>% # remove no variance predictors which provide no predictive information 
  prep()

save(preprocessing_recipe, file = here("model-files","preprocessing_recipe.RData"))

## * Apply our previously defined preprocessing recipe with bake() ----
ds_cv_folds <- recipes::bake(preprocessing_recipe, new_data = training(ds_split)) %>%  
  rsample::vfold_cv(v = 3)

## * Use the parsnip package to define the XGBoost model specification ----
xgboost_model <- parsnip::boost_tree(mode = "regression",
                                     trees = 500,
                                     min_n = tune(),
                                     tree_depth = tune(),
                                     learn_rate = tune(),
                                     loss_reduction = tune()) %>%
  set_engine("xgboost", objective = "reg:squarederror")

## * Use the tidymodel dials package to specify the parameter set ----
xgboost_params <- dials::parameters(#trees(range = c(100,1000)),
  min_n(range = c(15,45)),
  tree_depth(range = c(3,6)),
  learn_rate(),
  loss_reduction())

xgboost_grid <- dials::grid_max_entropy(xgboost_params, size = 25)

head(xgboost_grid)

## * Use the new tidymodel workflows package to add a formula to our XGBoost model specification ----
xgboost_wf <- workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(brownlow_votes ~ .)

## * Tune the model! ----

# hyperparameter tuning
xgboost_tuned <- tune::tune_grid(object = xgboost_wf,
                                 resamples = ds_cv_folds,
                                 grid = xgboost_grid,
                                 metrics = yardstick::metric_set(rmse, rsq, mae),
                                 control = tune::control_grid(verbose = TRUE))

xgboost_tuned %>%
  tune::show_best(metric = "rmse", n = 20)

xgboost_best_params <- xgboost_tuned %>%
  tune::select_best("rmse")

# xgboost_best_params <- xgboost_tuned %>%
#   tune::show_best(metric = "rmse", n = 20) %>%
#   filter(row_number()==3)

xgboost_best_params

xgboost_model_final <- xgboost_model %>% 
  finalize_model(xgboost_best_params)

## * Evaluate performance on test data ----

# check on training
train_processed <- bake(preprocessing_recipe, new_data = training(ds_split))

final_model_fit <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = train_processed) 

train_prediction <- final_model_fit %>%
  predict(new_data = train_processed) %>%
  bind_cols(training(ds_split))

train_prediction %>%
  yardstick::metrics(brownlow_votes, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

final_model_fit %>% vip:::vip(., num_features = 30)

version = list.files(here("model-files","model-versions")) %>% 
  tibble() %>% 
  filter(substr(.,1,1)=="v") %>% 
  mutate(value = as.numeric(substr(.,2,5))) %>% 
  summarise(max = paste0("v",format(max(value) + 0.01, nsmall = 2),"_model_gbm.RData")) %>% 
  pull(max)

#save(final_model_fit, file = here("model-files","model-versions","1.00_model_gbm.RData"))
save(final_model_fit, file = here("model-files","model-versions",version))



