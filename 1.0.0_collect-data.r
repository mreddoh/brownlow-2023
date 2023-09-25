
# Load packages ----
library(tidyverse)
library(here)
library(fitzRoy)

# Use fitzRoy to collect data ----
player_data_2023 <- fetch_player_stats(season = 2023, source = "fryzigg")
player_data_2022 <- fetch_player_stats(season = 2022, source = "fryzigg")
player_data_full <- fetch_player_stats(season = 2012:2022, source = "fryzigg")
player_data_partial <- fetch_player_stats(season = 1992:2011, source = "fryzigg")


# Remove finals! ----
player_data_2023 %>% filter(str_starts(match_round, "^[0-9]")) %>% mutate(id = row_number()) -> player_data_2023
player_data_2022 %>% filter(str_starts(match_round, "^[0-9]")) %>% mutate(id = row_number()) -> player_data_2022
player_data_full %>% filter(str_starts(match_round, "^[0-9]")) %>% mutate(id = row_number()) -> player_data_full
player_data_partial %>% filter(str_starts(match_round, "^[0-9]")) %>% mutate(id = row_number()) -> player_data_partial


# Save files ----
save(player_data_2023, file = here("data","player_data_2023.Rdata"))
save(player_data_2022, file = here("data","player_data_2022.Rdata"))
save(player_data_full, file = here("data","player_data_full.Rdata"))
save(player_data_partial, file = here("data","player_data_partial.Rdata"))

