library(nflreadr)
library(nflfastR)
library(tidyverse)
library(vroom)

#looad bdb data from csv
plays_df <- read_csv("~/Downloads/nfl-big-data-bowl-2024/plays.csv")
games_df <- vroom("~/Downloads/nfl-big-data-bowl-2024/games.csv", progress = vroom_progress())
players_df <- vroom("~/Downloads/nfl-big-data-bowl-2024/players.csv",progress = vroom_progress())
tackles_df <- vroom("~/Downloads/nfl-big-data-bowl-2024/tackles.csv",progress = vroom_progress())

#view(tackles_df)

#filter outside rushing plays| week 1-9.
pbp_bdb_2022_full_2 <- load_pbp(2021:2022) |> filter(play_type == "run",  qb_kneel == 0 , 
                                                     run_gap %in% c("end","tackle"), 
                                                     rush_touchdown == 0, out_of_bounds == 0, 
                                                     run_location %in% c("right","left"),
                                                     penalty == 0)  |> 
  filter(grepl("2022", game_id)) |> 
  filter(week %in% 1:9)

#names(pbp_bdb_2022_full_2)
view(pbp_bdb_2022_full_2)

#Get the df for the safeties and the linebackers
safeties_df <- players_df |> filter(position %in% c("SS","FS","DB")) |> 
  select(nflId,collegeName,position,displayName)
linebackers_df <- players_df |> filter(position %in% c("MLB","ILB")) |> 
  select(nflId,collegeName,position,displayName)


#getting the roster information for linebackers and safeties to get nflID

players_nflfastR <- load_players() |>
  rename(nflId = gsis_it_id) |> 
  select(nflId,gsis_id)
players_nflfastR$nflId <- as.numeric(as.character(players_nflfastR$nflId)) 

#test <- nflreadr::load_players() |> filter(position %in% c("MLB"|"ILB")| status == "ACT")
#view(test)

#view(safeties_nflfastR)
#view(linebackers_nflfastR)
#view(safeties_df)
#view(linebackers_df)

#connecting kaggle csv with nfl_ID

bdb_safeties <- left_join(safeties_df,players_nflfastR, by = "nflId") |> 
  filter(!is.na(gsis_id))
bdb_linebackers <- left_join(linebackers_df,players_nflfastR, by = "nflId") |> 
  filter(!is.na(gsis_id))
bdb_full_players_df <- rbind(bdb_safeties,bdb_linebackers)
#view(bdb_full_players_df)

#checking the df
#bdb_linebacker_fix <- anti_join(linebackers_df,bdb_linebackers| by = "nflId")
#bdb_safety_fix <- anti_join(safety_df,bdb_linebackers| by = "nflId")
#view(bdb_linebacker_fix)
#view(bdb_linebackers)
#view(bdb_safeties_fix)

#filtering plays where only the players in the 

#bdb_full_players_df have made a tackle
bdb_pbp_clean <- pbp_bdb_2022_full_2 |> filter(solo_tackle_1_player_id %in% bdb_full_players_df$gsis_id | assist_tackle_1_player_id %in% bdb_full_players_df$gsis_id |
                                                assist_tackle_2_player_id %in% bdb_full_players_df$gsis_id | assist_tackle_3_player_id %in% bdb_full_players_df$gsis_id |
                                                assist_tackle_4_player_id %in% bdb_full_players_df$gsis_id | 
                                                tackle_with_assist_1_player_id %in% bdb_full_players_df$gsis_id |
                                                tackle_with_assist_2_player_id %in% bdb_full_players_df$gsis_id) |> 
  rename(gameId = old_game_id,playId = play_id) |> 
select(playId,gameId)

#view(plays_df)

#converting pbp data ID's to numeric types 
bdb_pbp_clean$gameId <- as.numeric(as.character(bdb_pbp_clean$gameId)) 
bdb_pbp_clean$playId <- as.numeric(as.character(bdb_pbp_clean$playId)) 
final_plays <- left_join(bdb_pbp_clean,plays_df, by = c("gameId","playId")) |> 
  filter(!is.na(ballCarrierId)) 


view(final_plays)


view(bdb_pbp_clean)

nflreadr::.clear_cache()

run_types_df <- vroom("~/Desktop/NflfastR/Football R Code/BDB_2024/bdb_run_types.csv",progress = vroom_progress()) 
run_types_new <- run_types_df |> 
  mutate(run_type = case_when(run_type == "qb_scramble" ~ "QB Scramble",
                              run_type == "power" ~ "Power",
                              run_type == "hb_sweep" ~ "HB Sweep",
                              run_type == "hb_toss" ~ "HB Toss",
                              run_type == "broken_play" ~ "Broken Play",
                              run_type == "counter" ~ "Counter",
                              run_type == "inside_zone" ~ "Inside Zone",
                              run_type == "outside_zone" ~ "Outside Zone",
                              run_type == "read_option" ~ "Read Option (QB Keep)",
                              run_type == "zone_read"~ "Read Option (QB Keep)",
                              run_type == "trick_play" ~ "Trick Play",
                              run_type == "coou" ~ "Counter",
                              run_type == "rb_screen" ~ "RB Swing",
                              run_type == "reverse" ~ "Reverse",
                              run_type == "jet_sweep" ~ "Jet Sweep",
                              run_type == "oout" ~ "Outside Zone",
                              run_type == 'qb_draw' ~ "QB Scramble"
            ))
  
view(run_types_new)
vroom::vroom_write(run_types_new, file = "bdb_run_types_new.csv", delim = ",")

