#Big Data Bowl Project Workspace


#Filtering out Big Data for the Set of Plays We want in bdb_pbp_clean
desired_playIds <- bdb_pbp_clean$playId
desired_gameIds <- bdb_pbp_clean$gameId

tracking_proj <- tracking_all_weeks %>%
       semi_join(data.frame(playId = desired_plays, gameId = desired_gameIds), by = c("playId", "gameId"))

tackles_proj <- tackles %>%  semi_join(data.frame(playId = desired_plays, gameId = desired_gameIds), by = c("playId", "gameId"))

plays_proj <- plays %>% semi_join(data.frame(playId = desired_plays, gameId = desired_gameIds), by = c("playId", "gameId"))

games_proj <- games %>% filter(gameId %in% desired_gameIds)

tracking_proj <- inner_join(tracking_proj, plays_proj, by = c("gameId", "playId"))

tracking_proj <- inner_join(tracking_proj, games_proj, by = "gameId")

#Standardizing Desired Plays Data
tracking_proj <- tracking_proj %>% 
  mutate(ToLeft = playDirection == "left", 
         IsBallCarrier = nflId == ballCarrierId) 


#standardize direction of left to right
tracking_proj1 <- tracking_proj %>% 
  mutate(TeamOnOffense = ifelse(possessionTeam == homeTeamAbbr, homeTeamAbbr, visitorTeamAbbr),  
         IsOnOffense = club == TeamOnOffense,  ## Is player on offense?
         YardsFromOwnGoal = ifelse(as.character(yardlineSide) == possessionTeam, 
                                   yardlineNumber, 50 + (50-yardlineNumber)), 
         YardsFromOwnGoal = ifelse(yardlineNumber == 50, 50, YardsFromOwnGoal),  
         X_std = ifelse(ToLeft, 120-x, x) - 10, ## Standardizes X
         Y_std = ifelse(ToLeft, 160/3-y, y))    ## Standardized Y

#Standardizing Angle
tracking_proj2 <- tracking_proj1 %>% 
  mutate(Dir_std_1 = ifelse(ToLeft & dir < 90, dir + 360, dir), 
         Dir_std_1 = ifelse(!ToLeft & dir > 270, dir - 360, Dir_std_1), 
         Dir_std_2 = ifelse(ToLeft, Dir_std_1 - 180, Dir_std_1))

tracking_proj2 <- tracking_proj2 %>% 
  mutate(X_std_end = s*cos((90-Dir_std_2)*pi/180) + X_std, 
         Y_std_end = s*sin((90-Dir_std_2)*pi/180) + Y_std)



#Rename tracking_proj2 
std_tracking <- tracking_proj2

test <- std_tracking %>% filter(playId == 57, gameId == 2022101000)

filter_test <- filtered_std_tracking %>% filter(playId == 100, gameId == 2022110300)

passes <- std_tracking %>% filter(event == "pass_forward")

runs <- std_tracking %>% filter(event == "run")

line_set <- std_tracking %>% filter(event == "snap_direct")


#Find unique names of events
unique_events <- unique(std_tracking$event)
print(unique_events)

#filter out qb_scrambles

#bdb_pbp_clean <- bdb_pbp_clean %>% filter(qb_scramble == 0)
#Merge max_a_frameId from new_df with pbp_data

temp_fix <- merge(new_df, bdb_pbp_clean, by = c("playId","gameId"))

tackle_pbp <- merge(final_plays, tackles, by = c("playId", "gameId"))

tackle_pbp <- tackle_pbp %>% filter(tackle == 1)
#Merge with pbp_data

std_tracking$gameId <- as.character(std_tracking$gameId)
bdb_pbp_clean$gameId <- as.character(bdb_pbp_clean$gameId)

#std_tracking <- inner_join(std_tracking, bdb_pbp_clean, by = c("playId", "gameId"))



#Merge with players data
std_tracking <- std_tracking %>%
       mutate(
             position = players_df$position[match(nflId, players_df$nflId)]
         )

#Grab tackles data
plays_proj <- final_plays %>% dplyr::select(playId, gameId)



tackles_proj <- semi_join(tackles, plays_proj, by = c("gameId", "playId"))

#Create IsBallCarrier

std_tracking <- std_tracking %>%
  mutate(IsBallCarrier = ifelse(nflId == ballCarrierId, TRUE, FALSE))

#Filter by play defining events: 
start_of_play <- c("ball_snap","snap_direct","autoevent_ballsnap")

first_action <- c("handoff","play_action","pass_forward","lateral")

second_action <- c("run","first_contact")

end_of_play <- c("tackle","fumble","qb_slide","qb_sack","out_of_bounds","fumble_offense_recovered","fumble_defense_recovered")

event_list <- c(start_of_play, first_action, second_action, end_of_play)

#Calculate each player's distance from the ball carrier
#Assign a new column with ball carrier location for each entry
std_tracking <- std_tracking %>%
       group_by(frameId, playId, gameId) %>%
       mutate(
             ball_carrier_X = X_std[which(nflId == ballCarrierId)],
             ball_carrier_Y = Y_std[which(nflId == ballCarrierId)]
         )

#Calculate each player's distance from the ball carrier
std_tracking <- std_tracking %>%
  mutate(
    distance_from_ball_carrier = sqrt((X_std - ball_carrier_X)^2 + (Y_std - ball_carrier_Y)^2)
  )


#Create variable of snap location
std_tracking <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(
    snap_X = X_std[which(club == "football" & frameId == 1)],
    snap_Y = Y_std[which(club == "football" & frameId == 1)]
  )

#Create distance from snap location variables
std_tracking <- std_tracking %>%
  mutate(
    distance_from_snap_location = sqrt((X_std - snap_X)^2 + (Y_std - snap_Y)^2)
  )

#Line of scrimmage variable
std_tracking <- std_tracking %>%
  group_by(gameId, playId) %>%
  mutate(
    los = snap_X
)


#Attach player database

std_tracking <- merge(std_tracking, players, by = "nflId")

#Create dummy variable of safety charged with run fitting, by finding closest safety to ball carrier at the end of the play
std_tracking <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(
    fitting_safety_id = ifelse(any(position %in% c("FS", "SS") & frameId == max(frameId)), nflId[position %in% c("FS", "SS") & frameId == max(frameId)][which.min(distance_from_ball_carrier[position %in% c("FS", "SS") & frameId == max(frameId)])], NA)
  )

#Put in location of safety
std_tracking <- std_tracking %>%
  group_by(frameId, playId, gameId) %>%
  mutate(
    fitting_safety_X = X_std[which(nflId == fitting_safety_id)[1]],
    fitting_safety_Y = Y_std[which(nflId == fitting_safety_id)[1]]
  )

#Create variable for frameId where the safety first gets in the vertical gap
std_tracking <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(
    frame_where_gap_filled = ifelse(
      any(abs(ball_carrier_Y - fitting_safety_Y) <= 5 & abs(ball_carrier_X - fitting_safety_X) <= 9),
      min(frameId[abs(ball_carrier_Y - fitting_safety_Y) <= 5 & abs(ball_carrier_X - fitting_safety_X) <= 9]),
      NA
    )
  ) %>%
  ungroup()

#Create variable for where distance between the safety and ball carrier is minimized
std_tracking <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(
    frame_where_gap_filled = ifelse(
      distance_from_ball_carrier == min(distance_from_ball_carrier),
      frameId,
      NA
    )
  ) %>%
  ungroup()


#Calculate each player's distance from where they started
#Create total_distance variable, which sums the distance travlled up until that frame
std_tracking <- std_tracking %>%
  group_by(playId, gameId, nflId) %>%
  mutate(distance_travelled_ttp = cumsum(dis)) %>%
  ungroup()

#Find where ball carrier speed is minimized after they've begun their run, simulating where the tackle or end of play is
std_tracking <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(
    run_min_acc = ifelse(
      nflId == ballCarrierId & frameId > max_a_frameId,
      frameId[which.min(s)],
      NA
    )
  ) %>%
  ungroup()

#Find frame where ball carrier is furthest from the line of scrimmage and behind the LOS- merge with pbp data then merge with tracking data
max_a_behind_los_df <- std_tracking %>%
       group_by(playId, gameId) %>%
       filter(nflId == ballCarrierId & ball_carrier_X < los) %>%
       reframe(max_a_frameId = frameId[a == max(a)])
colnames(max_a_behind_los_df) <- c("playId","gameId","max_a_frameId")

#Finding frame where runner crosses LOS
cross_los_df <- std_tracking %>%
  group_by(playId, gameId) %>%
  mutate(distance_from_snap_X = X_std - snap_X) %>%
  filter(ballCarrierId == nflId & distance_from_snap_X > 0) %>%
  summarize(min_distance_from_snap_X = min(distance_from_snap_X)) %>%
  ungroup()


#Merge with tackles
fitting_safety_df <- std_tracking %>%
  left_join(tackles_proj, by = c("playId", "gameId", "nflId")) %>%
  group_by(playId, gameId, nflId) %>%
  filter(fitting_safety_id %in% nflId)

fitting_safety_df <- fitting_safety_df %>% rename("fitting_safety_tackle" = tackle ,  "fitting_safety_assist" = assist,"fitting_safety_forced_fumble" = forcedFumble, "fitting_safety_missed_tackle" = pff_missedTackle)

fitting_safety_df$fitting_safety_tackle <- ifelse(is.na(fitting_safety_df$fitting_safety_tackle), 0, fitting_safety_df$fitting_safety_tackle)

fitting_safety_df$fitting_safety_assist <- ifelse(is.na(fitting_safety_df$fitting_safety_assist), 0, fitting_safety_df$fitting_safety_assist)

fitting_safety_df$fitting_safety_forced_fumble <- ifelse(is.na(fitting_safety_df$fitting_safety_forced_fumble), 0, fitting_safety_df$fitting_safety_forced_fumble)

fitting_safety_df$fitting_safety_missed_tackle <- ifelse(is.na(fitting_safety_df$fitting_safety_missed_tackle), 0, fitting_safety_df$fitting_safety_missed_tackle)

fitting_safety_df <- fitting_safety_df %>% dplyr::select(fitting_safety_id, fitting_safety_tackle, fitting_safety_assist, fitting_safety_forced_fumble, fitting_safety_missed_tackle, everything())

fitting_safety_test <- fitting_safety_df %>% filter(playId == 270 & gameId ==2022101300)



#Select data frame for model
df1 <- fitting_safety_df %>% dplyr::select(fitting_safety_tackle, s,a,dis,o, X_std, Y_std,Dir_std_2, 
                                           ball_carrier_X, ball_carrier_Y, distance_from_ball_carrier)

df2<- fitting_safety_df %>% dplyr::select(fitting_safety_tackle,frameId,nflId,club,gameId, playId ,event,s,a,dis,o, yardsToGo, YardsFromOwnGoal, X_std, Y_std, Dir_std_2, 
                                          ball_carrier_X, ball_carrier_Y, distance_from_ball_carrier, distance_from_snap_location, distance_travelled_ttp)
df1 <- df1 %>% ungroup()

df2$event <- ifelse(is.na(df2$event), "none", df2$event)


df1 <- na.omit(df1)
df2 <- na.omit(df2)

df1 <- df1 %>% dplyr::select(fitting_safety_tackle, everything()) %>% dplyr::select(-c(playId, gameId,nflId))


smp_size <- floor(0.80 *nrow(df1))
ind <- sample(seq_len(nrow(df1)),size = smp_size)
train_data <- as.matrix(df1[ind, ])
test_data <- as.matrix(df1[-ind, ])

Safety_run_fitting_tackle_model <- xgboost(
  data = train_data[, 2:11], 
  label = train_data[, 1],
  nrounds = 1000,
  objective = "binary:logistic",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = .075
)
importance_scores <- xgb.importance(model = Safety_run_fitting_tackle_model)

print(importance_scores)

vip(Safety_run_fitting_tackle_model)

pred_tackle_prob <- predict(Safety_run_fitting_tackle_model, test_data[, 2:11])
yhat <- pred_tackle_prob
y <- test_data[,1]
postResample(yhat, y)

# Make predictions using the model
pred_safety_tackle_prob <- as.data.frame(matrix(predict(Safety_run_fitting_tackle_model, as.matrix(df1[, -1]))))

# Rename the prediction column
colnames(pred_safety_tackle_prob) <- "pred_safety_tackle_prob"

# Combine the predictions with the original data frame, moving 'pred_draft_bin' to the second column
full_safety_run_fitting <- cbind(df1[, 1], pred_safety_tackle_prob, df2[, -1])

full_safety_run_fitting <- full_safety_run_fitting %>%
  group_by(playId, gameId) %>%
  arrange(frameId)


write.csv(full_safety_run_fitting, file = "~/Documents/Big Data Bowl/Safety Solo Tackle Model.csv", row.names = FALSE)

#Aggregate model data frame
#Grab four key events, filter out plays that do not have these
safety_charts <- full_safety_run_fitting %>%
  group_by(playId, gameId) %>%
  filter(all(c("ball_snap", "handoff", "first_contact", "tackle") %in% event)) %>%
  ungroup()

safety_charts <- safety_charts %>%
           filter(event %in% c("ball_snap", "handoff", "first_contact", "tackle", "autoevent_ballsnap"))

aggregated_data <- safety_charts %>%
    group_by(playId, gameId) %>%
    mutate(diff_pred_safety_tackle_prob = c(NA, diff(pred_safety_tackle_prob))) %>%
    slice(1:4) %>%
    select(fitting_safety_tackle,nflId,club ,gameId, playId, diff_pred_safety_tackle_prob) %>%
    drop_na(diff_pred_safety_tackle_prob) %>%
    mutate(event_sequence = case_when(
             row_number() == 1 ~ "ball_snap to handoff",
             row_number() == 2 ~ "handoff to first contact",
             row_number() == 3 ~ "first contact to tackle"
         ))
snap_to_handoff <- aggregated_data %>% filter(event_sequence == "ball_snap to handoff")

snap_to_handoff$diff_pred_safety_tackle_prob <- 100 * snap_to_handoff$diff_pred_safety_tackle_prob

handoff_to_contact <- aggregated_data %>% filter(event_sequence == "handoff to first contact")

handoff_to_contact$diff_pred_safety_tackle_prob <- 100 *  handoff_to_contact$diff_pred_safety_tackle_prob

contact_to_tackle <- aggregated_data %>% filter(event_sequence == "first contact to tackle")

contact_to_tackle$diff_pred_safety_tackle_prob <- 100 * contact_to_tackle$diff_pred_safety_tackle_prob

#Aggregate by nflId
snap_to_handoff_mean <- snap_to_handoff %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club))

handoff_to_contact_mean <- handoff_to_contact %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club))

contact_to_tackle_mean <- contact_to_tackle %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club))

#Rename entry_count to # of plays
snap_to_handoff_mean <- snap_to_handoff_mean %>%
  rename(Num_of_Plays = entry_count,
          avg_tackle_prob_increase = mean_diff_pred_safety_tackle_prob )

handoff_to_contact_mean <- handoff_to_contact_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

contact_to_tackle_mean <- contact_to_tackle_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

#Now merge back with players_df
snap_to_handoff_mean <- left_join(snap_to_handoff_mean, bdb_safeties, by = "nflId")

handoff_to_contact_mean <- left_join(handoff_to_contact_mean, bdb_safeties, by = "nflId")

contact_to_tackle_mean <- left_join(contact_to_tackle_mean, bdb_safeties, by = "nflId")

#Move displayName to the front
snap_to_handoff_mean <- snap_to_handoff_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 8)

handoff_to_contact_mean <- handoff_to_contact_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 8)

contact_to_tackle_mean <- contact_to_tackle_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 8)

#GT Tables for three types of 
sth_safety <- snap_to_handoff_mean %>%
  dplyr::select(gsis_id,displayName, club,avg_tackle_prob_increase, Num_of_Plays, position)

htc_safety <- handoff_to_contact_mean %>%
  dplyr::select(gsis_id, displayName,club ,avg_tackle_prob_increase, Num_of_Plays, position)

ctt_safety <- contact_to_tackle_mean %>%
  dplyr::select(gsis_id, displayName, club ,avg_tackle_prob_increase, Num_of_Plays, position)

sth_safety <- sth_safety %>% 
     arrange(desc(avg_tackle_prob_increase))

htc_safety <- htc_safety %>% 
   arrange(desc(avg_tackle_prob_increase))

ctt_safety <- ctt_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

sth_safety <- sth_safety %>%
  slice(1:5, 61:65)

htc_safety <- htc_safety %>%
  slice(1:5, 61:65)

ctt_safety <- ctt_safety %>%
  slice(1:5, 61:65)

#Snap to Handoff Final
snap_to_handoff_final <- sth_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from Snap to Handoff(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Snap to Handoff")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(snap_to_handoff_final)
gtsave(snap_to_handoff_final, file = "~/Documents/Big Data Bowl/snap_to_handoff_table_final.html")


#Handoff to First Contact
handoff_to_contact_final <- htc_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from Handoff to First Contact by Defense(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Handoff to First Contact by Defense")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(handoff_to_contact_final)
gtsave(handoff_to_contact_final, file = "~/Documents/Big Data Bowl/handoff_to_contact_table_final.html")


#Contact to Tackle
contact_to_tackle_final <- ctt_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from First Contact to Tackle Event by Defense(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle by Safety from Contact to Tackle Event by Defense")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(contact_to_tackle_final)
gtsave(contact_to_tackle_final, file = "~/Documents/Big Data Bowl/contact_to_tackle_table_final.html")

#Run Types Model
#Merge with tackles
fitting_safety_df <- std_tracking %>%
  left_join(tackles_proj, by = c("playId", "gameId", "nflId")) %>%
  group_by(playId, gameId, nflId) %>%
  filter(fitting_safety_id %in% nflId)

fitting_safety_df <- fitting_safety_df %>% rename("fitting_safety_tackle" = tackle ,  "fitting_safety_assist" = assist,"fitting_safety_forced_fumble" = forcedFumble, "fitting_safety_missed_tackle" = pff_missedTackle)

fitting_safety_df$fitting_safety_tackle <- ifelse(is.na(fitting_safety_df$fitting_safety_tackle), 0, fitting_safety_df$fitting_safety_tackle)

fitting_safety_df$fitting_safety_assist <- ifelse(is.na(fitting_safety_df$fitting_safety_assist), 0, fitting_safety_df$fitting_safety_assist)

fitting_safety_df$fitting_safety_forced_fumble <- ifelse(is.na(fitting_safety_df$fitting_safety_forced_fumble), 0, fitting_safety_df$fitting_safety_forced_fumble)

fitting_safety_df$fitting_safety_missed_tackle <- ifelse(is.na(fitting_safety_df$fitting_safety_missed_tackle), 0, fitting_safety_df$fitting_safety_missed_tackle)

fitting_safety_df <- fitting_safety_df %>% dplyr::select(fitting_safety_id, fitting_safety_tackle, fitting_safety_assist, fitting_safety_forced_fumble, fitting_safety_missed_tackle, everything())

fitting_safety_test <- fitting_safety_df %>% filter(playId == 270 & gameId ==2022101300)

#Join with run_type based on playId and gameId
run_types <- read.csv("~/Documents/Big Data Bowl/Big Data Bowl.R/bdb_run_types.csv")

fitting_safety_df <- merge(fitting_safety_df, run_types, by = c("playId","gameId"), all = FALSE)

#Select data frame for model
df1 <- fitting_safety_df %>% dplyr::select(fitting_safety_tackle, s,a,dis,o, X_std, Y_std,Dir_std_2, 
                                           ball_carrier_X, ball_carrier_Y, distance_from_ball_carrier)

df2<- fitting_safety_df %>% dplyr::select(fitting_safety_tackle,frameId,nflId,club,gameId, playId ,event,run_type,s,a,dis,o, yardsToGo, YardsFromOwnGoal, X_std, Y_std,Dir_std_1, Dir_std_2, 
                                          ball_carrier_X, ball_carrier_Y, distance_from_ball_carrier, distance_from_snap_location, distance_travelled_ttp)
df1 <- df1 %>% ungroup()

df2$event <- ifelse(is.na(df2$event), "none", df2$event)

df2$run_type <- ifelse(is.na(df2$run_type),"none",df2$run_type)

df1 <- na.omit(df1)
df2 <- na.omit(df2)

df1 <- df1 %>% dplyr::select(fitting_safety_tackle, everything()) %>% dplyr::select(-c(playId, gameId,nflId))


smp_size <- floor(0.80 *nrow(df1))
ind <- sample(seq_len(nrow(df1)),size = smp_size)
train_data <- as.matrix(df1[ind, ])
test_data <- as.matrix(df1[-ind, ])

Safety_run_fitting_tackle_model_run_types <- xgboost(
  data = train_data[, 2:11], #Change to right number of columns
  label = train_data[, 1],
  nrounds = 1000,
  objective = "binary:logistic",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = 0.075
)
importance_scores <- xgb.importance(model = Safety_run_fitting_tackle_model_run_types)

print(importance_scores)

vip(Safety_run_fitting_tackle_model_run_types)

pred_tackle_prob <- predict(Safety_run_fitting_tackle_model_run_types, test_data[, 2:11])
yhat <- pred_tackle_prob
y <- test_data[,1]
postResample(yhat, y)

# Make predictions using the model
pred_safety_tackle_prob_run_types <- as.data.frame(matrix(predict(Safety_run_fitting_tackle_model_run_types, as.matrix(df1[, -1]))))

# Rename the prediction column
colnames(pred_safety_tackle_prob_run_types) <- "pred_safety_tackle_prob"

# Combine the predictions with the original data frame, moving 'pred_draft_bin' to the second column
full_safety_run_type_fitting <- cbind(df1[, 1], pred_safety_tackle_prob_run_types, df2[, -1])

full_safety_run_type_fitting <- full_safety_run_type_fitting %>% rename(fitting_safety_tackle = df1[,1])

full_safety_run_type_fitting <- full_safety_run_type_fitting %>% rename(fitting_safety_tackle = fitting_safety_tackle18794)

full_safety_run_type_fitting <- full_safety_run_type_fitting %>%
  group_by(playId, gameId) %>%
  arrange(frameId)


write.csv(full_safety_run_type_fitting, file = "~/Documents/Big Data Bowl/Safety Solo Tackle Model Run Type.csv", row.names = FALSE)

#Aggregate model data frame
#Grab four key events, filter out plays that do not have these
safety_charts_run_type <- full_safety_run_type_fitting %>%
  group_by(playId, gameId) %>%
  filter(all(c("ball_snap", "handoff", "first_contact", "tackle") %in% event)) %>%
  ungroup()

safety_charts_run_type <- safety_charts_run_type %>%
  filter(event %in% c("ball_snap", "handoff", "first_contact", "tackle", "autoevent_ballsnap"))

aggregated_data_run_type <- safety_charts_run_type %>% # first change is here to split by outside_zone, inside_zone, hb_toss, counter, and power
  group_by(playId, gameId) %>%
  mutate(diff_pred_safety_tackle_prob = c(NA, diff(pred_safety_tackle_prob))) %>%
  slice(1:4) %>%
  select(fitting_safety_tackle,nflId,club,run_type ,gameId, playId, diff_pred_safety_tackle_prob) %>%
  drop_na(diff_pred_safety_tackle_prob) %>%
  mutate(event_sequence = case_when(
    row_number() == 1 ~ "ball_snap to handoff",
    row_number() == 2 ~ "handoff to first contact",
    row_number() == 3 ~ "first contact to tackle"
  ))
inside_zone_sth <- aggregated_data_run_type %>% filter(run_type == "inside_zone" & event_sequence == "ball_snap to handoff")

inside_zone_sth$diff_pred_safety_tackle_prob <- 100 *inside_zone_sth$diff_pred_safety_tackle_prob

inside_zone_htc <- aggregated_data_run_type %>% filter(run_type == "inside_zone" & event_sequence == "handoff to first contact")

inside_zone_htc$diff_pred_safety_tackle_prob <- 100 *  inside_zone_htc$diff_pred_safety_tackle_prob

inside_zone_ctt <- aggregated_data_run_type %>% filter(run_type == "inside_zone" & event_sequence == "first contact to tackle")

inside_zone_ctt$diff_pred_safety_tackle_prob <- 100 * inside_zone_ctt$diff_pred_safety_tackle_prob

#Aggregate by nflId
inside_zone_sth_mean <- inside_zone_sth %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

inside_zone_htc_mean <- inside_zone_htc %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

inside_zone_ctt_mean  <- inside_zone_ctt %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

#Rename entry_count to # of plays
inside_zone_sth_mean <- inside_zone_sth_mean %>%
  rename(Num_of_Plays = entry_count,
         avg_tackle_prob_increase = mean_diff_pred_safety_tackle_prob )

inside_zone_htc_mean <- inside_zone_htc_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

inside_zone_ctt_mean <- inside_zone_ctt_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

#Now merge back with players_df
inside_zone_sth_mean <- left_join(inside_zone_sth_mean, bdb_safeties, by = "nflId")

inside_zone_htc_mean <- left_join(inside_zone_htc_mean, bdb_safeties, by = "nflId")

inside_zone_ctt_mean <- left_join(inside_zone_ctt_mean, bdb_safeties, by = "nflId")

#Move displayName to the front
inside_zone_sth_mean <- inside_zone_sth_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

inside_zone_htc_mean <- inside_zone_htc_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

inside_zone_ctt_mean <- inside_zone_ctt_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

#GT Tables for three types of 
sth_iz_safety <- inside_zone_sth_mean %>%
  dplyr::select(gsis_id,displayName ,club,run_type,avg_tackle_prob_increase,Num_of_Plays  ,position)

htc_iz_safety <- inside_zone_htc_mean %>%
  dplyr::select(gsis_id, displayName,club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

ctt_iz_safety <-inside_zone_ctt_mean %>%
  dplyr::select(gsis_id, displayName, club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

sth_iz_safety <- sth_iz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

htc_iz_safety <- htc_iz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

ctt_iz_safety <- ctt_iz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

sth_iz_safety <- sth_iz_safety %>%
slice(1:5, 21:25)

htc_iz_safety <- htc_iz_safety %>%
slice(1:5, 21:25)

ctt_iz_safety <- ctt_iz_safety %>%
slice(1:5, 21:25)

#Snap to Handoff Final
iz_snap_to_handoff_final <- sth_iz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,run_type = "Run Scheme",avg_tackle_prob_increase = "Average Change in Tackle Probability from Snap to Handoff(%)",
              position = "Position", club = "Team", Num_of_Plays = "Number of IZ Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Snap to Handoff on Inside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(iz_snap_to_handoff_final)
gtsave(iz_snap_to_handoff_final, file = "~/Documents/Big Data Bowl/iz_snap_to_handoff_table_final.html")


#Handoff to First Contact
iz_handoff_to_contact_final <- htc_iz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from Handoff to First Contact by Defense(%)",
              position = "Position", club = "Team", Num_of_Plays = "Number of IZ Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Handoff to First Contact by Defense on Inside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(iz_handoff_to_contact_final)
gtsave(iz_handoff_to_contact_final, file = "~/Documents/Big Data Bowl/iz_handoff_to_contact_table_final.html")


#Contact to Tackle
iz_contact_to_tackle_final <- ctt_iz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from First Contact to Tackle Event by Defense(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team", Num_of_Plays = "Number of IZ Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle by Safety from Contact to Tackle Event by Defense on Inside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(iz_contact_to_tackle_final)
gtsave(iz_contact_to_tackle_final, file = "~/Documents/Big Data Bowl/iz_contact_to_tackle_table_final.html")

#Aggregate model data frame for Outside Zone
#Grab four key events, filter out plays that do not have these
safety_charts_run_type <- full_safety_run_type_fitting %>%
  group_by(playId, gameId) %>%
  filter(all(c("ball_snap", "handoff", "first_contact", "tackle") %in% event)) %>%
  ungroup()

safety_charts_run_type <- safety_charts_run_type %>%
  filter(event %in% c("ball_snap", "handoff", "first_contact", "tackle", "autoevent_ballsnap"))

aggregated_data_run_type <- safety_charts_run_type %>% # first change is here to split by inside_zone, outside_zone, hb_toss, counter, and power
  group_by(playId, gameId) %>%
  mutate(diff_pred_safety_tackle_prob = c(NA, diff(pred_safety_tackle_prob))) %>%
  slice(1:4) %>%
  select(fitting_safety_tackle,nflId,club,run_type ,gameId, playId, diff_pred_safety_tackle_prob) %>%
  drop_na(diff_pred_safety_tackle_prob) %>%
  mutate(event_sequence = case_when(
    row_number() == 1 ~ "ball_snap to handoff",
    row_number() == 2 ~ "handoff to first contact",
    row_number() == 3 ~ "first contact to tackle"
  ))
outside_zone_sth <- aggregated_data_run_type %>% filter(run_type == "outside_zone" & event_sequence == "ball_snap to handoff")

outside_zone_sth$diff_pred_safety_tackle_prob <- 100 *outside_zone_sth$diff_pred_safety_tackle_prob

outside_zone_htc <- aggregated_data_run_type %>% filter(run_type == "outside_zone" & event_sequence == "handoff to first contact")

outside_zone_htc$diff_pred_safety_tackle_prob <- 100 *  outside_zone_htc$diff_pred_safety_tackle_prob

outside_zone_ctt <- aggregated_data_run_type %>% filter(run_type == "outside_zone" & event_sequence == "first contact to tackle")

outside_zone_ctt$diff_pred_safety_tackle_prob <- 100 * outside_zone_ctt$diff_pred_safety_tackle_prob

#Aggregate by nflId
outside_zone_sth_mean <- outside_zone_sth %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

outside_zone_htc_mean <- outside_zone_htc %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

outside_zone_ctt_mean  <- outside_zone_ctt %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

#Rename entry_count to # of plays
outside_zone_sth_mean <- outside_zone_sth_mean %>%
  rename(Num_of_Plays = entry_count,
         avg_tackle_prob_increase = mean_diff_pred_safety_tackle_prob )

outside_zone_htc_mean <- outside_zone_htc_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

outside_zone_ctt_mean <- outside_zone_ctt_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

#Now merge back with players_df
outside_zone_sth_mean <- left_join(outside_zone_sth_mean, bdb_safeties, by = "nflId")

outside_zone_htc_mean <- left_join(outside_zone_htc_mean, bdb_safeties, by = "nflId")

outside_zone_ctt_mean <- left_join(outside_zone_ctt_mean, bdb_safeties, by = "nflId")

#Move displayName to the front
outside_zone_sth_mean <- outside_zone_sth_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

outside_zone_htc_mean <- outside_zone_htc_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

outside_zone_ctt_mean <- outside_zone_ctt_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 2)

#GT Tables for three types of 
sth_oz_safety <- outside_zone_sth_mean %>%
  dplyr::select(gsis_id,displayName ,club,run_type,avg_tackle_prob_increase,Num_of_Plays  ,position)

htc_oz_safety <- outside_zone_htc_mean %>%
  dplyr::select(gsis_id, displayName,club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

ctt_oz_safety <-outside_zone_ctt_mean %>%
  dplyr::select(gsis_id, displayName, club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

sth_oz_safety <- sth_oz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

htc_oz_safety <- htc_oz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

ctt_oz_safety <- ctt_oz_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

#sth_oz_safety <- sth_oz_safety %>%
  slice(1:5, 21:25)

#htc_oz_safety <- htc_oz_safety %>%
  slice(1:5, 21:25)

#ctt_oz_safety <- ctt_oz_safety %>%
  slice(1:5, 21:25)

#Snap to Handoff Final
oz_snap_to_handoff_final <- sth_oz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,run_type = "Run Scheme",avg_tackle_prob_increase = "Average Change in Tackle Probability from Snap to Handoff(%)",
             position = "Position", club = "Team", Num_of_Plays = "Number of oz Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Snap to Handoff on Outside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(oz_snap_to_handoff_final)
gtsave(oz_snap_to_handoff_final, file = "~/Documents/Big Data Bowl/oz_snap_to_handoff_table_final.html")


#Handoff to First Contact
oz_handoff_to_contact_final <- htc_oz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from Handoff to First Contact by Defense(%)",
             position = "Position", club = "Team", Num_of_Plays = "Number of oz Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Handoff to First Contact by Defense on Outside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(oz_handoff_to_contact_final)
gtsave(oz_handoff_to_contact_final, file = "~/Documents/Big Data Bowl/oz_handoff_to_contact_table_final.html")


#Contact to Tackle
oz_contact_to_tackle_final <- ctt_oz_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from First Contact to Tackle Event by Defense(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team", Num_of_Plays = "Number of oz Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle by Safety from Contact to Tackle Event by Defense on Outside Zone")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(oz_contact_to_tackle_final)
gtsave(oz_contact_to_tackle_final, file = "~/Documents/Big Data Bowl/oz_contact_to_tackle_table_final.html")

#Aggregate model data frame for HB_Toss
#Grab four key events, filter out plays that do not have these
safety_charts_run_type <- full_safety_run_type_fitting %>%
  group_by(playId, gameId) %>%
  filter(all(c("ball_snap", "handoff", "first_contact", "tackle") %in% event)) %>%
  ungroup()

safety_charts_run_type <- safety_charts_run_type %>%
  filter(event %in% c("ball_snap", "handoff", "first_contact", "tackle", "autoevent_ballsnap"))

aggregated_data_run_type <- safety_charts_run_type %>% # first change is here to split by inside_zone, outside_zone, hb_toss, counter, and power
  group_by(playId, gameId) %>%
  mutate(diff_pred_safety_tackle_prob = c(NA, diff(pred_safety_tackle_prob))) %>%
  slice(1:4) %>%
  select(fitting_safety_tackle,nflId,club,run_type ,gameId, playId, diff_pred_safety_tackle_prob) %>%
  drop_na(diff_pred_safety_tackle_prob) %>%
  mutate(event_sequence = case_when(
    row_number() == 1 ~ "ball_snap to handoff",
    row_number() == 2 ~ "handoff to first contact",
    row_number() == 3 ~ "first contact to tackle"
  ))
hb_toss_sth <- aggregated_data_run_type %>% filter(run_type == "hb_toss" & event_sequence == "ball_snap to handoff")

hb_toss_sth$diff_pred_safety_tackle_prob <- 100 *hb_toss_sth$diff_pred_safety_tackle_prob

hb_toss_htc <- aggregated_data_run_type %>% filter(run_type == "hb_toss" & event_sequence == "handoff to first contact")

hb_toss_htc$diff_pred_safety_tackle_prob <- 100 *  hb_toss_htc$diff_pred_safety_tackle_prob

hb_toss_ctt <- aggregated_data_run_type %>% filter(run_type == "hb_toss" & event_sequence == "first contact to tackle")

hb_toss_ctt$diff_pred_safety_tackle_prob <- 100 * hb_toss_ctt$diff_pred_safety_tackle_prob

#Aggregate by nflId
hb_toss_sth_mean <- hb_toss_sth %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

hb_toss_htc_mean <- hb_toss_htc %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

hb_toss_ctt_mean  <- hb_toss_ctt %>%
  group_by(nflId) %>%
  summarize(mean_diff_pred_safety_tackle_prob = mean(diff_pred_safety_tackle_prob),
            entry_count = n(),
            club = first(club),
            run_type = first(run_type))

#Rename entry_count to # of plays
hb_toss_sth_mean <- hb_toss_sth_mean %>%
  rename(Num_of_Plays = entry_count,
         avg_tackle_prob_increase = mean_diff_pred_safety_tackle_prob )

hb_toss_htc_mean <- hb_toss_htc_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

hb_toss_ctt_mean <- hb_toss_ctt_mean %>%
  rename(`Num_of_Plays` = entry_count,
         `avg_tackle_prob_increase` = mean_diff_pred_safety_tackle_prob)

#Now merge back with players_df
hb_toss_sth_mean <- left_join(hb_toss_sth_mean, bdb_safeties, by = "nflId")

hb_toss_htc_mean <- left_join(hb_toss_htc_mean, bdb_safeties, by = "nflId")

hb_toss_ctt_mean <- left_join(hb_toss_ctt_mean, bdb_safeties, by = "nflId")

#Move displayName to the front
hb_toss_sth_mean <- hb_toss_sth_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 1)

hb_toss_htc_mean <- hb_toss_htc_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 1)

hb_toss_ctt_mean <- hb_toss_ctt_mean %>%
  select(displayName, everything()) %>%
  filter(Num_of_Plays >= 1)

#GT Tables for three types of 
sth_hb_safety <- hb_toss_sth_mean %>%
  dplyr::select(gsis_id,displayName ,club,run_type,avg_tackle_prob_increase,Num_of_Plays  ,position)

htc_hb_safety <- hb_toss_htc_mean %>%
  dplyr::select(gsis_id, displayName,club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

ctt_hb_safety <-hb_toss_ctt_mean %>%
  dplyr::select(gsis_id, displayName, club,run_type ,avg_tackle_prob_increase,Num_of_Plays,position)

sth_hb_safety <- sth_hb_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

htc_hb_safety <- htc_hb_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

ctt_hb_safety <- ctt_hb_safety %>% 
  arrange(desc(avg_tackle_prob_increase))

sth_hb_safety <- sth_hb_safety %>%
slice(1:5, 24:28)

htc_hb_safety <- htc_hb_safety %>%
slice(1:5, 24:28)

ctt_hb_safety <- ctt_hb_safety %>%
slice(1:5, 24:28)

#Snap to Handoff Final
hb_snap_to_handoff_final <- sth_hb_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,run_type = "Run Scheme",avg_tackle_prob_increase = "Average Change in Tackle Probability from Snap to Handoff(%)",
             position = "Position", club = "Team", Num_of_Plays = "Number of HB Toss Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Snap to Toss on HB Toss")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(hb_snap_to_handoff_final)
gtsave(hb_snap_to_handoff_final, file = "~/Documents/Big Data Bowl/hb_snap_to_handoff_table_final.html")


#Handoff to First Contact
hb_handoff_to_contact_final <- htc_hb_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from Handoff to First Contact by Defense(%)",
             position = "Position", club = "Team", Num_of_Plays = "Number of oz Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle made by Safety from Handoff to First Contact by Defense on HB Toss")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(hb_handoff_to_contact_final)
gtsave(hb_handoff_to_contact_final, file = "~/Documents/Big Data Bowl/hb_handoff_to_contact_table_final.html")


#Contact to Tackle
hb_contact_to_tackle_final <- ctt_hb_safety %>%
  gt() %>% 
  gt_theme_538 %>% 
  cols_label(displayName = "Player",gsis_id = "" ,avg_tackle_prob_increase = "Average Change in Tackle Probability from First Contact to Tackle Event by Defense(%)",
             Num_of_Plays = "Number of Outside Runs Fitted", position = "Position", club = "Team", Num_of_Plays = "Number of oz Schemes Fitted") %>%
  nflplotR::gt_nfl_headshots(columns = "gsis_id", height = 50) %>%
  nflplotR::gt_nfl_logos(columns = "club", height = 50) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = "Jacob Pickle: @pickleo7/ Alex Feazelle @alexfeazelle | Source: The National Football League, @nflfastR, nflplot" ) %>% 
  tab_header(
    title = md("Average Change in Probability of Tackle by Safety from Contact to Tackle Event by Defense on HB Toss")
  ) %>% 
  opt_align_table_header(align = "center") %>%  
  tab_options(
    table.font.size = px(16)
  ) %>%
  data_color(
    columns = vars(avg_tackle_prob_increase),
    colors = scales::col_numeric(
      palette = c("darkmagenta", "white", "darkgreen"),
      domain = NULL
    )
  )

print(hb_contact_to_tackle_final)
gtsave(hb_contact_to_tackle_final, file = "~/Documents/Big Data Bowl/hb_contact_to_tackle_table_final.html")

