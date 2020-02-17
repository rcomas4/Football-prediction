library(tidyverse)
library(stringr)
library(caret)
library(RSQLite)
library(corrplot) 
  
database = dbConnect(RSQLite::SQLite(), dbname = "database.sqlite")

matches <- as.data.frame(tbl(database, "Match"))
league <- as.data.frame(tbl(database,"League"))
country <- as.data.frame(tbl(database, "Country"))
teams <- as.data.frame(tbl(database, "Team"))
players <- as.data.frame(tbl(database, "Player"))
##-----------------------------------------------------------------------------------
## Selection of the most important variables
match_result <- matches %>% select(id, 
                                   country_id, 
                                   league_id, 
                                   season, 
                                   date, 
                                   match_api_id, 
                                   home_team_api_id, 
                                   away_team_api_id, 
                                   home_team_goal, 
                                   away_team_goal) %>%
  mutate(home_team_win = ifelse(home_team_goal > away_team_goal, 1, 0),
                                   draw = ifelse(home_team_goal == away_team_goal, 1, 0),
                                   away_team_win = ifelse(home_team_goal < away_team_goal, 1, 0),
                                  result = ifelse(home_team_goal > away_team_goal, "H", ifelse(home_team_goal < away_team_goal, "A", "D"))) 

#Summary of the home team
home_team_summary <- match_result %>% 
  group_by(home_team_api_id , season) %>%
  summarize(home_team_wins = sum(home_team_win), 
                           draw = sum(draw), 
                           away_team_wins = sum(away_team_win),
                           total_matches = n(),
                            goals = sum(home_team_goal)) %>%
  mutate(perc_home_win = home_team_wins/total_matches, mean_goals_h = goals/total_matches, perc_home_draw = draw/total_matches)

#Summary of the away team
away_team_summary <- match_result %>%
  group_by(away_team_api_id, season) %>%
  summarize(home_team_wins = sum(home_team_win),
            draw= sum(draw),
            away_team_wins = sum(away_team_win),
            total_matches = n(),
            goals = sum(away_team_goal)) %>%
  mutate(perc_away_win = away_team_wins/total_matches, mean_goals_a = goals/total_matches, perc_away_draw = draw/total_matches)

# Creating a database of home teams and another one of the away teams
home_team_summary <- home_team_summary %>% rename(team_api_id = home_team_api_id)
away_team_summary <- away_team_summary %>% rename(team_api_id = away_team_api_id)
perc_home_win <- home_team_summary %>% select(team_api_id, season, perc_home_win, perc_home_draw)
perc_away_win <- away_team_summary %>% select(team_api_id, season, perc_away_win, perc_away_draw)
mean_goals_home <- home_team_summary %>% select(team_api_id, season, mean_goals_h)
mean_goals_away <- away_team_summary %>% select(team_api_id, season, mean_goals_a)

# Database of home team
team_database_home <- teams %>% select(team_api_id, team_long_name, team_short_name) %>%
  left_join(perc_home_win) %>%
  left_join(mean_goals_home) %>%
  rename(home_team_api_id = team_api_id)

# Database of away team
team_database_away <- teams %>% select(team_api_id, team_long_name, team_short_name) %>%
  left_join(perc_away_win) %>%
  left_join(mean_goals_away) %>%
  rename(away_team_api_id = team_api_id)

match_final <- match_result %>% select(country_id, 
                                       league_id, 
                                       season, 
                                       match_api_id, 
                                       home_team_api_id, 
                                       away_team_api_id,
                                       home_team_win,
                                       result) %>%
  left_join(team_database_home, by = c("season", "home_team_api_id")) %>% 
  left_join(team_database_away, by = c("season", "away_team_api_id")) %>%
  select(country_id, league_id, season, match_api_id, home_team_api_id, perc_home_win, mean_goals_h, perc_home_draw, away_team_api_id, perc_away_win, mean_goals_a, perc_away_draw, result)

#----------------------------------------------------------------------------------------------------
## Some plots
# Plot with winning percentages
match_final %>% ggplot(aes(perc_home_win, perc_away_win, color = result)) + geom_point() + facet_grid(league_id ~ .)
match_final %>% ggplot(aes(perc_home_win, perc_away_win, color = result)) + geom_point() + facet_grid(season ~ .)

# Plot with mean goals
match_final %>% ggplot(aes(mean_goals_h, mean_goals_a, color = result)) + geom_point()

#---------------------------------------------------------------------------------------------------------------
#Creating new databases for each league:
unique(match_final$league_id)

## Belgium League
belgium_match <- match_result %>% filter(league_id == 1)

## Premier League
premier_match <- match_result %>% filter(league_id == 1729)


## Ligue 1
ligue1_match <- match_result %>% filter(league_id == 4769)
  
## Bundesliga 
bundesliga_match <- match_result %>% filter(league_id == 7809)

## Serie A
serieA_match <- match_result %>% filter(league_id == 10257)

  
## Eredivise
eredivise_match <- match_result %>% filter(league_id == 13274)

## Poland league
poland_match <- match_result %>% filter(league_id == 15722)

## Portgual league
portugal_match <- match_result %>% filter(league_id == 17642)

## Scotland Premier League
scotland_match <- match_result %>% filter(league_id == 19694)

## LaLiga
laliga_match <- match_result  %>% filter(league_id == 21518)

## Switzerland Super League

super_league <- match_result %>% filter(league_id == 24558)

# Teams names
teams_names <- teams%>% select(team_api_id, team_long_name, team_short_name)

#--------------------------------------------------------------------------------------------------------------
# Analysis of the Spanish League
#--------------------------------------------------------------------------------------------------------------
head(laliga_match)

laliga_match %>% summarize(perc_home = sum(home_team_win/n()),
                                          perc_away = sum(away_team_win/n()),
                                          perc_draw = sum(draw/n()))
laliga_match %>% 
  group_by(season) %>% 
  summarize(perc_home = sum(home_team_win/n()),
            perc_away = sum(away_team_win/n()),
            perc_draw = sum(draw/n())) %>%
  ggplot(aes(season,perc_home, fill = season)) + geom_bar(stat="identity")
  
# LaLiga home database
laliga_home_database <- laliga_match %>% 
  group_by(home_team_api_id, season) %>% 
  summarize(home_w = sum(home_team_win)/n(),
            home_t = sum(draw)/n(),
            home_l = sum(away_team_win)/n(),
            h_goal = mean(home_team_goal),
            h_r_goal = mean(away_team_goal)) %>%
  mutate(h_rel_goal = h_goal/h_r_goal)

# See the correlation of the variables of the home database
laliga_home_correlation <- cor(laliga_home_database[,3:8])
corrplot(laliga_home_correlation)
laliga_home_correlation

laliga_home_database %>% ggplot(aes(home_w, h_rel_goal)) + geom_point() + geom_smooth()
laliga_home_database %>% ggplot(aes(home_w, h_goal)) + geom_point() + geom_smooth()

# LaLiga away database
laliga_away_database <- laliga_match %>% 
  group_by(away_team_api_id, season) %>% 
  summarize(away_w = sum(away_team_win)/n(),
            away_t = sum(draw)/n(),
            away_l = sum(home_team_win)/n(),
            a_goal = mean(away_team_goal),
            a_r_goal = mean(home_team_goal)) %>%
  mutate(a_rel_goal = a_goal/a_r_goal)

#See the correlation of the variables of the away database
laliga_away_correlation <- cor(laliga_away_database[, 3:8])
laliga_away_correlation
corrplot(laliga_away_correlation)

laliga_away_database %>% ggplot(aes(away_w, a_rel_goal)) + geom_point() + geom_smooth()
laliga_away_database %>% ggplot(aes(away_w, a_goal)) + geom_point() + geom_smooth()

#As the winning perc variables are very correlated to the goals scored and the relation goal (scored/received), let's create a dataset with these variables
laliga_home_win <- laliga_home_database %>% select(season, home_team_api_id, home_w, h_goal, h_rel_goal, h_r_goal)
laliga_away_win <- laliga_away_database %>% select(season, away_team_api_id, away_w, a_goal, a_rel_goal,a_r_goal)


# Create a dataset with the master of Laliga matches and the variables of home_winning
h_laliga_win <- laliga_match %>%
  select(season, home_team_api_id, away_team_api_id, home_team_win) %>%
  left_join(laliga_home_win) %>%
  left_join(laliga_away_win)

#initialize dataframes
h_model1 = data.frame()
h_model2 = data.frame()
h_model3 = data.frame()
h_model4 = data.frame()
h_model5 = data.frame()
h_total_avg = data.frame()

B = c(1:10)

#Home win predictor
  
model = function(x){
  set.seed(x)
  #Create the validation dataset
  h_validation_index <- createDataPartition(h_laliga_win$home_team_win, times = 1, p= 0.2, list= FALSE)
  h_validation_set <- h_laliga_win[h_validation_index,]
  h_working_set <- h_laliga_win[-h_validation_index, ]
  
    #Create the training and testing datasets
  h_testing_index <- createDataPartition(h_working_set$home_team_win, times = 1, p = 0.2, list = FALSE)
  h_testing_set <- h_working_set[h_testing_index,]
  h_training_set <- h_working_set[-h_testing_index,]

  
  #First Model
 
  train_x <- h_training_set[,5:12]
  train_y <- as.factor(h_training_set[,4])
  test_x <- h_testing_set[,5:12]
  test_y <- as.factor(h_testing_set[,4])
  
  fit_knn <- train(train_x, train_y, method = "knn")
  y_hat_knn <- predict(fit_knn, test_x)
  h_testing_set1 <- h_testing_set %>% bind_cols(y_hat_knn = y_hat_knn)
  
  fit_rf <- train(train_x, train_y , method ="rf")
  y_hat_rf <- predict(fit_rf, test_x)
  h_testing_set1 <- h_testing_set1 %>% bind_cols(y_hat_rf = y_hat_rf)
  
  fit_nb <- train(train_x, train_y, method ="nb")
  y_hat_nb <- predict(fit_nb, test_x)
  h_testing_set1 <- h_testing_set1 %>% bind_cols(y_hat_nb = y_hat_nb)
  
  fit_svm <- svm(home_team_win ~ home_w + away_w + h_goal + h_rel_goal + h_r_goal + a_goal + a_rel_goal + a_r_goal, data= h_training_set, type = "C-classification")
  y_hat_svm <- predict(fit_svm, test_x)
  h_testing_set1 <- h_testing_set1 %>% bind_cols(y_hat_svm = y_hat_svm)
  
  fit_ksvm <- svm(home_team_win ~ home_w + away_w + h_goal + h_rel_goal + h_r_goal + a_goal + a_rel_goal + a_r_goal , data= h_training_set, type = "C-classification", kernel = "polynomial")
  y_hat_ksvm <- predict(fit_ksvm, test_x)
  h_testing_set1 <- h_testing_set1 %>% bind_cols(y_hat_ksvm = y_hat_ksvm)
  
  h_acc_model1 <- h_testing_set1  %>% summarize(knn_acc = mean(y_hat_knn == home_team_win),
                                           rf_acc = mean(y_hat_rf == home_team_win),
                                           nb_acc = mean(y_hat_nb == home_team_win),
                                           svm_acc = mean(y_hat_svm == home_team_win),
                                           ksvm_acc = mean(y_hat_ksvm == home_team_win))
  
  h_model1<- bind_rows(h_model1,h_acc_model1)
  

  h_accuracy = data.frame(h_model1)

  
  h_knn = mean(h_accuracy$knn_acc)
  h_rf = mean(h_accuracy$rf_acc)
  h_nb = mean(h_accuracy$nb_acc)
  h_svm = mean(h_accuracy$svm_acc)
  h_ksvm=mean(h_accuracy$ksvm_acc)
  
  h_avg = data.frame(knn = h_knn, rf = h_rf, nb = h_nb, svm = h_svm, ksvm = h_ksvm)
  return(h_avg)
}

h_final_model = sapply(B, model)
df_h_model = as.data.frame((h_final_model))
df_h_model = data.frame(t(h_final_model))

home_accuracies = data.frame(h_knn = mean(t(as.data.frame(df_h_model$knn))),
                             h_rf = mean(t(as.data.frame(df_h_model$rf))),
                             h_nb = mean(t(as.data.frame(df_h_model$nb))),
                             h_svm = mean(t(as.data.frame(df_h_model$svm))),
                             h_ksvm = mean(t(as.data.frame(df_h_model$ksvm))))
h_best_model <- which.max(home_accuracies)
h_best_model

#--------------------------------------------------------------------------------------------------------------------------

# Use the validation set fot the home win predict

# Create the validation dataset
h_validation_index <- createDataPartition(h_laliga_win$home_team_win, times = 1, p= 0.2, list= FALSE)
h_validation_set <- h_laliga_win[h_validation_index,]
h_working_set <- h_laliga_win[-h_validation_index, ]

h_working_x <- h_working_set[,5:12]
h_working_y <- as.factor(h_working_set[,4])
h_valid_x <- h_validation_set[,5:12]
h_valid_y <- as.factor(h_validation_set[,4])

h_fit_svm <- svm(home_team_win ~ home_w + away_w + h_goal + h_rel_goal + h_r_goal + a_goal + a_rel_goal + a_r_goal, data= h_validation_set, type = "C-classification")
h_y_hat_svm <- predict(h_fit_svm, h_valid_x)
mean(h_y_hat_svm == h_valid_y)



#------------------------------------------------------------------------------------------------------------------------
# Away team win predictor

#See the correlation of the variables of the away database
laliga_away_database %>% ggplot(aes(away_w, a_rel_goal)) + geom_point() + geom_smooth()
laliga_away_database %>% ggplot(aes(away_w, a_goal)) + geom_point() + geom_smooth()


#As the winning perc variables are very correlated to the goals scored and the relation goal (scored/received), let's create a dataset with these variables
laliga_home_win <- laliga_home_database %>% select(season, home_team_api_id, home_w, h_goal, h_rel_goal, h_r_goal)
laliga_away_win <- laliga_away_database %>% select(season, away_team_api_id, away_w, a_goal, a_rel_goal,a_r_goal)


# Create a dataset with the master of Laliga matches and the variables of home_winning
a_laliga_win <- laliga_match %>%
  select(season, home_team_api_id, away_team_api_id, away_team_win) %>%
  left_join(laliga_home_win) %>%
  left_join(laliga_away_win)

#initialize dataframes
a_model1 = data.frame()
a_model2 = data.frame()
a_model3 = data.frame()
a_model4 = data.frame()
a_model5 = data.frame()
a_total_avg = data.frame()

B = c(1:10)

a_model <- function(x){
  set.seed(x)
  #Create the validation dataset
  a_validation_index <- createDataPartition(a_laliga_win$away_team_win, times = 1, p= 0.2, list= FALSE)
  a_validation_set <- a_laliga_win[a_validation_index,]
  a_working_set <- a_laliga_win[-a_validation_index, ]
  
  #Create the training and testing datasets
  a_testing_index <- createDataPartition(a_working_set$away_team_win, times = 1, p = 0.2, list = FALSE)
  a_testing_set <- a_working_set[a_testing_index,]
  a_training_set <- a_working_set[-a_testing_index,]
  
  
  #First Model
  
  train_x <- a_training_set[,5:12]
  train_y <- as.factor(a_training_set[,4])
  test_x <- a_testing_set[,5:12]
  test_y <- as.factor(a_testing_set[,4])
  
  fit_knn <- train(train_x, train_y, method = "knn")
  y_hat_knn <- predict(fit_knn, test_x)
  a_testing_set1 <- a_testing_set %>% bind_cols(y_hat_knn = y_hat_knn)
  
  fit_rf <- train(train_x, train_y , method ="rf")
  y_hat_rf <- predict(fit_rf, test_x)
  a_testing_set1 <- a_testing_set1 %>% bind_cols(y_hat_rf = y_hat_rf)
  
  fit_nb <- train(train_x, train_y, method ="nb")
  y_hat_nb <- predict(fit_nb, test_x)
  a_testing_set1 <- a_testing_set1 %>% bind_cols(y_hat_nb = y_hat_nb)
  
  library(e1071)
  fit_svm <- svm(away_team_win ~ home_w + away_w + h_goal + h_rel_goal + h_r_goal + a_goal + a_rel_goal + a_r_goal, data= a_training_set, type = "C-classification")
  y_hat_svm <- predict(fit_svm, test_x)
  a_testing_set1 <- a_testing_set1 %>% bind_cols(y_hat_svm = y_hat_svm)
  
  fit_ksvm <- svm(away_team_win ~ home_w + away_w + h_goal + h_rel_goal + h_r_goal + a_goal + a_rel_goal + a_r_goal , data= a_training_set, type = "C-classification", kernel = "polynomial")
  y_hat_ksvm <- predict(fit_ksvm, test_x)
  a_testing_set1 <- a_testing_set1 %>% bind_cols(y_hat_ksvm = y_hat_ksvm)
  
  a_acc_model1 <- a_testing_set1  %>% summarize(knn_acc = mean(y_hat_knn == away_team_win),
                                            rf_acc = mean(y_hat_rf == away_team_win),
                                            nb_acc = mean(y_hat_nb == away_team_win),
                                            svm_acc = mean(y_hat_svm == away_team_win),
                                            ksvm_acc = mean(y_hat_ksvm == away_team_win))
  
  a_model1<- bind_rows(a_model1,a_acc_model1)
  
  a_accuracy = data.frame(a_model1)

  
  a_knn = mean(a_accuracy$knn_acc)
  a_rf = mean(a_accuracy$rf_acc)
  a_nb = mean(a_accuracy$nb_acc)
  a_svm = mean(a_accuracy$svm_acc)
  a_ksvm= mean(a_accuracy$ksvm_acc)
  
  a_avg = data.frame(knn = a_knn, rf = a_rf, nb = a_nb, svm = a_svm, ksvm = a_ksvm)

  return(a_avg)
}
a_final_model <- sapply(B,a_model)

df_a_model <- as.data.frame(a_final_model)
df_a_model <- data.frame(t(a_final_model))
away_accuracies = data.frame(a_knn = mean(t(as.data.frame(df_a_model$knn))),
                             a_rf = mean(t(as.data.frame(df_a_model$rf))),
                             a_nb = mean(t(as.data.frame(df_a_model$nb))),
                             a_svm = mean(t(as.data.frame(df_a_model$svm))),
                             a_ksvm = mean(t(as.data.frame(df_a_model$ksvm))))
a_best_model <- which.max(away_accuracies)
a_best_model

#--------------------------------------------------------------------------------------------------------------------------

# Use the validation set fot the away win predict
# Create the validation dataset
a_validation_index <- createDataPartition(a_laliga_win$away_team_win, times = 1, p= 0.2, list= FALSE)
a_validation_set <- a_laliga_win[a_validation_index,]
a_working_set <- a_laliga_win[-a_validation_index, ]

a_working_x <- a_working_set[,5:12]
a_working_y <- as.factor(a_working_set[,4])
a_valid_x <- a_validation_set[,5:12]
a_valid_y <- as.factor(a_validation_set[,4])

a_fit_nb <- train(a_working_x, a_working_y, method = "nb")
a_y_hat_nb <- predict(a_fit_nb, a_valid_x)
mean(a_y_hat_nb == a_valid_y)
