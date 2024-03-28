library(tidyverse)
library(caret)
library(data.table)
library(baseballr)


set.seed(123)
tune_grid <- expand.grid(
  nrounds = c(250, 300),       
  max_depth = c(6,9),           
  eta = c(0.1,0.3),                     
  gamma = c(0.2,0.4),               
  colsample_bytree = 1,
  min_child_weight = c(1,5),
  subsample = 1
)

ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE)

fastball_model_data <- train_model_data %>%
  filter(pitch_type == "FF") %>%
  select(-c(pitch_type, release_speed_diff, release_pos_x_diff, 
            release_pos_z_diff, pfx_x_diff, pfx_z_diff, vx0_diff, 
            vy0_diff, vz0_diff, ax_diff, ay_diff, az_diff, 
            release_spin_rate_diff, release_extension_diff, spin_axis_diff
 ))

stuff_fastball_model <- train(
  delta_run_exp ~ .,                    
  data = fastball_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_fastball_model, "FF_Model.rds")

changeup_model_data <- train_model_data %>%
  filter(pitch_type == "CH") %>%
  select(-pitch_type)

stuff_changeup_model <- train(
  delta_run_exp ~ .,                    
  data = changeup_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_changeup_model, "CH_Model.rds")

slider_model_data <- train_model_data %>%
  filter(pitch_type == "SL") %>%
  select(-pitch_type)

stuff_slider_model <- train(
  delta_run_exp ~ .,                    
  data = slider_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_slider_model, "SL_Model.rds")

sinker_model_data <- train_model_data %>%
  filter(pitch_type == "SI") %>%
  select(-c(pitch_type, release_speed_diff, release_pos_x_diff, 
            release_pos_z_diff, pfx_x_diff, pfx_z_diff, vx0_diff, 
            vy0_diff, vz0_diff, ax_diff, ay_diff, az_diff, 
            release_spin_rate_diff, release_extension_diff, spin_axis_diff
  ))

stuff_sinker_model <- train(
  delta_run_exp ~ .,                    
  data = sinker_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_sinker_model, "SI_Model.rds")

splitter_model_data <- train_model_data %>%
  filter(pitch_type == "FS") %>%
  select(-pitch_type)

stuff_splitter_model <- train(
  delta_run_exp ~ .,                    
  data = splitter_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_splitter_model, "FS_Model.rds")

curveball_model_data <- train_model_data %>%
  filter(pitch_type == "CU") %>%
  select(-pitch_type)

stuff_curveball_model <- train(
  delta_run_exp ~ .,                    
  data = curveball_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_curveball_model, "CU_Model.rds")

cutter_model_data <- train_model_data %>%
  filter(pitch_type == "FC") %>%
  select(-c(pitch_type, release_speed_diff, release_pos_x_diff, 
            release_pos_z_diff, pfx_x_diff, pfx_z_diff, vx0_diff, 
            vy0_diff, vz0_diff, ax_diff, ay_diff, az_diff, 
            release_spin_rate_diff, release_extension_diff, spin_axis_diff
  ))

stuff_cutter_model <- train(
  delta_run_exp ~ .,                    
  data = cutter_model_data,
  method = "xgbTree",             
  trControl = ctrl,            
  tuneGrid = tune_grid,    
  verbose = TRUE,
  metric = "RMSE",
  maximize = TRUE
)

write_rds(stuff_cutter_model, "FC_Model.rds")

ff_22 <- SC_data2 %>% filter(pitch_type == "FF" & game_year == 2022) %>% na.omit()
test <- data.frame(test = predict(stuff_fastball_model, ff_22))

scaled_data <- test %>%
  mutate(scaled_test = 100 - (scale(.[['test']]) * 100))

with_stuff <- cbind(ff_22, scaled_data)

ids <- chadwick_player_lu()

ids <- ids %>%
  mutate(name = paste(name_first, name_last)) %>%
  select(key_mlbam, name)

with_names <- merge(with_stuff, ids, by.x = "pitcher", by.y = "key_mlbam")

with_names %>% 
  group_by(pitcher) %>%
  mutate(stuff_plus = mean(scaled_test)) %>%
  ungroup() %>%
  select(name, stuff_plus) %>%
  group_by(name) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  unique() -> fb_22_stf

data_2023 <- SC_data2 %>% filter(game_year == 2023)

set.seed(123)
stuff_fastball_model <- readRDS("FF_Model.rds")
stuff_changeup_model <- readRDS("CH_Model.rds")
stuff_cutter_model <- readRDS("FC_Model.rds")
stuff_splitter_model <- readRDS("FS_Model.rds")
stuff_slider_model <- readRDS("SL_Model.rds")
stuff_curveball_model <- readRDS("CU_Model.rds")
stuff_sinker_model <- readRDS("SI_Model.rds")

fastball_2023 <- data_2023 %>%
  filter(pitch_type == "FF") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_fastball_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

changeup_2023 <- data_2023 %>%
  filter(pitch_type == "CH") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_changeup_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

cutter_2023 <- data_2023 %>%
  filter(pitch_type == "FC") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_cutter_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

curveball_2023 <- data_2023 %>%
  filter(pitch_type == "CU") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_curveball_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

slider_2023 <- data_2023 %>%
  filter(pitch_type == "SL") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_slider_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

sinker_2023 <- data_2023 %>%
  filter(pitch_type == "SI") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_sinker_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

splitter_2023 <- data_2023 %>%
  filter(pitch_type == "FS") %>%
  na.omit() %>%
  cbind(data.frame(test = predict(stuff_splitter_model, .))) %>%
  mutate(scaled_test = 100 - (scale(test) * 100)) %>%
  left_join(ids, by = c("pitcher" = "key_mlbam")) %>%
  group_by(pitcher, name) %>%
  summarise(stuff_plus = mean(scaled_test), 
            num_pitches = n()) %>%
  ungroup() %>%
  select(name, stuff_plus, num_pitches) %>%
  distinct()

at_300 <- changeup_2023 %>% filter(num_pitches >= 400)

hist(at_300$stuff_plus)
