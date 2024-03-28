SC_data <- read_csv("SConly.csv") %>%
  filter(pitch_type %in% c("FF", "FA", "CH", "CU", "SI", "SL", "FS", "FC", "ST", "SV", "KC", "FT", "FO", "SC"))

SC_data1 <- SC_data %>%
  filter(pitch_type %in% c("FF", "FA", "CH", "CU", "SI", "SL", "FS", "FC", "ST", "SV", "KC", "FT", "FO", "SC")) %>%
  mutate(pitch_type = ifelse(pitch_type == "FA", "FF", 
                             ifelse(pitch_type == "KC", "CU",
                                    ifelse(pitch_type == "FO", "FS",
                                           ifelse(pitch_type == "ST", "SL",
                                                  ifelse(pitch_type == "SV", "CU",
                                                         ifelse(pitch_type == "FT", "SI",
                                                                ifelse(pitch_type == "SC", "CH", pitch_type))))))))

SC_data2 <- SC_data1 %>%
  select(game_pk, pitcher, game_year, at_bat_number, p_throws, pitch_type, release_speed, 
         release_pos_x, release_pos_z, pfx_x, pfx_z, vx0, vy0, 
         vz0, ax, ay, az, release_spin_rate, release_extension,
         spin_axis, delta_run_exp) %>%
  filter(game_year %in% c(2022, 2023))

# Filter and summarize fastball average values
fastball_avg_values <- SC_data1 %>%
  group_by(pitcher, game_year) %>%
  summarize(
    avg_release_speed = ifelse(any(pitch_type == "FF"), mean(release_speed[pitch_type == "FF"], na.rm = TRUE),
                               ifelse(any(pitch_type == "SI"), mean(release_speed[pitch_type == "SI"], na.rm = TRUE),
                                      mean(release_speed[pitch_type == "FC"], na.rm = TRUE))),
    avg_release_pos_x = ifelse(any(pitch_type == "FF"), mean(release_pos_x[pitch_type == "FF"], na.rm = TRUE),
                               ifelse(any(pitch_type == "SI"), mean(release_pos_x[pitch_type == "SI"], na.rm = TRUE),
                                      mean(release_pos_x[pitch_type == "FC"], na.rm = TRUE))),
    avg_release_pos_z = ifelse(any(pitch_type == "FF"), mean(release_pos_z[pitch_type == "FF"], na.rm = TRUE),
                               ifelse(any(pitch_type == "SI"), mean(release_pos_z[pitch_type == "SI"], na.rm = TRUE),
                                      mean(release_pos_z[pitch_type == "FC"], na.rm = TRUE))),
    avg_pfx_x = ifelse(any(pitch_type == "FF"), mean(pfx_x[pitch_type == "FF"], na.rm = TRUE),
                       ifelse(any(pitch_type == "SI"), mean(pfx_x[pitch_type == "SI"], na.rm = TRUE),
                              mean(pfx_x[pitch_type == "FC"], na.rm = TRUE))),
    avg_pfx_z = ifelse(any(pitch_type == "FF"), mean(pfx_z[pitch_type == "FF"], na.rm = TRUE),
                       ifelse(any(pitch_type == "SI"), mean(pfx_z[pitch_type == "SI"], na.rm = TRUE),
                              mean(pfx_z[pitch_type == "FC"], na.rm = TRUE))),
    avg_vx0 = ifelse(any(pitch_type == "FF"), mean(vx0[pitch_type == "FF"], na.rm = TRUE),
                     ifelse(any(pitch_type == "SI"), mean(vx0[pitch_type == "SI"], na.rm = TRUE),
                            mean(vx0[pitch_type == "FC"], na.rm = TRUE))),
    avg_vy0 = ifelse(any(pitch_type == "FF"), mean(vy0[pitch_type == "FF"], na.rm = TRUE),
                     ifelse(any(pitch_type == "SI"), mean(vy0[pitch_type == "SI"], na.rm = TRUE),
                            mean(vy0[pitch_type == "FC"], na.rm = TRUE))),
    avg_vz0 = ifelse(any(pitch_type == "FF"), mean(vz0[pitch_type == "FF"], na.rm = TRUE),
                     ifelse(any(pitch_type == "SI"), mean(vz0[pitch_type == "SI"], na.rm = TRUE),
                            mean(vz0[pitch_type == "FC"], na.rm = TRUE))),
    avg_ax = ifelse(any(pitch_type == "FF"), mean(ax[pitch_type == "FF"], na.rm = TRUE),
                    ifelse(any(pitch_type == "SI"), mean(ax[pitch_type == "SI"], na.rm = TRUE),
                           mean(ax[pitch_type == "FC"], na.rm = TRUE))),
    avg_ay = ifelse(any(pitch_type == "FF"), mean(ay[pitch_type == "FF"], na.rm = TRUE),
                    ifelse(any(pitch_type == "SI"), mean(ay[pitch_type == "SI"], na.rm = TRUE),
                           mean(ay[pitch_type == "FC"], na.rm = TRUE))),
    avg_az = ifelse(any(pitch_type == "FF"), mean(az[pitch_type == "FF"], na.rm = TRUE),
                    ifelse(any(pitch_type == "SI"), mean(az[pitch_type == "SI"], na.rm = TRUE),
                           mean(az[pitch_type == "FC"], na.rm = TRUE))),
    avg_release_spin_rate = ifelse(any(pitch_type == "FF"), mean(release_spin_rate[pitch_type == "FF"], na.rm = TRUE),
                                   ifelse(any(pitch_type == "SI"), mean(release_spin_rate[pitch_type == "SI"], na.rm = TRUE),
                                          mean(release_spin_rate[pitch_type == "FC"], na.rm = TRUE))),
    avg_release_extension = ifelse(any(pitch_type == "FF"), mean(release_extension[pitch_type == "FF"], na.rm = TRUE),
                                   ifelse(any(pitch_type == "SI"), mean(release_extension[pitch_type == "SI"], na.rm = TRUE),
                                          mean(release_extension[pitch_type == "FC"], na.rm = TRUE))),
    avg_spin_axis = ifelse(any(pitch_type == "FF"), mean(spin_axis[pitch_type == "FF"], na.rm = TRUE),
                           ifelse(any(pitch_type == "SI"), mean(spin_axis[pitch_type == "SI"], na.rm = TRUE),
                                  mean(spin_axis[pitch_type == "FC"], na.rm = TRUE)))
  )

# Select and filter data for recent years, join with fastball average values, calculate differences
SC_data2 <- SC_data1 %>%
  select(game_pk, pitcher, game_year, at_bat_number, p_throws, pitch_type, release_speed, 
         release_pos_x, release_pos_z, pfx_x, pfx_z, vx0, vy0, 
         vz0, ax, ay, az, release_spin_rate, release_extension,
         spin_axis, delta_run_exp) %>%
  filter(game_year %in% c(2022, 2023)) %>%
  left_join(fastball_avg_values, by = c("pitcher", "game_year")) %>%
  mutate(
    release_speed_diff = ifelse(pitch_type == "FF", release_speed - avg_release_speed,
                                ifelse(pitch_type == "SI", release_speed - avg_release_speed,
                                       release_speed - avg_release_speed)),
    release_pos_x_diff = ifelse(pitch_type == "FF", release_pos_x - avg_release_pos_x,
                                ifelse(pitch_type == "SI", release_pos_x - avg_release_pos_x,
                                       release_pos_x - avg_release_pos_x)),
    release_pos_z_diff = ifelse(pitch_type == "FF", release_pos_z - avg_release_pos_z,
                                ifelse(pitch_type == "SI", release_pos_z - avg_release_pos_z,
                                       release_pos_z - avg_release_pos_z)),
    pfx_x_diff = ifelse(pitch_type == "FF", pfx_x - avg_pfx_x,
                        ifelse(pitch_type == "SI", pfx_x - avg_pfx_x,
                               pfx_x - avg_pfx_x)),
    pfx_z_diff = ifelse(pitch_type == "FF", pfx_z - avg_pfx_z,
                        ifelse(pitch_type == "SI", pfx_z - avg_pfx_z,
                               pfx_z - avg_pfx_z)),
    vx0_diff = ifelse(pitch_type == "FF", vx0 - avg_vx0,
                      ifelse(pitch_type == "SI", vx0 - avg_vx0,
                             vx0 - avg_vx0)),
    vy0_diff = ifelse(pitch_type == "FF", vy0 - avg_vy0,
                      ifelse(pitch_type == "SI", vy0 - avg_vy0,
                             vy0 - avg_vy0)),
    vz0_diff = ifelse(pitch_type == "FF", vz0 - avg_vz0,
                      ifelse(pitch_type == "SI", vz0 - avg_vz0,
                             vz0 - avg_vz0)),
    ax_diff = ifelse(pitch_type == "FF", ax - avg_ax,
                     ifelse(pitch_type == "SI", ax - avg_ax,
                            ax - avg_ax)),
    ay_diff = ifelse(pitch_type == "FF", ay - avg_ay,
                     ifelse(pitch_type == "SI", ay - avg_ay,
                            ay - avg_ay)),
    az_diff = ifelse(pitch_type == "FF", az - avg_az,
                     ifelse(pitch_type == "SI", az - avg_az,
                            az - avg_az)),
    release_spin_rate_diff = ifelse(pitch_type == "FF", release_spin_rate - avg_release_spin_rate,
                                    ifelse(pitch_type == "SI", release_spin_rate - avg_release_spin_rate,
                                           release_spin_rate - avg_release_spin_rate)),
    release_extension_diff = ifelse(pitch_type == "FF", release_extension - avg_release_extension,
                                    ifelse(pitch_type == "SI", release_extension - avg_release_extension,
                                           release_extension - avg_release_extension)),
    spin_axis_diff = ifelse(pitch_type == "FF", spin_axis - avg_spin_axis,
                            ifelse(pitch_type == "SI", spin_axis - avg_spin_axis,
                                   spin_axis - avg_spin_axis))
  ) %>%
  select(-c(avg_release_speed, avg_release_pos_x, avg_release_pos_z,
            avg_pfx_x, avg_pfx_z, avg_vx0, avg_vy0, avg_vz0, avg_ax,
            avg_ay, avg_az, avg_release_spin_rate, avg_release_extension,
            avg_spin_axis))
                             

train_model_data <- SC_data2 %>% na.omit() %>%
  filter(game_year == 2022) %>%
  select(-c(game_pk, pitcher, game_year, at_bat_number)) %>%
  mutate(p_throws = as.factor(p_throws))
  


