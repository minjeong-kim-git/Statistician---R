# 1. Filling in the blanks

# Impute the oceanbuoys data below the range using `impute_below`.
ocean_imp <- impute_below_all(oceanbuoys)

# Visualize the new missing values
ggplot(ocean_imp, 
       aes(x = wind_ew, y = air_temp_c)) +  
  geom_point()

# Impute and track data with `bind_shadow`, `impute_below_all`, and `add_label_shadow`
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>% 
  add_label_shadow()

# Look at the imputed values
ocean_imp_track

# Impute and track the missing values
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>% 
  add_label_shadow()

# Visualize the missingness in wind and air temperature,  
# coloring missing air temp values with air_temp_c_NA
ggplot(ocean_imp_track, 
       aes(x = wind_ew, y = air_temp_c, color = air_temp_c_NA)) + 
  geom_point()

# Visualize humidity and air temp, coloring any missing cases using the variable any_missing
ggplot(ocean_imp_track, 
       aes(x = humidity, y = air_temp_c, color = any_missing)) +  
  geom_point()

# Explore the values of air_temp_c, visualizing the amount of missings with `air_temp_c_NA`.
p <- ggplot(ocean_imp_track, aes(x = air_temp_c, fill = air_temp_c_NA)) +  geom_histogram()

# Expore the missings in humidity using humidity_NA
p2 <- ggplot(ocean_imp_track,  aes(x = humidity, fill = humidity_NA)) + geom_histogram()

# Explore the missings in air_temp_c according to year, using `facet_wrap(~year)`.
p + facet_wrap(~ year)

# Explore the missings in humidity according to year, using `facet_wrap(~year)`.
p2 + facet_wrap(~ year)

##############################################################################################################################

# 2. What makes a good imputation

# Impute the mean value and track the imputations 
ocean_imp_mean <- bind_shadow(oceanbuoys) %>% 
  impute_mean_all() %>% 
  add_label_shadow()

# Explore the mean values in humidity in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = humidity_NA, y = humidity)) + 
  geom_boxplot()

# Explore the values in air temperature in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c_NA, y = air_temp_c)) + 
  geom_boxplot()

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing, and faceting by year
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point() +  
  facet_wrap(~ year)

# Gather the imputed data 
ocean_imp_mean_gather <- shadow_long(ocean_imp_mean,
                                     humidity,
                                     air_temp_c)
# Inspect the data
ocean_imp_mean_gather

# Explore the imputations in a histogram 
ggplot(ocean_imp_mean_gather, 
       aes(x = value, fill = value_NA)) + 
  geom_histogram() + 
  facet_wrap(~variable)

##############################################################################################################################

# 3. Performing imputations

# Impute humidity and air temperature using wind_ew and wind_ns, and track missing values
ocean_imp_lm_wind <- oceanbuoys %>% 
  bind_shadow() %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns) %>% 
  impute_lm(humidity ~ wind_ew + wind_ns) %>%
  add_label_shadow()

# Plot the imputed values for air_temp_c and humidity, colored by missingness
ggplot(ocean_imp_lm_wind, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

# Bind the models together 
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          .id = "imp_model")

# Inspect the values of air_temp and humidity as a scatter plot
ggplot(bound_models, 
       aes(x = air_temp_c, 
           y = humidity, 
           color = any_missing)) +
  geom_point() + 
  facet_wrap(~ imp_model)

# Build a model adding year to the outcome
ocean_imp_lm_wind_year <- bind_shadow(oceanbuoys) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year) %>%
  add_label_shadow()

# Bind the mean, lm_wind, and lm_wind_year models together
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          lm_wind_year = ocean_imp_lm_wind_year,
                          .id = "imp_model")

# Explore air_temp and humidity, coloring by any missings, and faceting by imputation model
ggplot(bound_models, aes(x = air_temp_c, y = humidity, color = any_missing)) +
  geom_point() + facet_wrap(~imp_model)

##############################################################################################################################

# 4. Evaluating imputations and models

# Create an imputed dataset using a linear models
ocean_imp_lm_all <- bind_shadow(oceanbuoys) %>%
  add_label_shadow() %>%
  impute_lm(sea_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year + latitude + longitude)

# Bind the datasets
bound_models <- bind_rows(cc = ocean_cc,
                          imp_lm_wind = ocean_imp_lm_wind,
                          imp_lm_all = ocean_imp_lm_all,
                          .id = "imp_model")
# Look at the models
bound_models

# Create the model summary for each dataset
model_summary <- bound_models %>% 
  group_by(imp_model) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(sea_temp_c ~ air_temp_c + humidity + year, data = .)),
         res = map(mod, residuals),
         pred = map(mod, predict),
         tidy = map(mod, tidy))

# Explore the coefficients in the model
model_summary %>% 
  select(imp_model, tidy) %>% 
  unnest()

best_model <- "imp_lm_all"