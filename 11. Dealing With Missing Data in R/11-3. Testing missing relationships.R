# 1. Tools to explore missing data dependence

# Create shadow matrix data with `as_shadow()`
as_shadow(oceanbuoys)

# Create nabular data by binding the shadow to the data with `bind_shadow()`
bind_shadow(oceanbuoys)

# Bind only the variables with missing values by using bind_shadow(only_miss = TRUE)
bind_shadow(oceanbuoys, only_miss = TRUE)

# `bind_shadow()` and `group_by()` humidity missingness (`humidity_NA`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>% 
  summarize(wind_ew_mean = mean(wind_ew), # calculate mean of wind_ew
            wind_ew_sd = sd(wind_ew)) # calculate standard deviation of wind_ew

# Repeat this, but calculating summaries for wind north south (`wind_ns`).
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ns_mean = mean(wind_ns),
            wind_ns_sd = sd(wind_ns))

# Summarize wind_ew by the missingness of `air_temp_c_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Summarize wind_ew by missingness of `air_temp_c_NA` and `humidity_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA, humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

##############################################################################################################################

# 2. Visualizing missingness across one variable

# First explore the missingness structure of `oceanbuoys` using `vis_miss()`
vis_miss(oceanbuoys)

# Explore the distribution of `wind_ew` for the missingness  
# of `air_temp_c_NA` using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew, 
             color = air_temp_c_NA)) + 
  geom_density()

# Explore the distribution of sea temperature for the  
# missingness of humidity (humidity_NA) using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = sea_temp_c,
             color = humidity_NA)) + 
  geom_density()

# Explore the distribution of wind east west (wind_ew) for the missingness of air temperature 
# using geom_density() and faceting by the missingness of air temperature (air_temp_c_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew)) + 
  geom_density() + 
  facet_wrap(~ air_temp_c_NA)

# Build upon this visualization by coloring by the missingness of humidity (humidity_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew,
             color = humidity_NA)) + 
  geom_density() + 
  facet_wrap(~ air_temp_c_NA)

# Explore the distribution of wind east west (`wind_ew`) for  
# the missingness of air temperature using  `geom_boxplot()`
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot()

# Build upon this visualization by faceting by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot() + 
  facet_wrap(~ humidity_NA)

##############################################################################################################################

# 3. Visualizing missingness across two variables

# Explore the missingness in wind and air temperature, and  
# display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in humidity and air temperature,  
# and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in wind and air temperature, and display the 
# missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Explore the missingness in humidity and air temperature, and display the 
# missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Use geom_miss_point() and facet_wrap to explore how the missingness  
# in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~ humidity_NA)

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c 
# is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)