# 1. Models for each category

# Filter for rows where house age is 0 to 15 years
taiwan_0_to_15 <- taiwan_real_estate %>%
  filter(house_age_years == "0 to 15")


# Filter for rows where house age is 15 to 30 years
taiwan_15_to_30 <- taiwan_real_estate %>%
  filter(house_age_years == "15 to 30")


# Filter for rows where house age is 30 to 45 years
taiwan_30_to_45 <- taiwan_real_estate %>%
  filter(house_age_years == "30 to 45")

# Model price vs. no. convenience stores using 0 to 15 data
mdl_0_to_15 <- lm(price_twd_msq~n_convenience, data=taiwan_0_to_15)

# Model price vs. no. convenience stores using 15 to 30 data
mdl_15_to_30 <- lm(price_twd_msq~n_convenience, data=taiwan_15_to_30)

# Model price vs. no. convenience stores using 30 to 45 data
mdl_30_to_45 <- lm(price_twd_msq~n_convenience, data=taiwan_30_to_45)

# See the results
mdl_0_to_15
mdl_15_to_30
mdl_30_to_45

# Create a tibble of explanatory data, setting
# no. of conv stores to 0 to 10
explanatory_data <- tibble(
  explanatory_data_0_to_15 = seq(0, 10, 1)
)

# Add column of predictions using "0 to 15" model and explanatory data 
prediction_data_0_to_15 <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_0_to_15, explanatory_data)
  )

# Same again, with "15 to 30"
prediction_data_15_to_30 <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_15_to_30, explanatory_data)
  )

# Same again, with "30 to 45"
prediction_data_30_to_45 <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_30_to_45, explanatory_data)
  )

# Using taiwan_real_estate, plot price vs. no. of conv. stores
# colored by house age
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color=house_age_years)) +
  # Make it a scatter plot
  geom_point() +
  # Add smooth linear regression trend lines, no ribbon
  geom_smooth(method="lm", se=FALSE)

# Extend the plot to include prediction points
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color = house_age_years)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points using prediction_data_0_to_15, colored red, size 3, shape 15
  geom_point(data = prediction_data_0_to_15, color = "red", size = 3, shape = 15) +
  # Add points using prediction_data_15_to_30, colored green, size 3, shape 15
  geom_point(data = prediction_data_15_to_30, color = "green", size = 3, shape = 15) +
  # Add points using prediction_data_30_to_45, colored blue, size 3, shape 15
  geom_point(data = prediction_data_30_to_45, color = "blue", size = 3, shape = 15)

# Get the coeff. of determination for mdl_all_ages
mdl_all_ages %>% glance() %>% pull(r.squared)

# Get the coeff. of determination for mdl_0_to_15
mdl_0_to_15 %>% glance() %>% pull(r.squared)

# Get the coeff. of determination for mdl_15_to_30
mdl_15_to_30 %>% glance() %>% pull(r.squared)

# Get the coeff. of determination for mdl_30_to_45
mdl_30_to_45 %>% glance() %>% pull(r.squared)

# Get the RSE for mdl_all
mdl_all_ages %>% glance() %>% pull(sigma)

# Get the RSE for mdl_0_to_15
mdl_0_to_15 %>% glance() %>% pull(sigma)

# Get the RSE for mdl_15_to_30
mdl_15_to_30 %>% glance() %>% pull(sigma)

# Get the RSE for mdl_30_to_45
mdl_30_to_45 %>% glance() %>% pull(sigma)

###############################################################

# 2. One model with an interaction

# Model price vs both with an interaction using "times" syntax
lm(price_twd_msq ~ n_convenience * house_age_years,
   data = taiwan_real_estate)

# Model price vs both with an interaction using "colon" syntax
lm(price_twd_msq ~ n_convenience + house_age_years + n_convenience : house_age_years,
   data=taiwan_real_estate)

# Model price vs. house age plus an interaction, no intercept
mdl_readable_inter <- lm(price_twd_msq ~ house_age_years + n_convenience:house_age_years + 0,
                         data = taiwan_real_estate)

# See the result
mdl_readable_inter

# Get coefficients for mdl_0_to_15
coefficients(mdl_0_to_15)

# Get coefficients for mdl_15_to_30
coefficients(mdl_15_to_30)

# Get coefficients for mdl_30_to_45
coefficients(mdl_30_to_45)

###############################################################

# 3. Making predictions with interactions

# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set n_convenience to zero to ten
  n_convenience = seq(0, 10, 1),
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data

# Add predictions to the data frame
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_both_inter, explanatory_data)
  )

# See the result
prediction_data

# Using taiwan_real_estate, plot price vs. no. of convenience 
# stores, colored by house age
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color = house_age_years)) +
  # Make it a scatter plot
  geom_point() +
  # Add linear regression trend lines, no ribbon
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, size 5, shape 15
  geom_point(data = prediction_data, size = 5, shape = 15)

# Get the coefficients from mdl_price_vs_both_inter
coeffs <- coefficients(mdl_price_vs_both_inter)

# Get the intercept for 0 to 15 year age group
intercept_0_15 <- coeffs[1]

# Get the intercept for 15 to 30 year age group
intercept_15_30 <- coeffs[2]

# Get the intercept for 30 to 45 year age group
intercept_30_45 <- coeffs[3]

# Get the slope for 0 to 15 year age group
slope_0_15 <- coeffs[4]

# Get the slope for 15 to 30 year age group
slope_15_30 <- coeffs[5]

# Get the slope for 30 to 45 year age group
slope_30_45 <- coeffs[6]

prediction_data <- explanatory_data %>% 
  mutate(
    # Consider the 3 cases to choose the price
    price_twd_msq = case_when(
      house_age_years == "0 to 15" ~ intercept_0_15 + slope_0_15 * n_convenience,
      house_age_years == "15 to 30" ~ intercept_15_30 + slope_15_30 * n_convenience,
      house_age_years == "30 to 45" ~ intercept_30_45 + slope_30_45 * n_convenience 
    ) 
  )

# See the result
prediction_data

###############################################################

# 4. Simpson's Paradox

# Take a glimpse at the dataset
glimpse(auctions)

# Model price vs. opening bid using auctions
mdl_price_vs_openbid <- lm(price ~ openbid, data=auctions)

# See the result
mdl_price_vs_openbid

# Using auctions, plot price vs. opening bid as a 
# scatter plot with linear regression trend lines
ggplot(auctions, aes(openbid, price)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Fit linear regression of price vs. opening bid and auction 
# type, with an interaction.
mdl_price_vs_both <- lm(formula = price ~ openbid + auction_type + openbid: auction_type, data=auctions)

# See the result
mdl_price_vs_both

# Using auctions, plot price vs. opening bid colored by
# auction type as a scatter plot with linear regr'n trend lines
ggplot(auctions, aes(openbid, price, color=auction_type)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)