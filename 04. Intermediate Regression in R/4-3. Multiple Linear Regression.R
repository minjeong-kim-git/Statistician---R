# 1. 3D visualizations

# With taiwan_real_estate, draw a 3D scatter plot of
# no. of conv. stores, sqrt dist to MRT, and price
taiwan_real_estate %$% scatter3D(n_convenience,sqrt(dist_to_mrt_m), price_twd_msq)

# Using taiwan_real_estate, plot sqrt dist to MRT vs. 
# no. of conv stores, colored by price
ggplot(taiwan_real_estate, aes(n_convenience, sqrt(dist_to_mrt_m), color=price_twd_msq)) + 
  # Make it a scatter plot
  geom_point() +
  # Use the continuous viridis plasma color scale
  scale_color_viridis_c(option="plasma")

########################################################################

# 2. Modeling 2 numeric explanatory variables

# Fit a linear regression of price vs. no. of conv. stores
# and sqrt dist. to nearest MRT, no interaction
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m),data=taiwan_real_estate)

# See the result
mdl_price_vs_conv_dist

# From previous step 
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# Create expanded grid of explanatory variables with
# no. of conv. stores and  dist. to nearest MRT
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Add predictions using mdl_price_vs_conv_dist and explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data)
  )

# See the result
prediction_data

# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma")+
  # Add prediction points colored yellow, size 3
  geom_point(
    data=prediction_data,
    color="yellow",
    size=3
  )

########################################################################

# 3. Including an interaction

taiwan_real_estate

# Fit a linear regression of price vs. no. of conv. stores
# and sqrt dist. to nearest MRT, with interaction
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m) + n_convenience : sqrt(dist_to_mrt_m), data=taiwan_real_estate)

# See the result
mdl_price_vs_conv_dist

# From previous step 
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# Create expanded grid of explanatory variables with
# no. of conv. stores and  dist. to nearest MRT
explanatory_data <- expand_grid(
  n_convenience = seq(0, 10, 1),
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Add predictions using mdl_price_vs_conv_dist and explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data)
  )

# See the result
prediction_data

# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  # Add prediction points colored yellow, size 3
  geom_point(
    data=prediction_data,
    color="yellow",
    size=3
  )


########################################################################

# 4. Visualizing many variables

# Using taiwan_real_estate, no. of conv. stores vs. sqrt of
# dist. to MRT, colored by plot house price
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), n_convenience, color=price_twd_msq)) +
  # Make it a scatter plot
  geom_point() +
  # Use the continuous viridis plasma color scale
  scale_color_viridis_c(option="plasma") +
  # Facet, wrapped by house age
  facet_wrap(vars(house_age_years))

########################################################################

# 5. Different levels of interaction

taiwan_real_estate

# Model price vs. no. of conv. stores, sqrt dist. to MRT 
# station & house age, no global intercept, no interactions
mdl_price_vs_all_no_inter <- lm(formula = price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m) + house_age_years + 0, data=taiwan_real_estate)

# See the result
mdl_price_vs_all_no_inter
# Model price vs. sqrt dist. to MRT station, no. of conv.
# stores & house age, no global intercept, 3-way interactions
mdl_price_vs_all_3_way_inter <- lm(
  price_twd_msq ~ sqrt(dist_to_mrt_m) * n_convenience * house_age_years + 0, 
  data = taiwan_real_estate
)

# See the result
mdl_price_vs_all_3_way_inter

########################################################################

# 6. Predicting again

# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set dist_to_mrt_m a seq from 0 to 80 by 10s, squared
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  # Set n_convenience to 0 to 10
  n_convenience = seq(0, 10, 1),
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data

taiwan_real_estate

# From previous step
explanatory_data <- expand_grid(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# Add predictions to the data frame
prediction_data <- explanatory_data %>%
  mutate(
    price_twd_msq = predict(mdl_price_vs_all_3_way_inter, explanatory_data)
    
  )

# From previous step
explanatory_data <- expand_grid(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_all_3_way_inter, explanatory_data))

# Extend the plot
ggplot(
  taiwan_real_estate, 
  aes(sqrt(dist_to_mrt_m), n_convenience, color = price_twd_msq)
) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(vars(house_age_years)) +
  # Add points from prediction data, size 3, shape 15
  geom_point(
    data=prediction_data,
    size=3,
    shape=15
  )

########################################################################

# 8. Linear regression algorithm

# Set the intercept to 10
intercept <- 10

# Set the slope to 1
slope <- 1

# Calculate the predicted y values
y_pred <- intercept + slope * x_actual

# Calculate the differences between actual and predicted
y_diff <- y_actual - y_pred

# Calculate the sum of squares
sum(y_diff ^ 2)

calc_sum_of_squares <- function(coeffs) {
  # Get the intercept coeff
  intercept <- coeffs[1]
  
  # Get the slope coeff
  slope <- coeffs[2]
  
  # Calculate the predicted y values
  y_pred <- intercept + slope * x_actual
  
  # Calculate the differences between actual and predicted
  y_diff <- y_actual - y_pred
  
  # Calculate the sum of squares
  sum(y_diff ^ 2)
}

# From previous step
calc_sum_of_squares <- function(coeffs) {
  intercept <- coeffs[1]
  slope <- coeffs[2]
  y_pred <- intercept + slope * x_actual
  y_diff <- y_actual - y_pred
  sum(y_diff ^ 2)
}

# Optimize the metric
optim(
  # Initially guess 0 intercept and 0 slope
  par = c(intercept = 0, slope=0), 
  # Use calc_sum_of_squares as the optimization fn
  fn = calc_sum_of_squares
)

# Compare the coefficients to those calculated by lm()
lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)
