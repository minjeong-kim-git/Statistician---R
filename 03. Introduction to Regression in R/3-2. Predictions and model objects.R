# 1. Making predictions

# Create a tibble with n_convenience column from zero to ten
explanatory_data <- tibble(n_convenience = 0:10)

# Use mdl_price_vs_conv to predict with explanatory_data
predict(mdl_price_vs_conv, explanatory_data)

# Edit this, so predictions are stored in prediction_data
prediction_data <- explanatory_data %>%
  mutate(
    price_twd_msq = predict(
      mdl_price_vs_conv, explanatory_data
    )
  )

# See the result
prediction_data

# Add to the plot
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
  geom_point(
    data = prediction_data,
    color = 'yellow'
  )

# Define a tibble where n_convenience is -1
minus_one <- tibble(n_convenience = -1)

# Define a tibble where n_convenience is 2.5
two_pt_five <- tibble(n_convenience = 2.5)

###############################################################

# 2. Working with model objects

# Get the model coefficients of mdl_price_vs_conv
coefficients(mdl_price_vs_conv)

# Get the fitted values of mdl_price_vs_conv
fitted(mdl_price_vs_conv)

# Get the residuals of mdl_price_vs_conv
residuals(mdl_price_vs_conv)

# Print a summary of mdl_price_vs_conv
summary(mdl_price_vs_conv)

# Get the coefficients of mdl_price_vs_conv
coeffs <- coefficients(mdl_price_vs_conv)

# Get the intercept
intercept <- coeffs[1]

# Get the slope
slope <- coeffs[2]

explanatory_data %>% 
  mutate(
    # Manually calculate the predictions
    price_twd_msq = intercept + slope * n_convenience
  )

# Compare to the results from predict()
predict(mdl_price_vs_conv, explanatory_data)

# Get the coefficient-level elements of the model
library(broom)
tidy(mdl_price_vs_conv)

# Get the observation-level elements of the model
augment(mdl_price_vs_conv)

# Get the model-level elements of the model
glance(mdl_price_vs_conv)

###############################################################

# 3. Regression to the mean

# Using sp500_yearly_returns, plot return_2019 vs. return_2018
ggplot(sp500_yearly_returns, aes(return_2018, return_2019)) +
  # Make it a scatter plot
  geom_point() +
  # Add a line at y = x, colored green, size 1
  geom_abline(color = 'green', size = 1) +
  # Add a linear regression trend line, no std. error ribbon
  geom_smooth(method = 'lm', se = FALSE) +
  # Fix the coordinate ratio
  coord_fixed()

# Run a linear regression on return_2019 vs. return_2018
# using sp500_yearly_returns
mdl_returns <- lm(return_2019 ~ return_2018,
                  data = sp500_yearly_returns)

# See the result
mdl_returns

# Create a data frame with return_2018 at -1, 0, and 1 
explanatory_data <- tibble(
  return_2018 = c(-1, 0, 1)
)

# Use mdl_returns to predict with explanatory_data
predict(mdl_returns, explanatory_data)

###############################################################

# 4. Transforming variables

# Run the code to see the plot
# Edit so x-axis is square root of dist_to_mrt_m
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Run a linear regression of price_twd_msq vs. 
# square root of dist_to_mrt_m using taiwan_real_estate
mdl_price_vs_dist <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m),
                        data = taiwan_real_estate)

# See the result
mdl_price_vs_dist

# Use this explanatory data
explanatory_data <- tibble(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Use mdl_price_vs_dist to predict explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_dist, explanatory_data)
  )

# See the result
prediction_data

# From previous steps
mdl_price_vs_dist <- lm(
  price_twd_msq ~ sqrt(dist_to_mrt_m), 
  data = taiwan_real_estate
)
explanatory_data <- tibble(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_dist, explanatory_data)
  )

ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green, size 5
  geom_point(color = 'green', size = 5, data = prediction_data)

# Run the code to see the plot
# Edit to raise x, y aesthetics to power 0.25
ggplot(ad_conversion, aes(n_impressions ^ 0.25, n_clicks ^ 0.25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Run a linear regression of n_clicks to the power 0.25 vs. 
# n_impressions to the power 0.25 using ad_conversion
mdl_click_vs_impression <- lm(
  I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25), 
  data = ad_conversion
)

# Use this explanatory data
explanatory_data <- tibble(
  n_impressions = seq(0, 3e6, 5e5)
)

prediction_data <- explanatory_data %>% 
  mutate(
    # Use mdl_click_vs_impression to predict n_clicks ^ 0.25
    n_clicks_025 = predict(
      mdl_click_vs_impression, explanatory_data
    ),
    # Back transform to get n_clicks
    n_clicks = n_clicks_025 ^ 4
  )

# From previous steps
mdl_click_vs_impression <- lm(
  I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25),
  data = ad_conversion
)
explanatory_data <- tibble(
  n_impressions = seq(0, 3e6, 5e5)
)
prediction_data <- explanatory_data %>% 
  mutate(
    n_clicks_025 = predict(mdl_click_vs_impression, explanatory_data),
    n_clicks = n_clicks_025 ^ 4
  )

ggplot(ad_conversion, aes(n_impressions ^ 0.25, n_clicks ^ 0.25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green
  geom_point(color = 'green', data = prediction_data)