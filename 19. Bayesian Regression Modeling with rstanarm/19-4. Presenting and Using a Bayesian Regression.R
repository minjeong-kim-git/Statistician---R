# 1. Visualizing a Bayesian model

# Save the model parameters
tidy_coef <- tidy(stan_model)

# Extract intercept and slope
model_intercept <- tidy_coef$estimate[1]
model_slope <- tidy_coef$estimate[2]

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point() +
  geom_abline(intercept = model_intercept, slope = model_slope)

# Save the values from each draw of the posterior distribution
draws <- spread_draws(stan_model, `(Intercept)`, `song_age`)

# Print the `draws` data frame to the console
draws

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point()

# Save the values from each draw of the posterior distribution
draws <- spread_draws(stan_model, `(Intercept)`, `song_age`)

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point() +
  geom_abline(data = draws, aes(intercept = `(Intercept)`, slope = song_age),
              size = 0.1, alpha = 0.2, color = "skyblue")

# Save the values from each draw of the posterior distribution
draws <- spread_draws(stan_model, `(Intercept)`, `song_age`)

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point() +
  geom_abline(data = draws, aes(intercept = `(Intercept)`, slope = song_age),
              size = 0.1, alpha = 0.2, color = "skyblue") +
  geom_abline(intercept = model_intercept, slope = model_slope)

##############################################################################################################################

# 2. Making predictions

# Estimate the regression model
stan_model <- stan_glm(popularity ~ song_age + artist_name, data = songs)

# Print the model summary
summary(stan_model)

# Get posteriors of predicted scores for each observation
posteriors <- posterior_predict(stan_model)

# Print 10 predicted scores for 5 songs
posteriors[1:10, 1:5]

# Create data frame of new data
predict_data <- data.frame(song_age = 663, artist_name = "Beyonce")

# Create posterior predictions for Lemonade album
new_predictions <- posterior_predict(stan_model, newdata = predict_data)

# Print first 10 predictions for the new data
new_predictions[1:10,]

# Print a summary of the posterior distribution of predicted popularity
summary(new_predictions[, 1])

##############################################################################################################################

# 3. Visualizing predictions

# View new data predictions
new_predictions[1:10, ]

# Convert to data frame and rename variables
new_predictions <- as.data.frame(new_predictions)
colnames(new_predictions) <- c("Adele", "Taylor Swift", "Beyonce")

# Create tidy data structure
plot_posterior <- gather(new_predictions, key = "artist_name", value = "predict")

# Print formated data
head(plot_posterior)

# Create plot of 
ggplot(plot_posterior, aes(x = predict)) +
  facet_wrap(~ artist_name, ncol = 1) +
  geom_density()