# 1. Using the R Squared statistic

# Print the R-squared from the linear model
lm_summary$r.squared

# Calulate sums of squares
ss_res <- var(residuals(lm_model))
ss_fit <- var(fitted(lm_model))

# Calculate the R-squared
1 - (ss_res / (ss_fit + ss_res))

# Save the variance of residulas
ss_res <- var(residuals(stan_model))

# Save the variance of fitted values
ss_fit <- var(fitted(stan_model))

# Calculate the R-squared
1 - (ss_res / (ss_res + ss_fit))

##############################################################################################################################

# 2. Posterior predictive model checks

# Calculate posterior predictive scores
predictions <- posterior_linpred(stan_model)

# Print a summary of the observed data
summary(songs$popularity)

# Print a summary of the 1st replication
summary(predictions[1,])

# Print a summary of the 10th replication
summary(predictions[10,])

##############################################################################################################################

# 3. Model fit with posterior predictive model checks

# Calculate the posterior distribution of the R-squared
r2_posterior <- bayes_R2(stan_model)

# Make a histogram of the distribution
hist(r2_posterior)

# Create density comparison
pp_check(stan_model, "dens_overlay")

# Create scatter plot of means and standard deviations
pp_check(stan_model, "stat_2d")

##############################################################################################################################

# 4. Bayesian model comparisons

# Estimate the model with 1 predictor
model_1pred <- stan_glm(popularity ~ song_age, data = songs)

# Print the LOO estimate for the 1 predictor model
loo(model_1pred)

# Estimate the model with both predictors
model_2pred <- stan_glm(popularity ~ song_age * artist_name, data = songs)

# Print the LOO estimates for the 2 predictor model
loo(model_2pred)