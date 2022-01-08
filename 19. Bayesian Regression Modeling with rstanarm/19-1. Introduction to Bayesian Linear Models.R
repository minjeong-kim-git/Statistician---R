# 1. Non-Bayesian Linear Regression

# Print the first 6 rows
head(songs)

# Print the structure
str(songs)

# Create the model here
lm_model <- lm(popularity ~ song_age, data = songs)

# Produce the summary
summary(lm_model)

# Print a tidy summary of the coefficients
tidy(lm_model)

##############################################################################################################################

# 2. Bayesian Linear Regression

# Create the model here
stan_model <- stan_glm(popularity ~ song_age, data = songs)

# Produce the summary
summary(stan_model)

# Print a tidy summary of the coefficients
tidy(stan_model)

##############################################################################################################################

# 3. Comparing frequentist and Bayesian methods

# Create the 90% credible intervals
posterior_interval(stan_model)

# Create the 95% credible intervals
posterior_interval(stan_model, prob = 0.95)

# Create the 80% credible intervals
posterior_interval(stan_model, prob = 0.8)
