# 1. What's in a Bayesian Model?

# 3 chains, 1000 iterations, 500 warmup
model_3chains <- stan_glm(popularity ~ song_age, data = songs,
                          chains = 3, iter = 1000, warmup = 500)

# Print a summary of model_3chains
summary(model_3chains)

# 2 chains, 100 iterations, 50 warmup
model_2chains <- stan_glm(popularity ~ song_age, data = songs,
                          chains = 2, iter = 100, warmup = 50)

# Print a summary of model_2chains
summary(model_2chains)

##############################################################################################################################

# 2. Prior distributions

# Estimate the model
stan_model <- stan_glm(popularity ~ song_age, data = songs)

# Print a summary of the prior distributions
prior_summary(stan_model)

# Calculate the adjusted scale for the intercept
10 * sd(songs$popularity)

# Calculate the adjusted scale for `song_age`
(2.5 / sd(songs$song_age)) * sd(songs$popularity)

# Calculate the adjusted scale for `valence`
(2.5 / sd(songs$valence)) * sd(songs$popularity)

# Estimate the model with unadjusted scales
no_scale <- stan_glm(popularity ~ song_age, data = songs,
                     prior_intercept = normal(autoscale = FALSE),
                     prior = normal(autoscale = FALSE),
                     prior_aux = exponential(autoscale = FALSE)
)

# Print the prior summary
prior_summary(no_scale)

##############################################################################################################################

# 3. User Specified Priors

# Estimate a model with flat priors
flat_prior <- stan_glm(popularity ~ song_age, data = songs,
                       prior_intercept = NULL,
                       prior = NULL,
                       prior_aux = NULL
)

# Print a prior summary
prior_summary(flat_prior)

# Estimate the model with an informative prior
inform_prior <- stan_glm(popularity ~ song_age, data = songs,
                         prior = normal(location = 20, scale = 0.1, autoscale = FALSE))

# Print the prior summary
prior_summary(inform_prior)

##############################################################################################################################

# 4. Altering the estimation process

# Estimate the model with a new `adapt_delta`
adapt_model <- stan_glm(popularity ~ song_age, data = songs,
                        control = list(adapt_delta = 0.99))

# View summary
summary(adapt_model)

# Estimate the model with a new `max_treedepth`
tree_model <- stan_glm(popularity ~ song_age, data = songs,
                       control = list(max_treedepth = 15))

# View summary
summary(tree_model)
