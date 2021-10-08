# 1. This bears a striking resample-lance

# Generate 1 bootstrap resample
spotify_1_resample <- spotify_sample %>%
  slice_sample(prop = 1, replace = TRUE)

# See the result
spotify_1_resample

# Calculate mean danceability of resample
mean_danceability_1 <- spotify_1_resample %>%
  summarize(mean_danceability = mean(danceability, na.rm = TRUE)) %>%
  pull(mean_danceability)

# See the result
mean_danceability_1

# Replicate this 1000 times
mean_danceability_1000 <- replicate(
  n = 1000,
  expr = {
    spotify_1_resample <- spotify_sample %>% 
      slice_sample(prop = 1, replace = TRUE)
    spotify_1_resample %>% 
      summarize(mean_danceability = mean(danceability)) %>% 
      pull(mean_danceability)
  }
)

# See the result
mean_danceability_1000

# Store the resamples in a tibble
bootstrap_distn <- tibble(
  resample_mean = mean_danceability_1000
)

# Draw a histogram of the resample means with binwidth 0.002
ggplot(bootstrap_distn, aes(x = resample_mean)) +
  geom_histogram(binwidth = 0.002)

###############################################################

# 2. A breath of fresh error

# Generate a sampling distribution
mean_popularity_2000_samp <- replicate(
  # Use 2000 replicates
  n = 2000,
  expr = {
    # Start with the population
    spotify_population %>% 
      # Sample 500 rows without replacement
      slice_sample(n = 500) %>% 
      # Calculate the mean popularity as mean_popularity
      summarize(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull(mean_popularity)
  }
)

# See the result
mean_popularity_2000_samp

# Generate a bootstrap distribution
mean_popularity_2000_boot <- replicate(
  # Use 2000 replicates
  n = 2000,
  expr = {
    # Start with the sample
    spotify_sample %>% 
      # Sample same number of rows with replacement
      slice_sample(prop = 1, replace=TRUE) %>% 
      # Calculate the mean popularity
      summarize(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull(mean_popularity)
  }
)

# See the result
mean_popularity_2000_boot

# Calculate the true population mean popularity
pop_mean <- spotify_population %>% 
  summarize(mean(popularity))

# Calculate the original sample mean popularity
samp_mean <- spotify_sample %>% 
  summarize(mean(popularity))

# Calculate the sampling dist'n estimate of mean popularity
samp_distn_mean <- sampling_distribution %>% 
  summarize(mean(sample_mean))

# Calculate the bootstrap dist'n estimate of mean popularity
boot_distn_mean <- bootstrap_distribution %>% 
  summarize(mean(resample_mean))

# See the results
c(pop = pop_mean, samp = samp_mean, samp_distn = samp_distn_mean, boot_distn = boot_distn_mean)

# Calculate the true popluation std dev popularity
pop_sd <- spotify_population %>%
  summarize(sd(popularity))

# Calculate the true sample std dev popularity
samp_sd <- spotify_sample %>%
  summarize(sd(popularity))

# Calculate the sampling dist'n estimate of std dev popularity
samp_distn_sd <- sampling_distribution %>%
  summarize(sd(sample_mean) * sqrt(500))

# Calculate the bootstrap dist'n estimate of std dev popularity
boot_distn_sd <- bootstrap_distribution %>%
  summarize(sd(resample_mean) * sqrt(500))

## See the results
c(pop = pop_sd, samp = samp_sd, sam_distn = samp_distn_sd, boot_distn = boot_distn_sd)

###############################################################

# 3. Venus infers

# Generate a 95% confidence interval using the quantile method
conf_int_quantile <- bootstrap_distribution %>% 
  summarize(
    lower = quantile(resample_mean, 0.025),
    upper = quantile(resample_mean, 0.975)
  )

# See the result
conf_int_quantile

# Generate a 95% confidence interval using the std error method
conf_int_std_error <- bootstrap_distribution %>%
  summarize(
    point_estimate = mean(resample_mean),
    standard_error = sd(resample_mean),
    lower = qnorm(mean =  point_estimate, sd = standard_error, p = 0.025),
    upper = qnorm(mean =  point_estimate, sd = standard_error, p = 0.975)
  )

# See the result
conf_int_std_error