# 1. The normal distribution

# Histogram of amount with 10 bins
ggplot(amir_deals, aes(amount)) + geom_histogram(bins=10)

# Probability of deal < 7500
pnorm(7500, mean=5000, sd=2000)

# Probability of deal > 1000
1 - pnorm(1000, mean=5000, sd=2000)

# Probability of deal between 3000 and 7000
pnorm(7000, mean=5000, sd=2000) - pnorm(3000, mean=5000, sd=2000)

# Calculate amount that 75% of deals will be more than
qnorm(mean=5000, sd=2000, 0.75, lower.tail=FALSE)

# Calculate new average amount
new_mean <- 5000 + 5000 * 0.2

# Calculate new standard deviation
new_sd <- 2000 + 2000 * 0.3

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, new_mean, new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_histogram(bins=10)

###############################################################

# 2. The central limit theorem

# Create a histogram of num_users
ggplot(amir_deals, aes(num_users)) + geom_histogram(bins=10)

# Set seed to 104
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace=TRUE) %>%
  # Take mean
  mean(,c(amir_deals$num_users))

# Sample 20 num_users from amir_deals and take mean
sample(amir_deals$num_users, size = 20, replace = TRUE) %>%
  mean()

# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(mean)) +
  geom_histogram(bins=10)

###############################################################

# 3. The Poisson distribution

# Probability of 5 responses
dpois(5, 4)

# Probability of 5 responses from coworker
dpois(5, 5.5)

# Probability of 2 or fewer responses
1 - ppois(2, 4)

# Probability of > 10 responses
ppois(10, 4, lower.tail=FALSE)

###############################################################

# 4. More probability distributions

# Probability response takes < 1 hour
pexp(1, rate = 1/2.5)

# Probability response takes > 4 hours
pexp(4, rate = 1/2.5, lower.tail=FALSE)

# Probability response takes 3-4 hours
pexp(4, rate = 1/2.5) - pexp(3, rate = 1/2.5)