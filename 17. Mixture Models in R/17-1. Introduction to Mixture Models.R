# 1. Introduction to model-based clustering

# Have a look to gender (before clustering)
head(gender)

# Have a look to gender_with_probs (after clustering)
head(gender_with_probs)

# Scatterplot with probabilities
gender_with_probs %>% 
  ggplot(aes(x = Weight, y = BMI, col = probability))+
  geom_point(alpha = 0.5)

###############################################################

# 2. Gaussian distribution

# Set seed
set.seed(1313)

# Simulate a Gaussian distribution
simulation <- rnorm(n = 500, mean = 5, sd = 4)

# Check first six values
head(simulation)

# Estimation of the mean
mean_estimate <- mean(simulation)
mean_estimate

# Estimation of the standard deviation
standard_deviation_estimate <- sd(simulation)
standard_deviation_estimate

# Transform the results to a data frame
simulation <- data.frame(x = simulation)

# Plot the sample with the estimated curve
ggplot(simulation) + geom_histogram(aes(x = x, y = ..density..)) + 
  stat_function(geom = "line", fun = dnorm,
                args = list(mean = mean_estimate, 
                            sd = standard_deviation_estimate))

# Estimation of the mean
mean_estimate <- gender %>% 
  pull(Weight) %>% 
  mean()
mean_estimate

# Estimation of the standard deviation
sd_estimate <- gender %>% 
  pull(Weight) %>% 
  sd()
sd_estimate

# Plot the sample with the estimated curve
gender %>% 
  ggplot() + geom_histogram(aes(x = Weight, y = ..density..), bins = 100) + 
  stat_function(geom = "line", fun = dnorm,
                args = list(mean = mean_estimate, sd = sd_estimate))

###############################################################

# 3. Gaussian mixture models (GMM)

# Create coin object
coin <- sample(c(0, 1), size = 500, 
               replace = TRUE, prob = c(0.2, 0.8))

# Sample from two different Gaussian distributions
mixture <- ifelse(coin == 1, rnorm(n = 500, mean = 5, sd = 2), 
                  rnorm(n = 500))

# Check the first elements
head(mixture)

# Transform into a data frame
mixture <- data.frame(x = mixture)

# Create histogram especifiying that is a density plot
mixture %>% 
  ggplot() + geom_histogram(aes(x = x, y = ..density..), bins = 50)

number_observations <- 1000

# Create the assignment object
assignments <- sample(
  c(0,1,2), size = number_observations, replace = TRUE, prob = c(0.3, 0.4, 0.3)
)

# Simulate the GMM with 3 distributions
mixture <- data.frame(
  x = ifelse(assignments == 1, rnorm(n = number_observations, mean = 5, sd = 2),
      ifelse(assignments == 2, rnorm(n = number_observations, mean = 10, sd = 1),
      rnorm(n = number_observations)))
)

# Plot the mixture
mixture %>% 
  ggplot() + geom_histogram(aes(x = x, y = ..density..), bins = 50)
