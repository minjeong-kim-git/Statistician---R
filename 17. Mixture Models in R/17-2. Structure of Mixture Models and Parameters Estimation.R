# 1. Structure of mixture models

# Dimension
glance(data)

# Apply `glimpse` to the data
glimpse(data)

# Digit in row 50
show_digit(data[50,])

# Digit in row 100
show_digit(data[100,])

##############################################################################################################################

# 2. Parameters estimation

# Estimation of the means
means_estimates <- gaussian_sample_with_probs %>% 
  summarise(mean_cluster1= sum(x * prob_cluster1) / sum(prob_cluster1),
            mean_cluster2 = sum(x * prob_cluster2) / sum(prob_cluster2))
means_estimates

# Estimation of the proportions
props_estimates <- gaussian_sample_with_probs %>% 
  summarise(props_cluster1 = mean(prob_cluster1),
            props_cluster2 = 1 - props_cluster1)
props_estimates

# Transform to a vector
means_estimates <- as.numeric(means_estimates)

# Plot histogram with means estimates
ggplot(gaussian_sample_with_probs) + geom_histogram(aes(x = x), bins = 100) +
  geom_vline(xintercept = means_estimates)

# Create data frame with probabilities
gaussian_sample_with_probs <- gaussian_sample %>% 
  mutate(prob_from_cluster1 = 0.35 * dnorm(x, mean = 10, sd = 10),
         prob_from_cluster2 = 0.65 * dnorm(x, mean = 50, sd = 10),
         prob_cluster1 = prob_from_cluster1 / (prob_from_cluster1 + prob_from_cluster2),
         prob_cluster2 = prob_from_cluster2 / (prob_from_cluster1 + prob_from_cluster2)) %>%
  select(x, prob_cluster1, prob_cluster2) 

head(gaussian_sample_with_probs)

##############################################################################################################################

# 3. EM algorithm

expectation <- function(data, means, proportions, sds){
  # Estimate the probabilities
  exp_data <- data %>% 
    mutate(prob_from_cluster1 = proportions[1] * dnorm(x, mean = means[1], sd = sds[1]),
           prob_from_cluster2 = proportions[2] * dnorm(x, mean = means[2], sd = sds[2]),
           prob_cluster1 = prob_from_cluster1/(prob_from_cluster1 + prob_from_cluster2),
           prob_cluster2 = prob_from_cluster2/(prob_from_cluster1 + prob_from_cluster2)) %>% 
    select(x, prob_cluster1, prob_cluster2)
  
  # Return data with probabilities
  return(exp_data)
}

maximization <- function(data_with_probs){
  means_estimates <- data_with_probs %>%
    summarise(mean_1 = sum(x * prob_cluster1) / sum(prob_cluster1),
              mean_2 = sum(x * prob_cluster2) / sum(prob_cluster2)) %>% 
    as.numeric()
  props_estimates <- data_with_probs %>% 
    summarise(proportion_1 = mean(prob_cluster1),
              proportion_2 = 1 - proportion_1) %>% 
    as.numeric()
  list(means_estimates, props_estimates)   
}

means_init <- c(0, 100)
props_init <- c(0.5, 0.5)

# Iterative process
for(i in 1:100){
  new_values <- maximization(expectation(gaussian_sample, means_init, props_init, c(10, 10)))
  means_init <- new_values[[1]]
  props_init <- new_values[[2]]
  cat(c(i, means_init, props_init), "\n")
}

gaussian_sample %>% 
  ggplot() + geom_histogram(aes(x = x, y = ..density..), bins = 200) +
  stat_function(geom = "line", fun = fun_gaussian,
                args = list(mean = means_iter10[1], proportion = props_iter10[1])) +
  stat_function(geom = "line", fun = fun_gaussian,
                args = list(mean = means_iter10[2], proportion = props_iter10[2]))