# 1. Bernoulli Mixture Models

# Create the vector of probabilities
p_cluster_1 <- c(0.8, 0.8, 0.2, 0.9)

# Create the sample for each pixel
pixel_1 <- sample(c(0, 1), 100, replace = TRUE, 
                  prob = c(1 - p_cluster_1[1], p_cluster_1[1]))
pixel_2 <- sample(c(0, 1), 100, replace = TRUE, 
                  prob = c(1 - p_cluster_1[2], p_cluster_1[2]))
pixel_3 <- sample(c(0, 1), 100, replace = TRUE, 
                  prob = c(1 - p_cluster_1[3], p_cluster_1[3]))
pixel_4 <- sample(c(0, 1), 100, replace = TRUE, 
                  prob = c(1 - p_cluster_1[4], p_cluster_1[4]))
# Combine the samples
sample_cluster_1 <- cbind(pixel_1, pixel_2, pixel_3, pixel_4)

# Have a look to the sample
head(sample_cluster_1)

##############################################################################################################################

# 2. Bernoulli Mixture Models with flexmix

# transform into matrix
digits_sample_2 <- as.matrix(digits_sample_2)

# dimension
dim(digits_sample_2)

# look to the first observation
show_digit(digits_sample_2[1,])

# look to the last observation
show_digit(digits_sample_2[nrow(digits_sample_2),])

set.seed(1513)
# Fit Bernoulli mixture model
bernoulli_mix_model <- flexmix(digits_sample_2 ~ 1,
                               k = 3,
                               model = FLXMCmvbinary(),
                               control = list(tolerance = 1e-15, iter.max = 1000))
# Check the proportions
prior(bernoulli_mix_model)

# Extract the parameters for each cluster
param_comp_1 <- parameters(bernoulli_mix_model, component = 1)
param_comp_2 <- parameters(bernoulli_mix_model, component = 2)
param_comp_3 <- parameters(bernoulli_mix_model, component = 3)

# Visualize the clusters
show_digit(param_comp_1)
show_digit(param_comp_2)
show_digit(param_comp_3)

##############################################################################################################################

# 3. Poisson Mixture Models

set.seed(1541)

# Create the vector of lambdas
lambda_1 <- c(150, 300, 50)

# Create the sample of each crime
assault_1 <- rpois(n = 10, lambda = lambda_1[1])
robbery_1 <- rpois(n = 10, lambda = lambda_1[2])
battery_1 <- rpois(n = 10, lambda = lambda_1[3])

# Combine the results
cities_1 <- cbind(assault_1, robbery_1, battery_1)

# Check the sample
cities_1

##############################################################################################################################

# 4. Poisson Mixture Models with flexmix

# Check the dimension
dim(crimes)

# Check with glimpse
glimpse(crimes)

# Transform into a matrix, without `community`
matrix_crimes <- crimes %>%
  select(- community) %>%  
  as.matrix()

# Check the first values
head(matrix_crimes)

set.seed(2017)

# Fit the Poisson mixture model
poisson_mm <- stepFlexmix(matrix_crimes ~ 1, 
                          k = 1:15, 
                          nrep = 5, 
                          model = FLXMCmvpois(),
                          control = list(tolerance = 1e-15, iter.max = 1000))

# Select the model that minimize the BIC
best_poisson_mm <- getModel(poisson_mm, which = "BIC")

# Get the parameters into a data frame
params_lambdas <- data.frame(parameters(best_poisson_mm))

# Add the column with the type of crime
params_lambdas_crime <- params_lambdas %>% 
  mutate(crime = colnames(matrix_crimes))

# Plot the clusters with their lambdas
params_lambdas_crime %>% 
  gather(cluster, lambdas, - crime) %>% 
  ggplot(aes(x = crime, y = lambdas, fill = crime)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ cluster) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none")

# Add the cluster assignments
crimes_with_clusters <- crimes %>% 
  mutate(cluster = factor(clusters(best_poisson_mm)))

# Enumerate the cluster's elements
crimes_with_clusters <- crimes_with_clusters %>% 
  group_by(cluster) %>% 
  mutate(number = row_number()) 

# Plot the clusters with the communities
crimes_with_clusters %>% 
  ggplot(aes(x = cluster, y = number, col = cluster)) + 
  geom_text(aes(label = community), size = 2.3)+
  theme(legend.position="none")