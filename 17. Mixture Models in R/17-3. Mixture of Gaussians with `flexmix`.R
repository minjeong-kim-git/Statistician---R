# 2. Univariate Gaussian Mixture Models with flexmix

set.seed(1515)
fit_mix_example <- flexmix(x ~ 1, 
                           data = mix_example, 
                           k = 3,
                           model = FLXMCnorm1(),
                           control = list(tolerance = 1e-15, verbose = 1, iter = 1e4))

proportions <- prior(fit_mix_example)
comp_1 <- parameters(fit_mix_example, component = 1)
comp_2 <- parameters(fit_mix_example, component = 2)
comp_3 <- parameters(fit_mix_example, component = 3)

ggplot(mix_example) + geom_histogram(aes(x = x, y = ..density..)) + 
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_1[1], sd = comp_1[2], 
                            proportion = proportions[1])) +
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_2[1], sd = comp_2[2], 
                            proportion = proportions[2]))+
  stat_function(geom = "line", fun = fun_prop, 
                args = list(mean = comp_3[1], sd = comp_3[2], 
                            proportion = proportions[3]))

# Explore the first assignments
head(clusters(fit_mix_example))

# Explore the first real labels
head(mix_example$assignment)

# Create frequency table
table(mix_example$assignment, clusters(fit_mix_example))

##############################################################################################################################

# 4. Bivariate Gaussian Mixture Models with flexmix

set.seed(1313)
fit_with_covariance <- flexmix(cbind(Weight, BMI) ~ 1,
                               data = gender,
                               k = 2, 
                               model = FLXMCmvnorm(diag = TRUE),
                               control = list(tolerance = 1e-15, iter.max = 1000))

# Get the parameters
comp_1 <- parameters(fit_with_covariance, component = 1)
comp_2 <- parameters(fit_with_covariance, component = 2)

# The means
mean_comp_1 <- comp_1[1:2]
mean_comp_1
mean_comp_2 <- comp_2[1:2]
mean_comp_2

# The covariance matrices
covariance_comp_1 <- matrix(comp_1[3:6], nrow = 2)
covariance_comp_1
covariance_comp_2 <- matrix(comp_2[3:6], nrow = 2)
covariance_comp_2

# Create ellipse curve 1
ellipse_comp_1 <- ellipse(x = covariance_comp_1, 
                          centre = mean_comp_1,
                          npoints = nrow(gender))
head(ellipse_comp_1)

# Create ellipse curve 2
ellipse_comp_2 <- ellipse(x = covariance_comp_2, 
                          centre = mean_comp_2,
                          npoints = nrow(gender))
head(ellipse_comp_2)

# Plot the ellipses
gender %>% 
  ggplot(aes(x = Weight, y = BMI)) + geom_point()+
  geom_path(data = data.frame(ellipse_comp_1), aes(x=x,y=y), col = "red") +
  geom_path(data = data.frame(ellipse_comp_2), aes(x=x,y=y), col = "blue")
# Check the assignments
table(gender$Gender, clusters(fit_with_cov))