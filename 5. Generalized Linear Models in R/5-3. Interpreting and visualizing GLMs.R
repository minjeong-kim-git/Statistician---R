# 1. lm vs. Poisson coefficients

# Extract the coeffients from lm_out
lm_coef <- coef(lm_out)
lm_coef

# Extract the coefficients from poisson_out
poisson_coef <- coef(poisson_out)
poisson_coef

# Take the exponential using exp()
poisson_coef_exp <- exp(poisson_coef)
poisson_coef_exp

########################################################################

# 2. Poisson regression plotting

# Use geom_smooth to plot a continuous predictor variable
ggplot(data = dat, aes(x = dose, y = cells)) + 
  geom_jitter(width = 0.05, height = 0.05) + 
  geom_smooth(method = 'glm', method.args = list(family = 'poisson'))

########################################################################

# 3. Extracting and interpreting odds-ratios

# Extract out the coefficients 
coef_out <- coef(bus_out)

# Convert the coefficients to odds-ratios 
exp(coef_out)

########################################################################

# 4. Odds-ratios & confidence intervals in the Tidyverse

# Exponentiate the results and extract the confidence intervals of bus_out with tidy()
tidy(bus_out, exponentiate=TRUE, conf.int=TRUE)

########################################################################

# 5. Default trend lines

# Create a jittered plot of MilesOneWay vs Bus2 using the bus dataset
gg_jitter <- ggplot(data = bus, aes(x = MilesOneWay, y = Bus2)) + 
  geom_jitter(width = 0, height = 0.05) +
  ylab("Probability of riding the bus") +
  xlab("One-way commute trip (in miles)")

# Add a geom_smooth() to your plot
gg_jitter + geom_smooth()

########################################################################

# 6. Methods for trend lines

# Create a jittered plot of MilesOneWay vs Bus2 using the bus dataset
gg_jitter <- ggplot(data = bus, aes(x = MilesOneWay, y = Bus2)) + 
  geom_jitter(width = 0, height = 0.05) +
  ylab("Probability of riding the bus") +
  xlab("One-way commute trip (in miles)")

# Add a geom_smooth() that uses a GLM method to your plot
gg_jitter + geom_smooth(method =  'glm' , method.args = list(family="binomial"))

########################################################################

# 7. Comparing probits and logits

# Add geom_smooth() lines for the probit and logit link functions
gg_jitter + 
  geom_smooth(method = 'glm', 
              method.args = list(binomial(link = 'probit')), 
              color = 'red', se = FALSE) +
  geom_smooth(method = 'glm', 
              method.args = list(binomial(link = 'logit')), 
              color = 'blue', se = FALSE)