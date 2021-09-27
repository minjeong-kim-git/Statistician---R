# 1. Fitting a logistic regression

# Build a glm using the bus data frame that models Bus predicted by CommuteDays
bus_out <- glm(formula=Bus~CommuteDays, data=bus,family="binomial")

########################################################################

# 2. Examining & interpreting logistic regression outputs

# Print the bus_out with the print() function
print(bus_out)

# Print the bus_out (be sure to use the print function)
print(bus_out)

# Look at the summary() of bus_out
summary(bus_out)

# Look at the tidy() output of bus_out
tidy(bus_out)

########################################################################

# 3. Simulating binary data

# Simulate 1 draw with a sample size of 100
binomial_sim <- rbinom(n=1, size=100, p=0.5)

# Simulate 100 draw with a sample size of 1 
bernoulli_sim <- rbinom(n = 100, size = 1, prob = 0.5)

# Print the results from the binomial
print(binomial_sim)

# Sum the results from the Bernoulli
sum(bernoulli_sim)

########################################################################

# 4. Long-form logistic regression input

# Fit a a long format logistic regression
lr_1 <- glm(formula=y~x, data=data_long, family="binomial")
print(lr_1)

########################################################################

# 5, Wide-form input logistic regression

# Fit a wide form logistic regression
lr_2 <- glm(cbind(success, fail) ~ x, data=data_wide, family="binomial")

# Print the output of lr_2
print(lr_2)

# Using data_wide, fit a glm with successProportion
# predicted by x and weights = Total
lr_3 <-  glm(successProportion ~ x, family = 'binomial',
             data = data_wide, weights=Total)

# Print the output of lr_3
print(lr_3)

########################################################################

# 6. Fitting probits and logits

# Fit a GLM with a logit link and save it as bus_logit
bus_logit <- glm(formula=Bus~CommuteDays, data=bus, family=binomial(link="logit"))

# Fit a GLM with probit link and save it as bus_probit
bus_probit <- glm(Bus ~ CommuteDays, data = bus, family = binomial(link = "probit"))

# Print model summaries
summary(bus_logit)
summary(bus_probit)

########################################################################

# 7. Simulating a logit

# Convert from the logit scale to a probability
p <- plogis(0)

# Simulate a logit 
rbinom(n=10, size=1, p=p)

########################################################################

# 8. Simulating a probit

# Convert from the probit scale to a probability
p <- pnorm(0)

# Simulate a probit
rbinom(n=10, size=1, p=p)
