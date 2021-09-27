#1.Refresher on fitting linear models

# Fit a lm()
lm(formula = weight ~ Diet, data = chick_weight_end)
# Fit a glm()
glm(formula = weight ~ Diet, data =chick_weight_end, family = 'gaussian')

##########################################################################

# 2. Fitting a Poisson regression in R
# fit y predicted by x with data.frame dat using the poisson family
poisson_out <- glm(formula = count ~ time,data=dat, family='poisson')

# print the output
print(poisson_out)

# 3. Comparing linear and Poisson regression
# Fit a glm with count predicted by time using data.frame dat and gaussian family
lm_out <- glm(formula=count~time, data=dat, family='gaussian')

summary(lm_out)
summary(poisson_out)

# 4. Intercepts-comparisons versus means
# Fit a glm() that estimates the difference between players
summary(glm(formula = goal ~ player, data=scores, family='poisson'))

##########################################################################

# 5. Applying summary(), print(), and tidy() to glm
# Build your linear and Poisson regression models
lm_out <- lm(formula=Number~Month, data=dat)
poisson_out <- glm(formula=Number~Month, data=dat, family='poisson')

# Build your linear and Poisson regression models
lm_out <- lm(Number ~ Month, data = dat)
poisson_out <- glm(Number ~ Month, data = dat, family = 'poisson')

# Examine the outputs using print()
print(lm_out)
print(poisson_out)

# Build your linear and Poisson regression models
lm_out <- lm(Number ~ Month, data = dat)
poisson_out <- glm(Number ~ Month, data = dat, family = 'poisson')

# Examine the outputs using print()
summary(lm_out)
summary(poisson_out)

# Build your linear and Poisson regression models
lm_out <- lm(Number ~ Month, data = dat)
poisson_out <- glm(Number ~ Month, data = dat, family = 'poisson')

# Examine the outputs using tidy()
tidy(lm_out)
tidy(poisson_out)

##########################################################################

# 6. Extracting coefficients from glm()
# Extract the regression coefficients
coef(poisson_out)

# Extract the confidence intervals
confint(poisson_out)

# 7. Predicting with glm()
# print the new input months
print(new_dat)

# use the model to predict with new data 
pred_out <- predict(object = poisson_out, newdata = new_dat, type = "response")

# print the predictions
print(pred_out)