# 1. Fitting a multiple logistic regression

# Build a logistic regression with Bus predicted by CommuteDays and MilesOneWay
bus_both <- glm(formula=Bus~CommuteDays + MilesOneWay, data=bus, family="binomial")

# Look at the summary of the output
summary(bus_both)

########################################################################

# 2. Building two models

# Build a logistic regression with Bus predicted by CommuteDays
bus_days <- glm(formula=Bus~CommuteDays, data=bus, family="binomial")

# Build a logistic regression with Bus predicted by MilesOneWay
bus_miles <- glm(formula=Bus~ MilesOneWay, data=bus, family="binomial")

########################################################################

# 3. Comparing variable order

# Run a correlation
cor(bus$CommuteDays, bus$MilesOneWay)

# Build a glm with CommuteDays first and MilesOneWay second
bus_one <- glm(formula=Bus~CommuteDays + MilesOneWay, data=bus, family="binomial")

# Build a glm with MilesOneWay first and CommuteDays second
bus_two <- glm(formula=Bus~MilesOneWay + CommuteDays, data=bus, family="binomial")

# Print model summaries
summary(bus_one)
summary(bus_two)

########################################################################

# 4. Multiple slopes

# Use model.matrix() with size
model.matrix(~size)

# Use model.matrix() with size and count
model.matrix(~size + count)

########################################################################

# 5. Intercepts

# Create a matrix that includes a reference intercept
model.matrix(~color)

# Create a matrix that includes an intercept for each group
model.matrix(~color - 1)

########################################################################

# 6. Multiple intercepts

# Create a matrix that includes color and then shape  
model.matrix( ~ color + shape - 1)

# Create a matrix that includes shape and then color 
model.matrix(~ shape + color - 1)

########################################################################

# 7. Simpson's paradox

# Build a binomial glm where Admitted and Rejected are predicted by Gender
glm_1 <- glm(formula=cbind(Admitted, Rejected) ~ Gender, data=UCB_data, family="binomial")

# Build a binomial glm where Admitted and Rejected are predicted by Gender and Dept
glm_2 <- glm(cbind(Admitted, Rejected) ~ Gender + Dept, family = 'binomial', data = UCB_data)

# Look at the summary of both models
summary(glm_1)
summary(glm_2)

########################################################################

# 8. Non-linear logistic regression

# Plot linear effect of travel distance on probability of taking the bus
gg_jitter <-
  ggplot(data = bus, aes(x = MilesOneWay, y = Bus2)) + 
  geom_jitter(width = 0, height = 0.05) + 
  geom_smooth(method = 'glm', 
              method.args = list(family = 'binomial'))

# Add a non-linear equation to a geom_smooth()
gg_jitter +
  geom_smooth(method = 'glm', 
              method.args = list(family = 'binomial'), 
              formula = y ~ I(x^2), 
              color = 'red')
