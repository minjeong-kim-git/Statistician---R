# 1. Quantifying model fit

# Print a summary of mdl_click_vs_impression_orig
summary(mdl_click_vs_impression_orig)

# Print a summary of mdl_click_vs_impression_trans
summary(mdl_click_vs_impression_trans)

# Get coeff of determination for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)

# Get RSE for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>%
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)

###############################################################

# 2. Visualizing model fit

# Plot the three diagnostics for mdl_price_vs_conv
autoplot(
  mdl_price_vs_conv,
  which = 1 : 3,
  nrow = 3,
  ncol = 1
)

###############################################################

# 3. Outliers, leverage, and influence

mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending leverage
  arrange(desc(.hat)) %>% 
  # Get the head of the dataset
  head()

mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>% 
  # Arrange rows by descending Cook's distance
  arrange(desc(.cooksd)) %>% 
  # Get the head of the dataset
  head()

# Plot the three outlier diagnostics for mdl_price_vs_conv
autoplot(
  mdl_price_vs_dist,
  which = 4:6,
  nrow = 3,
  ncol = 1
)