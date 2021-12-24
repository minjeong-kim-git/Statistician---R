# 1. Missing data: what can go wrong

# Print first 10 observations
head(biopics, 10)

# Get the number of missing values per variable
biopics %>%
  is.na() %>% 
  colSums()

# Fit linear regression to predict earnings
model_1 <- lm(earnings ~ country + year + sub_type, 
              data = biopics)

# Fit linear regression to predict earnings
model_2 <- lm(earnings ~ country + year + sub_type + sub_race, 
              data = biopics)

# Print summaries of both models
summary(model_1)
summary(model_2)

##############################################################################################################################

# 2. Missing data mechanisms

# Create a dummy variable for missing earnings
biopics <- biopics %>% 
  mutate(missing_earnings = is.na(earnings))

# Pull the missing earnings dummy for males
missing_earnings_males <- biopics %>% 
  filter(sub_sex == 'Male') %>% 
  pull(missing_earnings)

# Pull the missing earnings dummy for females
missing_earnings_females <- biopics %>% 
  filter(sub_sex == 'Female') %>% 
  pull(missing_earnings)

# Run the t-test
t.test(missing_earnings_males, missing_earnings_females)

##############################################################################################################################

# 3. Visualizing missing data patterns

# Load the VIM package
library(VIM)

# Draw an aggregation plot of biopics
biopics %>% 
  aggr(combined = TRUE, numbers = TRUE)

# Draw a spine plot to analyse missing values in earnings by sub_race
biopics %>% 
  select(sub_race, earnings) %>%
  spineMiss()

# Prepare data for plotting and draw a mosaic plot
biopics %>%
  # Create a dummy variable for US-produced movies
  mutate(is_US_movie = grepl("US", country)) %>%
  # Draw mosaic plot
  mosaicMiss(highlight = "earnings", 
             plotvars = c("is_US_movie", "sub_sex"))

# Return plot from latest VIM package - expand the HTML viewer section
display_image()