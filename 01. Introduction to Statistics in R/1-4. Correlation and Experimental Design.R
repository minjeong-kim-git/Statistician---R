# 1. Correlation

# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(x=life_exp, y=happiness_score)) + geom_point()

# Add a linear trendline to scatterplot
ggplot(world_happiness, aes(life_exp, happiness_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)

###############################################################

# 2. Correlation caveats

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(x=gdp_per_cap, y=life_exp)) +
  geom_point()

# Correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap, world_happiness$life_exp)

# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness, aes(x=gdp_per_cap, y=happiness_score)) +
  geom_point()

# Calculate correlation
cor(world_happiness$gdp_per_cap, world_happiness$happiness_score)

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log((world_happiness$gdp_per_cap)))

# Scatterplot of log_gdp_per_cap vs. happiness_score
ggplot(world_happiness, aes(log_gdp_per_cap, happiness_score)) +
  geom_point()

# Calculate correlation
cor(world_happiness$happiness_score, world_happiness$log_gdp_per_cap)

# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness, aes(grams_sugar_per_day, happiness_score)) +
  geom_point()

# Correlation between grams_sugar_per_day and happiness_score
cor(world_happiness$grams_sugar_per_day,
    world_happiness$happiness_score)