# 1. Latin squares

# Mean, var, and median of Math score
nyc_scores %>%
  group_by(Borough) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
  group_by(Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
  group_by(Borough, Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Load naniar
library(naniar)

# Examine missingness with miss_var_summary()
nyc_scores %>%
  miss_var_summary()

# Examine missingness with md.pattern()
md.pattern(nyc_scores)

# Impute the Math score by Borough
nyc_scores_2 <- impute_median(nyc_scores, Average_Score_SAT_Math ~ Borough)

# Convert Math score to numeric
nyc_scores_2$Average_Score_SAT_Math <- as.numeric(nyc_scores_2$Average_Score_SAT_Math)

# Examine scores by Borough in both datasets, before and after imputation
nyc_scores %>% 
  group_by(Borough) %>% 
  summarize(median = median(Average_Score_SAT_Math, na.rm = TRUE), 
            mean = mean(Average_Score_SAT_Math, na.rm = TRUE))
nyc_scores_2 %>% 
  group_by(Borough) %>% 
  summarize(median = median(Average_Score_SAT_Math), 
            mean = mean(Average_Score_SAT_Math))

###############################################################

# 2. Graeco-Latin squares

# Create a boxplot of Math scores by Borough, with a title and x/y axis labels
ggplot(nyc_scores, aes(Borough, Average_Score_SAT_Math)) +
  geom_boxplot() + 
  labs(title = "Average SAT Math Scores by Borough, NYC",
       x = "Borough (NYC)",
       y = "Average SAT Math Scores (2014-15)")

# Create trt1 and trt2
trt1 <- LETTERS[1:5]
trt2 <- 1:5

# Create my_graeco_design
my_graeco_design <- design.graeco(trt1, trt2, seed = 42)

# Examine the parameters and sketch
my_graeco_design$parameters
my_graeco_design$sketch

# Build nyc_scores_gls_lm
nyc_scores_gls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program +
                          Borough + Teacher_Education_Level + Homework_Type,
                        data = nyc_scores_gls )

# Tidy the results with broom
tidy(nyc_scores_gls_lm)

# Examine the results with anova
anova(nyc_scores_gls_lm)

###############################################################

# 3. Factorial experiments

# Load ggplot2
library(ggplot2)

# Build the boxplot for the tutoring program vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Tutoring_Program, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Build the boxplot for the percent black vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Black_HL, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Build the boxplot for percent tested vs. Math SAT score
ggplot(nyc_scores,
       aes(x = Percent_Tested_HL, y = Average_Score_SAT_Math)) + 
  geom_boxplot()

# Create nyc_scores_factorial and examine the results
nyc_scores_factorial <- aov(Average_Score_SAT_Math ~ Percent_Tested_HL *
                              Percent_Black_HL * Tutoring_Program,
                            data = nyc_scores)

tidy(nyc_scores_factorial)

# Use shapiro.test() to test the outcome
shapiro.test(nyc_scores$Average_Score_SAT_Math)

# Plot nyc_scores_factorial to examine residuals
par(mfrow = c(2,2))
plot(nyc_scores_factorial)