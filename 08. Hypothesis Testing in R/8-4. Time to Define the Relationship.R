# 1. What do you assume?

# Get counts by freight_cost_group
counts <- late_shipments %>%
  count(freight_cost_group)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 30)

# Get counts by late
counts <- late_shipments %>%
  count(late)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 10)

# Count the values of vendor_inco_term and freight_cost_group
counts <- late_shipments %>%
  count(vendor_inco_term,freight_cost_group)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 5)

# Count the values of shipment_mode
counts <- late_shipments %>%
  count(shipment_mode)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 30)

###############################################################

# 2. X-ray specs: don't believe the hyp

# Perform a proportion test appropriate to the hypotheses 
test_results <- late_shipments %>% 
  prop_test(
    late ~ freight_cost_group,
    order = c("expensive", "reasonable"),
    success = "Yes",
    alternative = "greater",
    correct = FALSE
  )

# See the results
test_results

# Specify that we are interested in late proportions across
# freight_cost_groups, where "Yes" denotes success
specified <- late_shipments %>%
  specify(late ~ freight_cost_group, success = 'Yes')

# See the result
specified

# Extend the pipeline to declare a null hypothesis that 
# the variables are independent
hypothesized <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = 'independence')

# See the result
hypothesized

###############################################################

# 3. The generation game

# Extend the pipeline to generate 2000 permutations
generated <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = 'permute')

# See the result
generated

# Extend the pipeline to calculate the difference in 
# proportions (expensive minus reasonable)
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = 'diff in props',
    order = c('expensive', 'reasonable')
  )

# See the result
null_distn

# Visualize the null distribution
visualize(null_distn)

# Copy, paste, and modify the pipeline to get the observed statistic
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  # hypothesize(null = "independence") %>% 
  # generate(reps = 5000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Visualize the null dist'n, adding a vertical line at
# the observed statistic
visualize(null_distn) +
  geom_vline(
    aes(xintercept = stat),
    data = obs_stat,
    color = 'red'
  )

# Get the p-value
p_value <- get_p_value(
  null_distn, obs_stat,
  direction = 'two sided'
)

# See the result
p_value

###############################################################

# 4. Look ma! No parameters!

# Fill out the null distribution pipeline
null_distn <- late_shipments %>% 
  # Specify weight_kilograms vs. late
  specify(weight_kilograms ~ late) %>% 
  # Declare a null hypothesis of independence
  hypothesize(null = 'independence') %>% 
  # Generate 1000 permutation replicates
  generate(reps = 1000, type = 'permute') %>% 
  # Calculate the difference in means ("No" minus "Yes")
  calculate(
    stat = 'diff in means',
    order = c('No', 'Yes')
  )

# See the results
null_distn

# Calculate the observed difference in means
obs_stat <- late_shipments %>%
  specify(weight_kilograms ~ late) %>%
  calculate(
    stat = "diff in means",
    order = c("No", "Yes")
  )

# See the result
obs_stat

# Get the p-value
p_value <- get_p_value(
  null_distn, obs_stat,
  direction = 'less'
)

# See the result
p_value

# Run a Wilcoxon-Mann-Whitney test on weight_kilograms vs. late
test_results <- wilcox.test(
  weight_kilograms ~ late,
  data = late_shipments
)

# See the result
test_results

# Run a Kruskal-Wallace test on weight_kilograms vs. shipment_mode
test_results <- kruskal.test(
  weight_kilograms ~ shipment_mode,
  data = late_shipments
)

# See the result
test_results