# 1. Difference strokes for proportions, folks

# Hypothesize that the proportion of late shipments is 6%
p_0 <- 0.06

# Calculate the sample proportion of late shipments
p_hat <- late_shipments %>%
  summarize(mean(late == "Yes"))

# Calculate the sample size
n <- nrow(late_shipments)

# Calculate the numerator of the test statistic
numerator <- p_hat - p_0

# Calculate the denominator of the test statistic
denominator <- sqrt(p_0 * (1 - p_0) / n)

# Calculate the test statistic
z_score <- numerator / denominator

# See the result
z_score

# Calculate the p-value from the z-score
p_value <- pnorm(z_score, lower.tail = FALSE)

# See the result
p_value

###############################################################

#2. A sense of proportion

# See the sample variables
print(p_hats)
print(ns)

# Calculate the pooled estimate of the population proportion
p_hat <- weighted.mean(p_hats, ns)

# See the result
p_hat

# Calculate sample prop'n times one minus sample prop'n
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)

# Divide this by the sample sizes
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns

# Calculate std. error
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))

# See the result
std_error

# Calculate the z-score
z_score <- (p_hats["expensive"] - p_hats["reasonable"]) / std_error

# See the result
z_score

# Calculate the p-value from the z-score
p_value <- pnorm(z_score, lower.tail = FALSE)

# See the result
p_value

# Perform a proportion test appropriate to the hypotheses 
test_results <- late_shipments %>%
  prop_test(
    late ~ freight_cost_group,
    order = c('expensive', 'reasonable'),
    success = 'Yes',
    alternative = 'greater',
    correct = 'FALSE'
  )

# See the results
test_results

###############################################################

# 3. Declaration of independence

# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(vendor_inco_term, fill = freight_cost_group)) +
  geom_bar(position = 'fill')

# Perform a chi-square test of independence on
# freight_cost_group and vendor_inco_term
test_results <- late_shipments %>%
  chisq_test(freight_cost_group ~ vendor_inco_term)


# See the results
test_results

###############################################################

# 4. Does this dress make my fit look good?

# Using late_shipments, count the vendor incoterms
vendor_inco_term_counts <- late_shipments %>%
  count(vendor_inco_term)

# Get the number of rows in the whole sample
n_total <- nrow(late_shipments)

hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  # Add a column of hypothesized counts for the incoterms
  mutate(n = prop * n_total)

# Using vendor_inco_term_counts, plot n vs. vendor_inco_term 
ggplot(vendor_inco_term_counts, aes(vendor_inco_term, n)) +
  # Make it a (precalculated) bar plot
  geom_col() +
  # Add points from hypothesized 
  geom_point(data = hypothesized)

# Run chi-square goodness of fit test on vendor_inco_term
test_results <- late_shipments %>%
  chisq_test(
    response = vendor_inco_term,
    p = hypothesized_props
  )

# See the results
test_results