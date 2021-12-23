# 1. Searching for and replacing missing values

# Explore the strange missing values "N/A"
miss_scan_count(data = pacman, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = pacman, search = list("missing"))

# Explore the strange missing values "na"
miss_scan_count(data = pacman, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = pacman, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = pacman, search = list("N/A", "missing", "na", " "))

# Print the top of the pacman data using `head()`
head(pacman)

# Replace the strange missing values "N/A", "na", and  
# "missing" with `NA` for the variables, year, and score
pacman_clean <- replace_with_na(data = pacman, replace = list(year = c("N/A", "na", "missing"),
                                                              score = c("N/A", "na", "missing")))
# Test if `pacman_clean` still has these values in it?
miss_scan_count(data = pacman_clean, search = list("N/A", "na", "missing"))

# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,
                   .vars = c("year", "month", "day"), 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,
                   .predicate = is.character, 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, ~.x %in% c("N/A", "missing", "na", " "))

##############################################################################################################################

# 2. Filling down missing values

# Print the frogger data to have a look at it
frogger

# Use `complete()` on the `time` and `name` variables to  
# make implicit missing values explicit
frogger_tidy <- frogger %>% complete(time, name)

# Use `fill()` to fill down the name variable in the frogger dataset
frogger %>% fill(name)

# Correctly fill() and complete() missing values so that our dataset becomes sensible
frogger %>% 
  fill(name) %>%
  complete(name, time)

##############################################################################################################################

# 3. Missing Data dependence

# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys %>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()