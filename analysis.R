# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/choiceDataClean.csv")
head(data)

# Estimate MNL model

# First create some dummy coded variables for categorical variables
data <- dummy_cols(data, c("Training_Program","Meal_Plan","Gamification","Counseling","Combined_Training_Session"))

# Clean up names of created variables
data <- clean_names(data)
head(data)

# Estimate the model
model <- logitr(
  data    = data,
  outcome = "choice",
  obsID   = "obs_id",
  pars = c(
    "price","training_program_yes",
    "meal_plan_yes","gamification_yes","counseling_yes","combined_training_session_yes")
)


# Get the model coefficients
coefs <- coef(model)
coefs

# Compute WTP estimates
wtp <- coefs / (-1*coefs['price'])

# Compute WTP with uncertainty:

# Get the model coefficients and covariance matrix
covariance <- vcov(model)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))

# Compute WTP for each coefficient draw
wtp_draws = -1*(coef_draws[,2:6] / coef_draws[,1])
head(wtp_draws)

# For each coefficient, get the mean and 95% confidence interval of WTP
wtp_ci <- ci(wtp_draws, level = 0.95)
wtp_ci



# Directly estimate WTP using a "WTP Space" model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(fastDummies)

options(dplyr.width = Inf) # So you can see all of the columns

# Load the data set:
data2 <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/choiceDataClean.csv")
head(data2)

# Create dummy coded variables
data_dummy2 <- dummy_cols(data2, c("Training_Program","Meal_Plan","Gamification","Counseling","Combined_Training_Session"))
head(data_dummy2)


# Estimate the model
mnl_wtp <- logitr(
  data    = data_dummy2,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("Training_Program_Yes", "Meal_Plan_Yes", "Gamification_Yes", "Counseling_Yes", "Combined_Training_Session_Yes"),
  scalePar = 'price', 
  numMultiStarts = 10 # Use a multi-start since log-likelihood is nonconvex
)

# View summary of results
summary(mnl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_wtp$hessian)$values

# Compare computed versus estimated WTP

wtpCompare(model, mnl_wtp, scalePar = 'price')



# Visualize results of estimated WTP space model

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)

# -----------------------------------------------------------------------------
# Get WTP estimates with 95% CI

# Method 1: Computed WTP from preference space model:

coefs <- coef(model)
covariance <- vcov(model)
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_draws = -1*(coef_draws[,2:6] / coef_draws[,1])
wtp_ci1 <- ci(wtp_draws, level = 0.95)
wtp_ci1

# Method 2: Estimate WTP in WTP space model:
coefs <- coef(mnl_wtp)
covariance <- vcov(mnl_wtp)
wtp_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
wtp_ci2 <- ci(wtp_draws, level = 0.95)
wtp_ci2 <- wtp_ci2[-1,] # Drop lambda (we won't plot this)
wtp_ci2

# -----------------------------------------------------------------------------
# Plot results

wtp_ci <- wtp_ci2

# Separate coefficient CIs by attribute 
wtp_ci$par <- row.names(wtp_ci)
wtp_training <- wtp_ci %>% filter(par == 'Training_Program_Yes')
wtp_meal <- wtp_ci %>% filter(par == 'Meal_Plan_Yes')
wtp_game <- wtp_ci %>% filter(par == 'Gamification_Yes')
wtp_counsel <- wtp_ci %>% filter(par == 'Counseling_Yes')
wtp_ctrain <- wtp_ci %>% filter(par == 'Combined_Training_Session_Yes')


library(ggplot2)
library(dplyr)

# Assuming wtp_ci has been prepared as described
# Combine the separate WTP data frames into one
wtp_combined <- bind_rows(wtp_training, wtp_meal, wtp_game, wtp_counsel, wtp_ctrain)


# Create the tornado chart with blue color aesthetics
ggplot(wtp_combined, aes(x = par, y = mean)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "darkblue") +
  geom_point(size = 3, color = "lightblue") +
  coord_flip() +  # Flip coordinates to make the chart horizontal
  labs(x = "Attribute", y = "WTP Estimate", title = "WTP for product attributes with simulated 95% confidence intervals") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Adjust text alignment


# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)

df_training <- data.frame(level = c("Yes", "No")) %>%
  mutate(
    mean  = c(0, wtp_training$mean),
    lower = c(0, wtp_training$lower),
    upper = c(0, wtp_training$upper))

df_meal <- data.frame(level = c("Yes", "No")) %>%
  mutate(
    mean  = c(0, wtp_meal$mean),
    lower = c(0, wtp_meal$lower),
    upper = c(0, wtp_meal$upper))

df_game <- data.frame(level = c("Yes", "No")) %>%
  mutate(
    mean  = c(0, wtp_game$mean),
    lower = c(0, wtp_game$lower),
    upper = c(0, wtp_game$upper))

df_counsel <- data.frame(level = c("Yes", "No")) %>%
  mutate(
    mean  = c(0, wtp_counsel$mean),
    lower = c(0, wtp_counsel$lower),
    upper = c(0, wtp_counsel$upper))

df_ctrain <- data.frame(level = c("Yes", "No")) %>%
  mutate(
    mean  = c(0, wtp_ctrain$mean),
    lower = c(0, wtp_ctrain$lower),
    upper = c(0, wtp_ctrain$upper))



# Define a function to create a plot for WTP estimates
create_wtp_plot <- function(wtp_ci, title) {
  ci_data <- do.call(rbind, lapply(names(wtp_ci), function(attr) {
    data.frame(
      Attribute = attr,
      Lower = wtp_ci[[attr]][1],
      Upper = wtp_ci[[attr]][2],
      Mean = mean(c(wtp_ci[[attr]][1], wtp_ci[[attr]][2]))
    )
  }))
  
  ggplot(ci_data, aes(x = Attribute, y = Mean)) +
    geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
    coord_flip() +
    labs(title = title, x = "Attribute", y = "WTP Estimate") +
    theme_minimal()
}

# Create plots
plot_pref_space <- create_wtp_plot(wtp_ci1, "WTP Estimates - Preference Space Model")
plot_wtp_space <- create_wtp_plot(wtp_ci2, "WTP Estimates - WTP Space Model")

# Display plots
print(plot_pref_space)
print(plot_wtp_space)


library(ggplot2)
library(gridExtra)

# Assuming 'create_wtp_plot' function returns a ggplot object
# and wtp_ci1, wtp_ci2 are your data frames with WTP estimates

# Create the plots
plot_pref_space <- create_wtp_plot(wtp_ci1, "WTP Estimates - Preference Space Model")
plot_wtp_space <- create_wtp_plot(wtp_ci2, "WTP Estimates - WTP Space Model")


library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming wtp_ci1 and wtp_ci2 are data frames as described

# Add a new column for attribute names and convert to long format
wtp_long1 <- wtp_ci1 %>%
  rownames_to_column(var = "Attribute") %>%
  gather(key = "Estimate", value = "Value", -Attribute) %>%
  mutate(Model = "Preference Space")

wtp_long2 <- wtp_ci2 %>%
  rownames_to_column(var = "Attribute") %>%
  gather(key = "Estimate", value = "Value", -Attribute) %>%
  mutate(Model = "WTP Space")

# Combine the two datasets
wtp_long <- bind_rows(wtp_long1, wtp_long2)

# Ensure 'Estimate' is a factor and in the correct order
wtp_long$Estimate <- factor(wtp_long$Estimate, levels = c("lower", "mean", "upper"))

# Create the comparison plot
ggplot(wtp_long, aes(x = Estimate, y = Value, color = Model, group = Model)) +
  geom_line() +
  facet_wrap(~Attribute, scales = "free_y") +
  theme_minimal() +
  labs(title = "Comparison of WTP Estimates", x = "Estimate Type", y = "Value") +
  theme(legend.position = "bottom")



# Compute expected probabilities of different alternatives

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)


# -----------------------------------------------------------------------------
# Single market simulation using the linear model

summary(model)

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
  altID       = c(1, 2, 3), 
  obsID       = c(1, 1, 1),
  price       = c(15, 25, 20),
  training_program_yes = c(0, 0, 1),
  training_program_no = c(1, 1, 0),
  meal_plan_yes = c(1, 1, 0),
  meal_plan_no = c(0, 0, 1),
  gamification_yes = c(1, 1, 1),
  gamification_no = c(0, 0, 0),
  counseling_yes = c(1,0,0),
  counseling_no = c(0,1,1),
  combined_training_session_yes = c(0, 1, 1),
  combined_training_session_no = c(1, 0, 0)
)

model

# Columns are attributes, rows are alternatives
baseline

# Use the predict() function to compute the probabilities
sim_mnl_linear <- predict(
  model,
  newdata = baseline, 
  obsID = 'obsID', 
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE # This returns your data along with predicted values
)

sim_mnl_linear

# Plot market simulation results

# Load libraries & functions
library(tidyverse)
library(here)


# Bar plot of probabilities for single market simulation (with 95% CI) 
sim_mnl_linear %>% 
  mutate(label = c("$15,meals, gamification, counseling", "$25, meals, gamification, combined", "$20,training, gamification, combined")) %>% 
  ggplot(aes(
    x = label, y = predicted_prob, 
    ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
  geom_col(fill = "#ADD8E6", width = 0.6) +
  geom_errorbar(width = 0.3) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(x = 'Alternative', y = 'Market Share') +
  theme_bw()


# Measure the sensitivity of the market simulation outcome for one product to
# changes in attribute levels

# Load libraries & functions
library(tidyverse)
library(here)
library(logitr)
library(jph)


# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in *price*

# Create a set of alternatives for which to simulate shares
baseline <- data.frame(
  altID       = c(1, 2, 3), 
  obsID       = c(1, 1, 1),
  price       = c(15, 25, 20),
  training_program_yes = c(0, 0, 1),
  training_program_no = c(1, 1, 0),
  meal_plan_yes = c(1, 1, 0),
  meal_plan_no = c(0, 0, 1),
  gamification_yes = c(1, 1, 1),
  gamification_no = c(0, 0, 0),
  counseling_yes = c(1,0,0),
  counseling_no = c(0,1,1),
  combined_training_session_yes = c(0, 1, 1),
  combined_training_session_no = c(1, 0, 0)
)

baseline 

# Define the sensitivity cases
# For this case, let's see how the market share for the Electric Vehicle 
# (option 2) changes with different EV prices. That is, I'm holding everything
# the same in every simulation except the price for the EV

prices <- seq(10, 30) # Define sensitivity price levels
n <- length(prices) # Number of simulations (21)
scenarios_price <- rep_df(baseline, n) # Repeat the baseline data frame n times
scenarios_price$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Set the price for each scenario
scenarios_price$price[which(scenarios_price$altID == 3)] <- prices 
head(scenarios_price)

# For each case, simulate the market share predictions
sens_price <- predict(
  model,
  newdata = scenarios_price, 
  obsID = 'obsID', 
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE) %>%
  # Keep only EV alternative
  filter(altID == 3) %>% 
  # Keep only prices and predictions
  select(price, starts_with("predicted_")) 

sens_price
# The probability shifts from essentially 100% of the market share at 
# a price of $10,000 to 0% at $30,000

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in multiple attributes

# For these cases, we'll look at how the market share for the Electric Vehicle 
# (option 2) changes with +/- 20% changes price, fuel economy, & acceleration time

# "high" means they result in higher market shares
# "low"  means they result in lower market shares
cases <- tribble(
  ~obsID, ~altID, ~attribute,    ~case,  ~value,
  2,      3,     'price',       'high',  25*0.8,
  3,      3,     'price',       'low',   25*1.2,

)

cases

# Define scenarios
n <- 7 # baseline + high & low for each attribute
scenarios_atts <- rep_df(baseline, n) 
scenarios_atts$obsID <- rep(seq(n), each = 3) # Reset obsIDs

# Replace scenarios with case values 
scenarios_atts <- scenarios_atts %>% 
  left_join(cases, by = c("altID", "obsID")) %>% 
  mutate(
    attribute = ifelse(is.na(attribute), "other", attribute),
    case = ifelse(is.na(case), "base", case),
    price = ifelse(attribute == 'price', value, price)
  )

scenarios_atts

# For each case, simulate the market share predictions
sens_atts <- predict(
  model,
  newdata = scenarios_atts, 
  obsID = 'obsID', 
  level = 0.95,
  interval = 'confidence',
  returnData = TRUE) %>%
  # Keep only EV alternative
  filter(altID == 2) %>% 
  # Keep only attributes and predictions
  select(attribute, case, value, predicted_prob)

sens_atts




# Plot sensitivity simulation results

# Load libraries & functions
library(tidyverse)
library(here)


# -----------------------------------------------------------------------------
# Make a line plot of the market sensitivity to price (with uncertainty)

share_price_plot <- 
  sens_price %>% 
  ggplot(aes(
    x = price, y = predicted_prob, 
    ymin = predicted_prob_lower, ymax = predicted_prob_upper)) +
  geom_ribbon(alpha = 0.2) +
  # Use a dashed line for the full range of prices
  geom_line(linetype = "dashed") +
  # Overlay solid line for range of prices included in survey
  geom_line(
    data = sens_price %>% filter(price <= 25, price >= 15), 
    linetype = "solid") +
  expand_limits(x = c(10, 30), y = c(0, 1)) +
  labs(x = 'Price in Dollars', y = 'Market Share') +
  theme_bw()

share_price_plot

# -----------------------------------------------------------------------------
# Make a line plot of the revenue sensitivity to price (with uncertainty)

marketSize <- 1000
rev_data <- sens_price %>%
  mutate(
    rev_mean = price*marketSize*predicted_prob / 10^3, 
    rev_low  = price*marketSize*predicted_prob_lower / 10^3,
    rev_high = price*marketSize*predicted_prob_upper / 10^3)

rev_price_plot <- rev_data %>% 
  ggplot(aes(x = price, y = rev_mean, ymin = rev_low, ymax = rev_high)) +
  geom_ribbon(alpha = 0.2) +
  # Use a dashed line for the full range of prices
  geom_line(linetype = "dashed") +
  # Overlay solid line for range of prices included in survey
  geom_line(
    data = rev_data %>% filter(price <= 25, price >= 15), 
    linetype = "solid") +
  # expand_limits(x = c(10, 30), y = c(0, 1)) +
  labs(x = 'Price', y = 'Revenue/month ($ thousands)') +
  theme_bw()

rev_price_plot

# -----------------------------------------------------------------------------
 





