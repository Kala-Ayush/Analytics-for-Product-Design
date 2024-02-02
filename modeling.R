# Estimate multinomial logit (MNL) models

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(janitor)
library(here)
library(cbcTools)
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
# View summary of results
summary(model)

# Check the 1st order condition: Is the gradient at the solution zero?
model$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model$hessian)$values

# Get the model coefficients
coefs <- coef(model)
coefs


profiles <- cbc_profiles(
  price       = c(10,20,30),
  training = c('yes','no'),
  meal_plan = c('yes', 'no'), 
  gamification   = c('yes', 'no'),
  counseling  = c('yes', 'no'),
  combined_training = c('yes', 'no')
)

design_rand <- cbc_design(
  profiles = profiles,
  n_resp   = 160, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 8    # Number of questions per respondent
)

head(design_rand)

cbc_balance(design_rand)
cbc_overlap(design_rand)


data_rand <- cbc_choices(
  design = design_rand,
  obsID = "obsID"
)

power_rand <- cbc_power(
  nbreaks = 10,
  n_q     = 8,
  n_cores = 12,
  data    = data_rand,
  pars    = c( 'price',
               'training' ,
               'meal_plan', 
               'gamification',
               'counseling' ,
               'combined_training'),
  outcome = "choice",
  obsID   = "obsID"
)

head(power_rand)
tail(power_rand)

ggplot(power_rand) +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(aes(x = sampleSize, y = se, color = coef)) +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0, 0.125)) +
  theme_bw() + 
  labs(
    x = "Sample size", 
    y = "Standard error", 
    color = "Coefficient"
  )

priors <- list(
  price = -0.1,
  training = 0.2,
  meal_plan = 0.2,
  gamification = 0.1,
  counseling = 0.1,
  combined_training = 0.1
)

design_db_eff <- cbc_design(
  profiles  = profiles,
  n_resp    = 160, # Number of respondents
  n_alts    = 3,   # Number of alternatives per question
  n_q       = 8,   # Number of questions per respondent
  n_start   = 10, 
  n_blocks  = 1,
  priors = priors
)

head(design_db_eff)


data_db_eff <- cbc_choices(
  design = design_db_eff,
  obsID  = "obsID",
  priors = priors
)

head(data_db_eff)

cbc_balance(design_db_eff)
cbc_overlap(design_db_eff)

power_db_eff <- cbc_power(
  nbreaks = 10,
  n_q     = 8,
  n_cores = 12,
  data    = data_db_eff,
  pars    = c( 'price',
               'training' ,
               'meal_plan', 
               'gamification',
               'counseling' ,
               'combined_training'),
  outcome = "choice",
  obsID   = "obsID"
)

head(power_db_eff)
tail(power_db_eff)

ggplot(power_db_eff) +
  geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
  geom_point(aes(x = sampleSize, y = se, color = coef)) +
  expand_limits(y = 0) +
  scale_y_continuous(limits = c(0, 0.125)) +
  theme_bw() + 
  labs(
    x = "Sample size", 
    y = "Standard error", 
    color = "Coefficient"
  )

# Estimate mixed logit (MXL) models

# Load libraries
library(logitr)
library(tidyverse)
library(here)

options(dplyr.width = Inf) # So you can see all of the columns

# -----------------------------------------------------------------------------
# Load the data set:
data_mix <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/choiceDataClean.csv")
head(data_mix)

# Variables:
# "respID"      = Identifies each survey respondent
# "qID"         = Identifies each question for each survey respondent
# "altID"       = Identifies the alternative in each unique choice observation
# "obsID"       = Identifies each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas

# -----------------------------------------------------------------------------
# Estimate preference space MXL model with linear price, fuelEconomy, and accelTime

# Create dummy coefficients for powertrain
data_dummy <- fastDummies::dummy_cols(data_mix, c("Training_Program","Meal_Plan","Gamification","Counseling","Combined_Training_Session"))
head(data_dummy)

# Estimate the model
mxl_pref <- logitr(
  data    = data_dummy,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price","Training_Program_Yes",
              "Meal_Plan_Yes","Gamification_Yes","Counseling_Yes","Combined_Training_Session_Yes"),
  randPars = c(Training_Program_Yes = 'n',
               Meal_Plan_Yes = 'n', Gamification_Yes = 'n', Counseling_Yes = 'n', Combined_Training_Session_Yes = 'n')
)

# View summary of results
summary(mxl_pref)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_pref$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_pref$hessian)$values

# -----------------------------------------------------------------------------
# Estimate WTP space MXL model with linear price, fuelEconomy, and accelTime

# Estimate the model
mxl_wtp <- logitr(
  data    = data_dummy,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("Training_Program_Yes",
              "Meal_Plan_Yes","Gamification_Yes","Counseling_Yes","Combined_Training_Session_Yes"),
  scalePar = 'price',
  randPars = c(Training_Program_Yes = 'n',
               Meal_Plan_Yes = 'n', Gamification_Yes = 'n', Counseling_Yes = 'n', Combined_Training_Session_Yes = 'n')
)

# View summary of results
summary(mxl_wtp)

# Check the 1st order condition: Is the gradient at the solution zero?
mxl_wtp$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mxl_wtp$hessian)$values

# Load libraries
library(logitr)
library(tidyverse)
library(fastDummies)
library(here)
remotes::install_github("jhelvy/jph")
library(jph)


# -----------------------------------------------------------------------------
# Load the data set:
data_group <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/choiceDataClean.csv")
head(data_group)

# Calculate the midpoint
midpoint <- nrow(data_group) / 2

# Create the 'group' column
data_group$group <- ifelse(seq_len(nrow(data_group)) <= midpoint, "A", "B")

head(data_group)

# Create dummy coefficients for the group and powertrain variables
data_dummy <- fastDummies::dummy_cols(data_group, c("Training_Program","Meal_Plan","Gamification","Counseling","Combined_Training_Session", "group"))
head(data_dummy)

# Create interactions of each variable with group_B
data_dummy <- data_dummy %>%
  mutate(
    price_B       = price*group_B,
    Training_Program_Yes_B = Training_Program_Yes*group_B,
    Meal_Plan_Yes_B   = Meal_Plan_Yes*group_B,
    Gamification_Yes_B = Gamification_Yes*group_B,
    Counseling_Yes_B = Counseling_Yes*group_B,
    Combined_Training_Session_Yes_B = Combined_Training_Session_Yes*group_B
  )
head(data_dummy)

# Estimate the model
mnl_groups <- logitr(
  data    = data_dummy,
  outcome = "choice",
  obsID   = "obsID",
  pars = c(
    "price","Training_Program_Yes",
    "Meal_Plan_Yes","Gamification_Yes",
    "Counseling_Yes","Combined_Training_Session_Yes",
    'price_B',
    'Training_Program_Yes_B',
    'Meal_Plan_Yes_B',
    'Gamification_Yes_B',
    'Counseling_Yes_B',
    'Combined_Training_Session_Yes_B'
  )
)

# View summary of results
summary(mnl_groups)

# Check the 1st order condition: Is the gradient at the solution zero?
mnl_groups$gradient

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(mnl_groups$hessian)$values

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

# Get the model coefficients and covariance matrix
coefs <- coef(mnl_groups)
covariance <- vcov(mnl_groups)

# Take 10,000 draws of the coefficients
coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
coef_draws_A <- coef_draws %>%
  select(price,Training_Program_Yes,
         Meal_Plan_Yes,Gamification_Yes,
         Counseling_Yes,Combined_Training_Session_Yes)

coef_draws_B <- coef_draws %>%
  mutate(
    price       = price + price_B,
    Training_Program_Yes = Training_Program_Yes + Training_Program_Yes_B,
    Meal_Plan_Yes   = Meal_Plan_Yes + Meal_Plan_Yes_B,
    Gamification_Yes = Gamification_Yes + Gamification_Yes_B,
    Counseling_Yes = Counseling_Yes + Counseling_Yes_B,
    Combined_Training_Session_Yes = Combined_Training_Session_Yes + Combined_Training_Session_Yes_B) %>%
  select(price,Training_Program_Yes,
         Meal_Plan_Yes,Gamification_Yes,
         Counseling_Yes,Combined_Training_Session_Yes)

colMeans(coef_draws_A)
colMeans(coef_draws_B)

# -----------------------------------------------------------------------------
# Compute WTP for each group

wtp_A <- coef_draws_A / (-1* coef_draws_A$price)
wtp_B <- coef_draws_B / (-1* coef_draws_B$price)
ci(wtp_A, level = 0.95)
ci(wtp_B, level = 0.95)



# Assuming ci() function calculates confidence intervals and returns a dataframe with 'lower' and 'upper' bounds
ci_A <- ci(wtp_A, level = 0.95)
ci_B <- ci(wtp_B, level = 0.95)

# Calculate means for WTP
mean_A <- colMeans(wtp_A)
mean_B <- colMeans(wtp_B)

# Convert the means and CIs into data frames
df_A <- data.frame(attribute = names(mean_A), mean = mean_A, ci_lower = ci_A$lower, ci_upper = ci_A$upper, Group = 'A')
df_B <- data.frame(attribute = names(mean_B), mean = mean_B, ci_lower = ci_B$lower, ci_upper = ci_B$upper, Group = 'B')

# Combine data frames
df_combined <- rbind(df_A, df_B)

# Create the plot
ggplot(df_combined, aes(x = attribute, y = mean, ymin = ci_lower, ymax = ci_upper, color = Group)) +
  geom_pointrange() +
  facet_wrap(~ Group, scales = "free") +
  theme_minimal() +
  labs(title = "Comparison of WTP Estimates with Confidence Intervals",
       x = "Attribute",
       y = "WTP Estimate") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

