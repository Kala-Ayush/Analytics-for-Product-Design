# Load libraries
library(fastDummies)
library(here)
library(lubridate)
library(tidyverse)

# Change dplyr settings so I can view all columns 
options(dplyr.width = Inf)

# Import raw data
p1 <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/FutStat_Final_P1.csv")
p2 <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/FutStat_Final_P2.csv")
p3 <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/FutStat_Final_P3.csv")



# Format and join the three surveys -------

# Variables common to each survey: 
# session  = a unique ID for the Run - should be the same across all surveys
# created  = time stamp when survey was started
# modified = time stamp when survey was last modified by respondent
# ended    = time stamp when survey ended
# expired  = time stamp when survey expired (if respondent didn't reach end)

# Compute time values for each part
p1 <- p1 %>% 
  mutate(
    created = ymd_hms(created, tz = "EST"),
    ended =  ymd_hms(ended, tz = "EST"),
    time_sec_p1 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p1, FilterQuestion1,CriticalInfoQ1,CriticalInfoQ3,CriticalInfoQ4,CriticalInfoQ5,CriticalInfoQ6,CriticalInfoQ7)


p2 <- p2 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p2 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns 
  select(session, time_sec_p2, respondentID, starts_with("cbc"), cbcAllSame)

p3 <- p3 %>% 
  mutate(
    created = ymd_hms(created),
    ended =  ymd_hms(ended),
    time_sec_p3 = as.numeric(ended - created, units = "secs")) %>%
  # Select important columns
  select(session, time_sec_p3, ConQ1, ConQ2, ConQ3, ConQ4 , ConQ5, feedback, CompletionCode)

# Join all parts together using the session variable
data <- p1 %>% 
  left_join(p2, by = "session") %>% 
  left_join(p3, by = "session") %>% 
  # No longer need session variable
  select(-session)
head(data)


# Filter out bad responses ---------

nrow(data)

# Drop people who got screened out
#data <- data %>% 
#filter(!is.na(screenout), screenout == 2)
#nrow(data)

# Drop anyone who didn't complete all choice questions
data <- data %>% 
  filter(!is.na(cbc1)) %>% 
  filter(!is.na(cbc2)) %>% 
  filter(!is.na(cbc3)) %>% 
  filter(!is.na(cbc4)) %>% 
  filter(!is.na(cbc5)) %>% 
  filter(!is.na(cbc6)) %>% 
  filter(!is.na(cbc7)) %>% 
  filter(!is.na(cbc8))
nrow(data)

# Drop respondents who went too fast
data <- data %>% 
  mutate(
    # First replace NA values with 0 seconds
    time_sec_p1 = ifelse(is.na(time_sec_p1), 0, time_sec_p1),
    time_sec_p2 = ifelse(is.na(time_sec_p2), 0, time_sec_p2),
    time_sec_p3 = ifelse(is.na(time_sec_p3), 0, time_sec_p3),
    # Now compute the total time
    time_min_total = (time_sec_p1 + time_sec_p2 + time_sec_p3) / 60
  )
# Look at summary of completion times
summary(data$time_min_total)
# Drop anyone who finished in under the 10th percentile of completion times
time_10 <- quantile(data$time_min_total, 0.1)
nrow(data)
data <- data %>% 
  filter(time_min_total >= time_10)
nrow(data)

# Drop respondents that got the attention check question wrong
data <- data %>% 
  filter(FilterQuestion1 == 2)
nrow(data)


# Create choice data ---------

# First convert the data to long format
choiceData <- data %>% 
  pivot_longer(
    cols = cbc1:cbc8,
    names_to = "qID",
    values_to = "choice") %>% 
  # Convert the qID variable to a number
  mutate(qID = parse_number(qID))

head(choiceData)
nrow(choiceData)

# Read in choice questions and join it to the choiceData
survey <- read_csv("https://raw.githubusercontent.com/sumedhjoshi01/MarketingAnalytics/main/FUTSTAT_choice_questionsNew.csv")
choiceData <- choiceData %>% 
  rename(respID = respondentID) %>% 
  left_join(survey, by = c("respID", "qID"))

# Convert choice column to 1 or 0 based on if the alternative was chosen 
choiceData <- choiceData %>% 
  mutate(choice = ifelse(choice == altID, 1, 0)) %>% 
  # Drop unused variables
  select(-FilterQuestion1, -cbcAllSame)

head(choiceData)

# Create new values for respID & obsID
nRespondents <- nrow(data)
nAlts <- max(survey$altID)
nQuestions <- max(survey$qID)
choiceData$respID <- rep(seq(nRespondents), each = nAlts*nQuestions)
choiceData$obsID <- rep(seq(nRespondents*nQuestions), each = nAlts)

nRespondents
nrow(data)

# Reorder columns - it's nice to have the "ID" variables first
choiceData <- choiceData %>% 
  select(ends_with("ID"), "choice", everything())

head(choiceData)

choiceData %>% count(price)
choiceData %>% count(Training_Program)
choiceData %>% count(Meal_Plan)
choiceData %>% count(Gamification)
choiceData %>% count(Combined_Training_Session)


# Visualize how many times each price level was chosen:
choiceData %>% count(price, choice)
choiceData %>%
  ggplot() + 
  geom_col(aes(x = as.factor(price), y = choice))

# Visualize how many times electric cars were chosen compared to gasoline cars:
library(ggplot2)
library(dplyr)
library(tidyr)

# Convert numeric columns to character for consistent data types
choiceData <- choiceData %>%
  mutate(across(c("price"), as.character))

# Counting the number of times each level was chosen for each attribute
attribute_counts <- choiceData %>%
  pivot_longer(cols = c("Training_Program", "Meal_Plan", "Gamification", "Counseling", "Combined_Training_Session", "price"), 
               names_to = "attribute", values_to = "level") %>%
  count(attribute, level, choice) %>%
  filter(choice == 1)  # Considering only choices where the option was chosen

# Define custom colors
custom_colors <- c("Yes" = "lightblue", "No" = "darkblue", "10" = "lightgreen", "20" = "mediumseagreen", "30" = "darkgreen")

# Plotting
ggplot(attribute_counts, aes(x = level, y = n, fill = level)) +
  geom_col() +
  facet_wrap(~ attribute, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = custom_colors) +
  labs(x = "Level", y = "Count", fill = "Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save cleaned data for modeling
#write_csv(choiceData, here("data", "choiceDataClean.csv"))

# Number of respondents that chose each option for the critical 
# respondent questions and demographic questions respectively 

library(dplyr)
library(knitr)
library(tidyr)

# Define the mapping of questions to their descriptions as a named vector
question_map <- c(
  CriticalInfoQ1 = "1. Age: Which age group do you belong to?",
  CriticalInfoQ3 = "2. Soccer Skill Level: How would you describe your soccer skill level?",
  CriticalInfoQ4 = "3. How often do you play soccer?",
  CriticalInfoQ5 = "4. What position(s) do you usually play or are interested in getting better at?",
  CriticalInfoQ6 = "5. What is the primary purpose for using our soccer training app?",
  CriticalInfoQ7 = "6. Current Soccer apps: Do you currently use any soccer training related apps?",
  ConQ1 = "1. How do you identify your gender?",
  ConQ2 = "2. What is your monthly expenditure on mobile apps?",
  ConQ3 = "3. How many hours per day, on average, do you spend using learning apps on your phone?",
  ConQ4 = "4. Which type of device do you use?",
  ConQ5 = "5. What is your annual household income (from all sources) before taxes and other deductions from pay?"
)

# Function to summarize and transform into a more readable format
summarize_and_transform <- function(choiceData, prefix) {
  choiceData %>%
    summarise(across(starts_with(prefix), ~ list(table(.)))) %>%
    pivot_longer(cols = everything(), names_to = "Question", values_to = "Distribution") %>%
    unnest(Distribution) %>%
    mutate(Question = gsub(prefix, "", Question)) %>%
    group_by(Question) %>%
    mutate(Description = question_map[Question]) %>%
    summarise(Answer = names(Distribution), Count = Distribution, .groups = 'drop')
}

# Summarize critical information questions
critical_info_summary <- summarize_and_transform(choiceData, "CriticalInfoQ")

# Summarize demographic questions
demographic_summary <- summarize_and_transform(choiceData, "ConQ")

# Combine summaries
summary_table <- bind_rows(critical_info_summary, demographic_summary)

# Use kable() for a well-formatted table
kable(summary_table, caption = "Summary of Critical Information and Demographic Questions")



