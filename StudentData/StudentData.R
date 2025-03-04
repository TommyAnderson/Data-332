# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

rm(list = ls())

setwd("C:/Users/tomma/Data332/")

# Load the datasets
course <- read_excel("Course.xlsx", .name_repair = 'universal')
registration <- read_excel("Registration.xlsx", .name_repair = 'universal')
student <- read_excel("Student.xlsx", .name_repair = 'universal')

# Clear the environment
rm(list = ls())

# Set working directory
setwd("C:/Users/tomma/Data332/")

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load data
course <- read_excel("Course.xlsx", .name_repair = 'universal')
registration <- read_excel("Registration.xlsx", .name_repair = 'universal')
student <- read_excel("Student.xlsx", .name_repair = 'universal')

# Merge data
merged_data <- registration %>%
  left_join(course, by = "Instance.ID") %>%
  left_join(student, by = "Student.ID")

# Get birth year
merged_data <- merged_data %>%
  mutate(Birth.Year = as.numeric(format(as.Date(Birth.Date), "%Y")))

# Chart on the number of majors (TITLE) 
ggplot(merged_data, aes(x = Title)) +
  geom_bar(fill = "blue") +
  labs(title = "Number of Students per Major", x = "Major", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chart on the birth year of the student
ggplot(merged_data, aes(x = Birth.Year)) +
  geom_histogram(binwidth = 2, fill = "darkgreen", color = "black") +
  labs(title = "Student Birth Years", x = "Birth Year", y = "Count")

# Total cost per major, segment by payment plan
total_cost <- merged_data %>%
  group_by(Title, Payment.Plan) %>%
  summarise(Total.Cost = sum(Total.Cost, na.rm = TRUE))

ggplot(total_cost, aes(x = Title, y = Total.Cost / 1000, fill = as.factor(Payment.Plan))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::label_number(suffix = "k")) +
  labs(title = "Total Cost per Major", x = "Major", y = "Total Cost", fill = "Payment Plan") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total balance due by major, segment by payment plan
total_balance <- merged_data %>%
  group_by(Title, Payment.Plan) %>%
  summarise(Total.Balance.Due = sum(Balance.Due, na.rm = TRUE))

ggplot(total_balance, aes(x = Title, y = Total.Balance.Due, fill = as.factor(Payment.Plan))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Balance Due per Major", x = "Major", y = "Total Balance Due", fill = "Payment Plan") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

