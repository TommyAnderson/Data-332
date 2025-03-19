#Complaint Analysis

## Introduction
This project analyzes consumer complaints using sentiment analysis in R. The analysis includes:

Data cleaning and preprocessing
Sentiment analysis using Bing and NRC lexicons
Word cloud visualization
Bar plots of sentiment distribution and top positive words . <br>

'''
data <- read.csv("Consumer_Complaints.csv", stringsAsFactors = FALSE)

data_clean <- data %>% drop_na(Consumer.complaint.narrative)

tokens <- data_clean %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% c("xxxx", "xx", "xxx"))) # Remove unwanted words
'''
