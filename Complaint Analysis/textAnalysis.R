library(tidyverse)
library(tidytext)
library(wordcloud2)
library(ggplot2)

rm(list = ls())
data <- read.csv("Consumer_Complaints.csv", stringsAsFactors = FALSE)

data_clean <- data %>% drop_na(Consumer.complaint.narrative)

tokens <- data_clean %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% c("xxxx", "xx", "xxx"))) # Remove unwanted words

bing_sentiment <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  arrange(desc(n))

nrc_sentiment <- tokens %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(sentiment) %>%
  arrange(desc(n))

word_freq <- tokens %>% count(word, sort = TRUE)
wordcloud2(word_freq[1:100, ])

bing_sentiment$sentiment <- factor(bing_sentiment$sentiment, levels = unique(bing_sentiment$sentiment))
nrc_sentiment$sentiment <- factor(nrc_sentiment$sentiment, levels = unique(nrc_sentiment$sentiment))

ggplot(bing_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("negative" = "red", "positive" = "blue")) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(title = "Positive vs Negative Words in Complaints",
       x = "Sentiment",
       y = "Word Count") +
  theme_minimal()

top_positive_words <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "positive") %>%
  count(word, sort = TRUE) %>%
  top_n(15, n)

ggplot(top_positive_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  
  labs(title = "Most Common Positive Words in Complaints",
       x = "Word",
       y = "Count") +
  theme_minimal()

