# Complaint Analysis

## Introduction
This project analyzes consumer complaints using sentiment analysis in R. The analysis includes:

-Data cleaning and preprocessing

-Sentiment analysis using Bing and NRC lexicons

-Word cloud visualization

-Bar plots of sentiment distribution and top positive words . <br>

## Data Collecting, Cleaning, and Tokenization
```
data <- read.csv("Consumer_Complaints.csv", stringsAsFactors = FALSE)

data_clean <- data %>% drop_na(Consumer.complaint.narrative)

tokens <- data_clean %>%
  unnest_tokens(word, Consumer.complaint.narrative) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!(word %in% c("xxxx", "xx", "xxx"))) # Remove unwanted words
```
This code reads the complaint data from a CSV file, removes any rows with missing complaint narratives, and tokenizes the complaint text. The unnest_tokens() function splits the complaint text into individual words, and censored words, like "xxxx" and "xx" are filtered out to focus on relevant terms.

## Sentiment Analysis
```
bing_sentiment <- tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  arrange(desc(n))

nrc_sentiment <- tokens %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  count(sentiment) %>%
  arrange(desc(n))
```
This code performs sentiment analysis on the tokenized words using the Bing and NRC lexicons. It joins the tokens with the sentiment lexicons, counts the occurrences of each sentiment, and arranges the results by frequency. The bing_sentiment and nrc_sentiment data frames show the most common sentiment categories in the complaints.

## Word Frequncy Factoring With Word Cloud
```
word_freq <- tokens %>% count(word, sort = TRUE)
wordcloud2(word_freq[1:100, ])
```
This part of the code calculates the frequency of each word in the tokenized complaints. It then generates an interactive word cloud to visually represent the top 100 most frequent words, providing a quick overview of common terms mentioned in the complaints.


## Plotting Negative vs Positive Words
```
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
```
This code visualizes the distribution of positive and negative words in the complaints by creating a bar plot. It sets custom colors for the positive and negative sentiments and labels the y-axis with human-readable numbers. The plot provides a clear comparison of sentiment occurrences in the complaint data.

## Finding Positive Word Frequency
```
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

```
This code identifies the top 15 most frequent positive words in the complaints. It filters the bing_sentiment data for positive sentiments, counts the frequency of each positive word, and creates a horizontal bar plot. This visualization helps to highlight the positive aspects that consumers are mentioning in their complaints.

## Results and Predictions
The sentiment analysis identified several positive words, such as refund, protection, and recovery, which, upon closer examination, appear out of context in the complaints dataset. These words, while typically positive, are used in this case to describe issues or complaints, such as problems with refunds, lack of protection, or the need for recovery. A word that is generally positive may not always convey a positive sentiment in every scenario. Therefore, further refinement of the bing sentiment analysis approach may be necessary to account for context and improve the accuracy of sentiment classification in this dataset.









