library(lubridate)
library(ggplot2)
library(readr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(igraph)
library(ggraph)
library(rtweet)
library(dplyr)


data3<-read.csv("2019.csv")
data3$year<-matrix(2019, ncol = 1, nrow = 8312)
data3<-data3[ ,c(8,40)]
data4<-read.csv("2020.csv")
data4$year<-matrix(2020, ncol = 1, nrow = 11717)
data4<-data4[ ,c(8,40)]
data5<-read.csv("2021.csv")
data5$year<-matrix(2021, ncol = 1, nrow = 12339)
data5<-data5[ ,c(8,40)]

tweets<-rbind(data3,data4,data5)

text_df <- tibble(line=1, text=data_all$tweet)
text_df
data_all$tweet <-  gsub("https\\S*", "", data_all$tweet)
data_all$tweet <-  gsub("@\\S*", "", data_all$tweet) 
data_all$tweet  <-  gsub("amp", "", data_all$tweet) 
data_all$tweet  <-  gsub("[\r\n]", "", data_all$tweet)
data_all$tweet  <-  gsub("[[:punct:]]", "", data_all$tweet)


#Word frequency

words <- data_all %>%
  unnest_tokens(word, tweet) %>%
  count(year, word,sort=TRUE)

total_words <- words %>% 
  group_by(year) %>% 
  summarize(total = sum(n))

words <- left_join(words, total_words)

ggplot(words, aes(n/total, fill = year)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, ncol = 2, scales = "free_y")

# Zipf's law
freq_by_rank <- words %>% 
  group_by(year) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = year)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


book_tf_idf <- words %>%
  bind_tf_idf(word, year, n)


temp<-book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

library(forcats)

book_tf_idf %>%
  group_by(year) %>%
  slice_max(tf_idf, n = 20) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 3, scales = "free") +
  labs(x = "tf-idf", y = NULL)




ggplot(data_all, aes(n/total, fill = year)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, ncol = 2, scales = "free_y")




# bigrams

elon <- data_all %>%
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2)



# Counting bigrams
elon %>%
  count(bigram, sort = TRUE)

# bigrams with stop words
bigrams_separated <- elon %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# bigram as tf-idf
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Visualizing bigrams
library(igraph)
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(200)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()




