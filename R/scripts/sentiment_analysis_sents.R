# Sentiment analysis (sentence-level) ==========================================
# Setup ------------------------------------------------------------------------
# load packages
pacman::p_load(
  dplyr,
  ggplot2,
  lubridate,
  quanteda, 
  readr,
  stringr,
  tidyr,
  tidytext,
  vader
)

# load corpus
corpus <- readr::read_csv(here::here("data/vintages/corpus_sent_2023-12-02.csv")) |> # latest version (sentence-level)
  select(-body)

# Clean data -------------------------------------------------------------------
corpus_tidy <- corpus |> 
  mutate(
    sentence = str_trim(sentence),
    sentence = str_remove_all(sentence, "\\\\n"), # remove line breaks: "\\n"
    sentence = str_remove_all(sentence, "\\\""),
    #sentence = str_conv(sentence, "ASCII"),# remove accent marks to ensure comparability
    #sentence = str_remove_all(sentence, "�") # remove unknown characters 
  ) |> 
  filter(
    !is.na(sentence),
    !str_detect(sentence, "^\\w$"),
    !str_detect(sentence, "Mr|Ms|Dr|Gen|Col|Lt|Adm|com|co|The U|A U|Some U"),
    !str_detect(sentence, "Sign up"),
    !str_detect(sentence, "https://|theguardian"),
    !str_detect(sentence, "block-time published-time \\d"),
    !str_detect(sentence, "\\d{2}[am|pm] GMT"),
    !str_detect(sentence, "subscribe now"),
    !str_detect(sentence, "Brain teaser"),
    !str_detect(sentence, "If you value our reporting, please make"),
    !str_detect(sentence, "We also published the following articles recently"),
    !str_detect(sentence, "GAMES"),
    !str_detect(sentence, "Play"),
    !str_detect(sentence, "And finish your day"),
    !str_detect(sentence, "Prefer notifications?"),
    !str_detect(sentence, "Thanks for spending part of your morning"),
    !str_detect(sentence, "crossword"),
    !str_detect(sentence, "Audio produced by"),
    !str_detect(sentence, "Photograph: "),
    !sentence %in% c("Good morning", "Good afternoon", "See you tomorrow", "Until tomorrow"),
    sentence != "",
    sentence != "\\\"",
    sentence != "\\ +",
    sentence != "\'",
    sentence != "pic",
    sentence != "twitter",
    sentence != "''",
    sentence != "(",
    sentence != ")",
    sentence != "“",
    sentence != "”",
    sentence != "And start your day with a curated breakdown of the key stories you need to know",
    sentence != "Play Wordiply",
    sentence != "You have five goes to get the longest word including the starter word",
    sentence != "That’s it for today’s briefing",
    sentence != "We also published the following articles recently",
    sentence != "Send us your suggestions at",
    sentence != "Reach our team at",
    sentence != "-C",
    sentence != "(A full transcript of the episode is available",
    date >= as_date("2023-09-07"), # articles from up to 1 month before Oct 7
    date <= as_date("2023-11-07") # articles from up to 1 month after Oct 7
  ) |> 
  group_by(sentence) |> 
  filter(n() == 1) |> 
  ungroup() |> 
  select(newspaper, date, sentence) |> 
  mutate(sentence = str_to_lower(sentence)) |> 
  arrange(date)

# assign Palestine/Gaza or Israel label
corpus_labeled <- corpus_tidy |> 
  mutate(
    label = case_when(
      !str_detect(sentence, "palest\\w+|gaza\\w+|israel\\w+") ~ NA, # "both", # NA if no mention of Palestine (+Gaza) or Israel
      str_detect(sentence, "palest\\w+|gaza\\w+") &
        str_detect(sentence, "israel\\w+") ~ NA_character_, # NA if both are mentioned
      str_detect(sentence, "palest\\w+|gaza\\w+") ~ "palestine/gaza",
      str_detect(sentence, "israel\\w+") ~ "israel",
      TRUE ~ NA
    )
  ) |> 
  filter(!is.na(label))

# Create Document Feature Matrix -----------------------------------------------
# create dfm
dfmat <- corpus_labeled |> 
  corpus(text_field = "sentence") |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |> # remove punctuation and numbers
  #tokens_tolower() |> 
  tokens_remove(pattern = stopwords("en")) |> # remove English stopwords
  #tokens_wordstem() |> # stem tokens
  dfm() # convert to DFM

# inspect metadata 
docvars(dfmat) |> head()

# Sentiment analysis by media outlet -------------------------------------------
# start time 
tictoc::tic()

# calculate valence of sentences
sentiments <- vader_df(corpus_labeled$sentence)

# combine sentence with valence scores
sent_sentiment <- cbind(
  text = corpus_labeled$sentence, # text
  sentiments |> select(-text, -word_scores) # metadata
)

# most positive
pos <- sent_sentiment |> 
  arrange(desc(compound)) |> 
  head()

for (i in rownames(pos)) {
  print(pos[i, "text"])
  print(pos[i, "compound"])
}

# most negative
neg <- sent_sentiment |> 
  arrange(compound) |> 
  head()

for (i in rownames(neg)) {
  print(neg[i, "text"])
  print(neg[i, "compound"])
}

# add metadata: news source, date, label
sent_sentiment$newspaper <- corpus_labeled$newspaper
sent_sentiment$date <- as_date(corpus_labeled$date) 
sent_sentiment$label <- corpus_labeled$label

# calculate daily sentiment by news source
daily_sentiment <- sent_sentiment %>% 
  group_by(newspaper, date) %>%
  summarise(score = mean(compound)) %>%
  pivot_wider(names_from = newspaper, values_from = score)

# create df of days
days <- data.frame(
  date = seq(as_date("2023-09-07"), as_date("2023-11-07"), 1)
)

# reshape data
daily_sentiment <- daily_sentiment %>% 
  pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
  #group_by(newspaper) |> 
  arrange(date) 
#mutate(score_7day_avg = data.table::frollmean(score, 7))

# plot
daily_sentiment |> 
  filter(!newspaper %in% c("Die Welt (English)", "South China Morning Post", "The Straits Times (Singapore)")) |> 
  ggplot(aes(date, color = newspaper)) + 
  geom_point(aes(y = score)) + 
  geom_smooth(aes(y = score), se = FALSE) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "longdash") +
  geom_vline(xintercept = as_date("2023-10-07"), color = "red") + 
  annotate("text", x = as_date("2023-10-09"), y = .65, angle = 90, label = "Oct 7th", color = "red") +
  scale_x_date(limits = c(as_date("2023-09-07"), as_date("2023-11-07"))) +
  labs(
    title = "Avg. sentiment by media outlet",
    subtitle = "News coverage of Israel-Hamas War",
    caption = "Sources: Al Jazeera, Die Welt, The Guardian, The New York Times, South China Morning Post",
    x = NULL,
    y = "Sentiment Score",
    color = "Media Outlet"
  ) + 
  theme_minimal()

# end time 
tictoc::toc()

# Sentiment analysis by media outlet (coverage of Israel) ----------------------
# start time 
tictoc::tic()

# select label
corpus_isr <- corpus_labeled |> 
  filter(label == "israel")

# calculate valence of sentences
sentiments <- vader_df(corpus_isr$sentence)

# combine sentence with valence scores
sent_sentiment <- cbind(
  text = corpus_isr$sentence, # text
  sentiments |> select(-text, -word_scores) # metadata
)

# most positive
pos <- sent_sentiment |> 
  arrange(desc(compound)) |> 
  head()

for (i in rownames(pos)) {
  print(pos[i, "text"])
  print(pos[i, "compound"])
}

# most negative
neg <- sent_sentiment |> 
  arrange(compound) |> 
  head()

for (i in rownames(neg)) {
  print(neg[i, "text"])
  print(neg[i, "compound"])
}

# add metadata: news source, date, label
sent_sentiment$newspaper <- corpus_isr$newspaper
sent_sentiment$date <- as_date(corpus_isr$date) 
sent_sentiment$label <- corpus_isr$label

# calculate daily sentiment by news source
daily_sentiment <- sent_sentiment %>% 
  group_by(newspaper, date) %>%
  summarise(score = mean(compound)) %>%
  pivot_wider(names_from = newspaper, values_from = score)

# create df of days
days <- data.frame(
  date = seq(as_date("2023-09-07"), as_date("2023-11-07"), 1)
)

# reshape data
daily_sentiment <- daily_sentiment %>% 
  pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
  #group_by(newspaper) |> 
  arrange(date) 
#mutate(score_7day_avg = data.table::frollmean(score, 7))

# plot
daily_sentiment |> 
  filter(!newspaper %in% c("Die Welt (English)", "South China Morning Post", "The Straits Times (Singapore)")) |> 
  ggplot(aes(date, color = newspaper)) + 
  geom_point(aes(y = score)) + 
  geom_smooth(aes(y = score), se = FALSE) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "longdash") +
  geom_vline(xintercept = as_date("2023-10-07"), color = "red") + 
  annotate("text", x = as_date("2023-10-09"), y = .65, angle = 90, label = "Oct 7th", color = "red") +
  scale_x_date(limits = c(as_date("2023-09-07"), as_date("2023-11-07"))) +
  labs(
    title = "Avg. sentiment by media outlet",
    subtitle = "News coverage of Israel-Hamas War",
    caption = "Sources: Al Jazeera, Die Welt, The Guardian, The New York Times, South China Morning Post",
    x = NULL,
    y = "Sentiment Score",
    color = "Media Outlet"
  ) + 
  theme_minimal()

# end time 
tictoc::toc()

# Sentiment analysis by media outlet (coverage of Palestine/Gaza) --------------
# start time 
tictoc::tic()

# select label
corpus_pal <- corpus_labeled |> 
  filter(label == "palestine/gaza")

# calculate valence of sentences
sentiments <- vader_df(corpus_pal$sentence)

# combine sentence with valence scores
sent_sentiment <- cbind(
  text = corpus_pal$sentence, # text
  sentiments |> select(-text, -word_scores) # metadata
)

# most positive
pos <- sent_sentiment |> 
  arrange(desc(compound)) |> 
  head()

for (i in rownames(pos)) {
  print(pos[i, "text"])
  print(pos[i, "compound"])
}

# most negative
neg <- sent_sentiment |> 
  arrange(compound) |> 
  head()

for (i in rownames(neg)) {
  print(neg[i, "text"])
  print(neg[i, "compound"])
}

# add metadata: news source, date, label
sent_sentiment$newspaper <- corpus_pal$newspaper
sent_sentiment$date <- as_date(corpus_pal$date) 
sent_sentiment$label <- corpus_pal$label

# calculate daily sentiment by news source
daily_sentiment <- sent_sentiment %>% 
  group_by(newspaper, date) %>%
  summarise(score = mean(compound)) %>%
  pivot_wider(names_from = newspaper, values_from = score)

# create df of days
days <- data.frame(
  date = seq(as_date("2023-09-07"), as_date("2023-11-07"), 1)
)

# reshape data
daily_sentiment <- daily_sentiment %>% 
  pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
  #group_by(newspaper) |> 
  arrange(date) 
#mutate(score_7day_avg = data.table::frollmean(score, 7))

# plot
daily_sentiment |> 
  filter(!newspaper %in% c("Die Welt (English)", "South China Morning Post", "The Straits Times (Singapore)")) |> 
  ggplot(aes(date, color = newspaper)) + 
  geom_point(aes(y = score)) + 
  geom_smooth(aes(y = score), se = FALSE) + 
  geom_hline(yintercept = 0, color = "grey", linetype = "longdash") +
  geom_vline(xintercept = as_date("2023-10-07"), color = "red") + 
  annotate("text", x = as_date("2023-10-09"), y = .65, angle = 90, label = "Oct 7th", color = "red") +
  scale_x_date(limits = c(as_date("2023-09-07"), as_date("2023-11-07"))) +
  labs(
    title = "Avg. sentiment by media outlet",
    subtitle = "News coverage of Israel-Hamas War",
    caption = "Sources: Al Jazeera, Die Welt, The Guardian, The New York Times, South China Morning Post",
    x = NULL,
    y = "Sentiment Score",
    color = "Media Outlet"
  ) + 
  theme_minimal()

# end time 
tictoc::toc()