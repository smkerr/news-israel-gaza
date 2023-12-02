# Sentiment analysis (article-level) ==========================================
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
corpus <- readr::read_csv(here::here("data/vintages/corpus_filtered_2023-12-02.csv")) # latest version (article-level)

# Clean data -------------------------------------------------------------------
corpus_tidy <- corpus |> 
  mutate(
    body = str_trim(body),
    body = str_remove_all(body, "\\\\n"), # remove line breaks: "\\n"
    body = str_remove_all(body, "\\\""),
    #body = str_conv(body, "ASCII"),# remove accent marks to ensure comparability
    #body = str_remove_all(body, "�") # remove unknown characters 
  ) |> 
  filter(
    !is.na(body),
    body != "",
    date >= as_date("2023-09-07"), # articles from up to 1 month before Oct 7
    date <= as_date("2023-11-07") # articles from up to 1 month after Oct 7
  ) |> 
  group_by(body) |> 
  filter(n() == 1) |> 
  ungroup() |> 
  select(newspaper, date, title, body) |> 
  mutate(body = str_to_lower(body)) |> 
  arrange(date)

# Create lexicon ---------------------------------------------------------------
#lex <- get_sentiments("afinn") # "bing" , "loughran" "nrc"
#sample_n(lex, 5)

# lex <- read_tsv(
#   "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-en-165.txt",
#   col_names = c("word","value")
# )

# Create Document Feature Matrix -----------------------------------------------
# create dfm
dfmat <- corpus_tidy |> 
  corpus(text_field = "body") |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |> # remove punctuation and numbers
  #tokens_tolower() |> 
  tokens_remove(pattern = stopwords("en")) |> # remove English stopwords
  #tokens_wordstem() |> # stem tokens
  dfm() # convert to DFM

# inspect metadata 
docvars(dfmat) |> head()

# Sentiment analysis: Vader method ---------------------------------------------
# start time 
tictoc::tic()

# calculate valence of articles
sentiments <- vader_df(corpus_tidy$body[1:1000])

# combine article with valence scores
text_sentiment <- cbind(
  text = corpus_tidy$body[1:1000], 
  sentiments |> select(-text, -word_scores)
)

# add metadata: newspaper, date, and article title
text_sentiment$newspaper <- corpus_tidy$newspaper[1:1000]
text_sentiment$date <- as_date(corpus_tidy$date[1:1000]) 
text_sentiment$title <- corpus_tidy$title[1:1000]

# most positive
pos <- text_sentiment |> 
  arrange(desc(compound)) |> 
  head()

for (i in rownames(pos)) {
  print(pos[i, "title"])
  print(pos[i, "compound"])
}

# most negative
neg <- text_sentiment |> 
  arrange(compound) |> 
  head()

for (i in rownames(neg)) {
  print(neg[i, "title"])
  print(neg[i, "compound"])
}

# calculate daily sentiment by news source
daily_sentiment <- text_sentiment %>% 
  group_by(newspaper, date) %>%
  summarise(score = mean(compound)) %>%
  pivot_wider(names_from = newspaper, values_from = score)

# create df of days
days <- data.frame(
  date = seq(as_date("2023-09-07"), as_date("2023-11-07"), 1)
)

# prepare data
daily_sentiment <- daily_sentiment %>% 
  pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
  group_by(newspaper) |> 
  mutate(score_7day_avg = data.table::frollmean(score, 7)) |> 
  arrange(date) 

# plot
daily_sentiment |> 
  #filter(!newspaper %in% c("Die Welt (English)", "South China Morning Post", "The Straits Times (Singapore)")) |> 
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

# Validation -------------------------------------------------------------------
# TODO: random sample for human validation