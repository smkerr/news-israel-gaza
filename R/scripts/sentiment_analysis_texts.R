# Sentiment analysis (article-level) ==========================================
# Setup ------------------------------------------------------------------------
# load packages
pacman::p_load(
  dplyr,
  ggplot2,
  here,
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
  mutate(
    body = str_to_lower(body),
    title = coalesce(title, highlight, byline)
    ) |> 
  select(newspaper, date, title, body) |> 
  arrange(date)

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

# Prepare data for sentiment analysis ------------------------------------------
# calculate valence of articles
sentiments <- vader_df(corpus_tidy$body) # ~90 min

# save for later
#saveRDS(sentiments, here("data/sentiments_articles.rds"))

# combine articles, sentiment scores, and metadata
text_sentiment <- cbind(
  text = corpus_tidy$body, # text
  sentiments |> select(-text, -word_scores), # sentiment scores
  newspaper = corpus_tidy$newspaper, # media oulet
  date = as_date(corpus_tidy$date), # date 
  title = corpus_tidy$title # label (e.g., palestine/gaza, israel)
)


# Write function to find the most positive articles ----------------------------
most_pos_text <- function(df) {
  
  # most positive
  pos <- df |> 
    arrange(desc(compound)) |> 
    head()
  
  return(pos)
  
} 


# Write function to find the most negative articles ----------------------------
most_neg_text <- function(df) {
  
  # most negative
  neg <- df |> 
    arrange(compound) |> 
    head()
  
  return(neg)
  
} 


# Write function to plot daily sentiment ---------------------------------------
plot_sentiment_text <- function(
    df, 
    group = "newspaper", 
    start_date = as_date("2023-09-07"), 
    end_date = as_date("2023-11-07")
) {
  
  # calculate daily sentiment by news source
  daily_sentiment <- df |> 
    group_by(newspaper, date) |>
    summarise(score = mean(compound, na.rm = TRUE)) |>
    pivot_wider(names_from = newspaper, values_from = score)
  
  # define date range
  days <- data.frame(
    date = seq(start_date, end_date, 1)
  )
  
  # reshape data
  daily_sentiment <- daily_sentiment |> 
    pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
    arrange(date) 
  
  # plot sentiment over time
  out <- daily_sentiment |> 
    ggplot(aes(date, color = .data[[group]])) + 
    geom_hline(yintercept = 0, color = "darkgrey") +
    geom_vline(xintercept = as_date("2023-10-07"), color = "red", linetype = "longdash") + 
    geom_point(aes(y = score), alpha = .9) + 
    geom_smooth(aes(y = score), se = FALSE, method = "lm") + 
    annotate("text", x = as_date("2023-10-09"), y = .75, angle = 90, label = "Oct 7th", color = "red") +
    scale_x_date(
      limits = c(start_date, end_date),
      date_breaks = "1 week", 
      date_labels = "%d %b",
      date_minor_breaks = "1 week"
    ) +
    scale_y_continuous(breaks = seq(-1, 1, 0.2), minor_breaks = NULL) +
    scale_color_brewer(
      palette = "Set2",
      labels = c("Al Jazeera", "Die Welt", "South China Morning Post", "The Guardian", 
                 "The New York Times", "The Straits Times", "The Times of India")
    ) +
    labs(
      title = "Sentiment by News Source",
      subtitle = "News coverage of 2023 Israel-Hamas War",
      caption = "Source: LexisNexis",
      x = NULL,
      y = "Sentiment Score",
      color = "News Source"
    ) + 
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 0.5)
    )
  
  return(out)
  
}


# Sentiment analysis: Vader method ---------------------------------------------
# most positive articles
pos_text <- most_pos_text(text_sentiment)
pos_text

for (i in rownames(pos_text)) {
  print(pos_text[i, "title"])
  print(pos_text[i, "compound"])
}

pos_text |> 
  gt::gt()

# most negative articles
neg_text <- most_neg_text(text_sentiment)
neg_text

for (i in rownames(neg_text)) {
  print(neg_text[i, "title"])
  print(neg_text[i, "compound"])
}

# plot article sentiment over time 
p <- plot_sentiment_text(text_sentiment)
p

# save plot
ggsave(filename = here("output/sentiment_articles.png"))

# Sentiment analysis: NRC method -----------------------------------------------
# load NRC lexicon
lex <- read_tsv(
  here("data/lex/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt"), 
  col_names = c("word", "sentiment", "value")
  ) |> 
  filter(!sentiment %in% c("negative", "positive"))

# types of sentiment
unique(lex$sentiment)

doc_sentiments <- dfmat |> 
  tidy() |> # DFM to tidy format
  inner_join(lex, by = c("term" = "word"), relationship = "many-to-many") |> # combine DFM with lexicon
  mutate(value = count * value) |> # calculate sentiment scores
  group_by(document, sentiment) |>
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ungroup()

# combine with metadata
doc_sentiments <- doc_sentiments |> 
  inner_join(
    dfmat@docvars |> select(docname_, newspaper, date, title),
    by = c("document" = "docname_")
    )

# calculate averages
daily_sentiments <- doc_sentiments |> 
  group_by(newspaper, date, sentiment) |> 
  summarize(value = mean(value, na.rm = TRUE)) |> 
  ungroup()

# plot sentiment over time
daily_sentiments |> 
  filter(!is.na(sentiment)) |> 
  ggplot(aes(date, color = newspaper)) + 
  geom_hline(yintercept = 0, color = "darkgrey") +
  geom_vline(xintercept = as_date("2023-10-07"), color = "red", linetype = "longdash") + 
  geom_point(aes(y = value), alpha = .25) +
  geom_smooth(aes(y = value), se = FALSE, method = "lm") + 
  annotate("text", x = as_date("2023-10-09"), y = 50, angle = 90, label = "Oct 7th", color = "red") +
  facet_wrap(~sentiment, ncol = 2) +
  scale_x_date(
    limits = c(as_date("2023-09-07"), as_date("2023-11-07")),
    date_breaks = "1 week", 
    date_labels = "%d %b",
    date_minor_breaks = "1 week"
  ) +
  #scale_y_continuous(breaks = seq(-1, 1, 0.2), minor_breaks = NULL) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Al Jazeera", "Die Welt", "South China Morning Post", "The Guardian", 
               "The New York Times", "The Straits Times", "The Times of India")
  ) +
  labs(
    title = "Sentiment by News Source",
    subtitle = "News coverage of 2023 Israel-Hamas War",
    caption = "Source: LexisNexis",
    x = NULL,
    y = "Sentiment Score",
    color = "News Source"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

# save plot 
ggsave(filename = here("output/sentiment_articles_nrc.png"))

# Write function to find the top articles by sentiment -------------------------
top_sentiment_text <- function(df, emotion) {
  
  if (!emotion %in% unique(lex$sentiment)) return("Enter a valid sentiment: 'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust'")
  
  # top articles by sentiment
  top_df <- df |> 
    filter(sentiment == emotion) |> 
    arrange(desc(value)) |> 
    head()
  
  return(top_df)
  
} 

# top articles by sentiment
top_sentiment_text(doc_sentiments, emotion = "anger")
top_sentiment_text(doc_sentiments, emotion = "anticipation")
top_sentiment_text(doc_sentiments, emotion = "disgust")
top_sentiment_text(doc_sentiments, emotion = "fear")
top_sentiment_text(doc_sentiments, emotion = "joy")
top_sentiment_text(doc_sentiments, emotion = "sadness")
top_sentiment_text(doc_sentiments, emotion = "surprise")
top_sentiment_text(doc_sentiments, emotion = "trust")

# Validation -------------------------------------------------------------------
# TODO: random sample for human validation