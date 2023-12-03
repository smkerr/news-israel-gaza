# Sentiment analysis (sentence-level) ==========================================
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
corpus <- readr::read_csv(here::here("data/vintages/corpus_sent_2023-12-02.csv")) |> # latest version (sentence-level)
  select(-body)

# Clean data -------------------------------------------------------------------
corpus_tidy <- corpus |> 
  mutate(
    sentence = str_trim(sentence),
    sentence = str_remove_all(sentence, "\\\\n"), # remove line breaks: "\\n"
    sentence = str_remove_all(sentence, "\\\"")
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
    date <= as_date("2023-11-07"), # articles from up to 1 month after Oct 7
  ) |> 
  group_by(sentence) |> 
  filter(n() == 1) |> # remove duplicates
  ungroup() |> 
  mutate(sentence = str_to_lower(sentence)) |> 
  select(newspaper, date, sentence, title) |> 
  arrange(date)

# assign Palestine/Gaza or Israel label
corpus_labeled <- corpus_tidy |> 
  mutate(
    label = case_when(
      !str_detect(sentence, "palest\\w+|gaza\\w*|israel\\w*|hamas|hammas") ~ NA, # NA if no mention of Palestine/Gaza, Israel, or Hamas
      str_detect(sentence, "palest\\w+|gaza\\w*") & str_detect(sentence, "hamas|hammas") |
        str_detect(sentence, "palest\\w+|gaza\\w*") & str_detect(sentence, "israel\\w+") |
        str_detect(sentence, "hamas|hammas") & str_detect(sentence, "israel\\w+") ~ NA_character_, # NA if two are mentioned together
      str_detect(sentence, "palest\\w+|gaza\\w*") ~ "palestine/gaza",
      str_detect(sentence, "israel\\w*") ~ "israel",
      str_detect(sentence, "hamas|hammas") ~ "hamas",
      TRUE ~ NA
    )
  ) |> 
  filter(!is.na(label))

# inspect labels
corpus_labeled |> 
  count(label, sort = TRUE)
corpus_labeled |> 
  janitor::tabyl(newspaper, label) |> 
  janitor::adorn_totals()

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

# Prepare data for sentiment analysis ------------------------------------------
# start time 
tictoc::tic()

# calculate valence of sentences
sentiments <- vader_df(corpus_labeled$sentence)

# end time 
tictoc::toc()

# combine sentences, sentiment scores, and metadata
sent_sentiment <- cbind(
  text = corpus_labeled$sentence, # text
  sentiments |> select(-text, -word_scores), # sentiment scores
  newspaper = corpus_labeled$newspaper, # media oulet
  date = as_date(corpus_labeled$date), # date 
  label = corpus_labeled$label # label (e.g., palestine/gaza, israel)
)

# Write function to find the most positive sentences ---------------------------
most_pos_sent <- function(df, type = "all") {
  
  # check inputs
  if (!type %in% c("all", "palestine/gaza", "israel", "hamas")) return("Invalid type. Please choose from 'all', 'palestine/gaza', 'israel', 'hamas'")
  
  # filter by type, if needed
  if (type != "all") {
    df <- df |> 
      filter(label == type)
  }
  
  # most positive
  pos <- df |> 
    arrange(desc(compound)) |> 
    head()
  
  return(pos)

} 

# Write function to find the most negative sentences ---------------------------
most_neg_sent <- function(df, type = "all") {
  
  # check inputs
  if (!type %in% c("all", "palestine/gaza", "israel", "hamas")) return("Invalid type. Please choose from 'all', 'palestine/gaza', 'israel', 'hamas'")
  
  # filter by type, if needed
  if (type != "all") {
    df <- df |> 
      filter(label == type)
  }
  
  # most negative
  neg <- sent_sentiment |> 
    arrange(compound) |> 
    head()
  
  return(neg)
  
} 

# Write function to plot daily sentiment ---------------------------------------
plot_sentiment_sent <- function(
    df, 
    type = "all", 
    group = "newspaper", 
    start_date = as_date("2023-09-07"), 
    end_date = as_date("2023-11-07")
    ) {
  
  # check input
  if (!type %in% c("all", "palestine/gaza", "israel", "hamas")) return("Invalid type. Please choose from 'all', 'palestine/gaza', 'israel', 'hamas'")

  # filter based on type, if needed
  if (type != "all") {
    df <- df |> 
      filter(label == type)
  }

  # calculate daily sentiment by news source
  daily_sentiment <- df |> 
    group_by(newspaper, date) |>
    summarise(score = mean(compound, na.rm = TRUE)) |>
    pivot_wider(names_from = newspaper, values_from = score)
  
  # create df of days
  days <- data.frame(
    date = seq(start_date, end_date, 1)
  )
  
  # reshape data
  daily_sentiment <- daily_sentiment |> 
    pivot_longer(cols = -date, names_to = "newspaper", values_to = "score") |> 
    #group_by(newspaper) |> 
    #mutate(score_7day_avg = data.table::frollmean(score, 7)) |> 
    arrange(date) 
  
  # plot sentiment over time
  out <- daily_sentiment |> 
    #filter(!newspaper %in% c("Die Welt (English)", "South China Morning Post", "The Straits Times (Singapore)", "The Times of India (TOI)")) |> 
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
      subtitle = case_when(
        type == "all" ~ "News coverage of 2023 Israel-Hamas War",
        type == "israel" ~ "News coverage mentioning Israel",
        type == "palestine/gaza" ~ "News coverage mentioning Palestine and/or Gaza",
        type == "hamas" ~ "News coverage mentioning Hamas"
        ),
      caption = "Source: LexisNexis",
      #caption = str_wrap(str_c("Sources: ", str_c(head(unique(df$newspaper), -1), collapse = ", "), ", and ", tail(unique(df$newspaper), 1)), 100),
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

# Sentiment analysis, overall --------------------------------------------------
# most positive sentences, overall
pos_sent_all <- most_pos_sent(sent_sentiment, "all")
pos_sent_all

for (i in rownames(pos_sent_all)) {
  print(pos_sent_all[i, "text"])
  print(pos_sent_all[i, "compound"])
}

# most negative sentences, overall 
neg_sent_all <- most_neg_sent(sent_sentiment, "all")
neg_sent_all

for (i in rownames(neg_sent_all)) {
  print(neg_sent_all[i, "text"])
  print(neg_sent_all[i, "compound"])
}

# plot sentiment over time, overall
p <- plot_sentiment_sent(sent_sentiment, type = "all", group = "newspaper")
p

# save plot
ggsave(filename = here("output/sentiment_sent_all.png"))

# Sentiment analysis, Israel ---------------------------------------------------
# most positive sentences, Israel
pos_sent_isr <- most_pos_sent(sent_sentiment, "israel")
pos_sent_isr

for (i in rownames(pos_sent_isr)) {
  print(pos_sent_isr[i, "text"])
  print(pos_sent_isr[i, "compound"])
}

# most negative sentences, Israel 
neg_sent_isr <- most_neg_sent(sent_sentiment, "israel")
neg_sent_isr

for (i in rownames(neg_sent_isr)) {
  print(neg_sent_isr[i, "text"])
  print(neg_sent_isr[i, "compound"])
}

# plot sentiment over time, Israel
p <- plot_sentiment_sent(sent_sentiment, type = "israel", group = "newspaper")
p

# save plot
ggsave(filename = here("output/sentiment_sent_israel.png"))


# Sentiment analysis, Palestine/Gaza -------------------------------------------
# most positive sentences, Palestine/Gaza
pos_sent_pal <- most_pos_sent(sent_sentiment, "palestine/gaza")
pos_sent_pal

for (i in rownames(pos_sent_pal)) {
  print(pos_sent_pal[i, "text"])
  print(pos_sent_pal[i, "compound"])
}

# most negative sentences, Palestine/Gaza 
neg_sent_pal <- most_neg_sent(sent_sentiment, "palestine/gaza")
neg_sent_pal

for (i in rownames(neg_sent_pal)) {
  print(neg_sent_pal[i, "text"])
  print(neg_sent_pal[i, "compound"])
}

# plot sentiment over time, Palestine/Gaza
p <- plot_sentiment_sent(sent_sentiment, type = "palestine/gaza", group = "newspaper")
p

# save plot
ggsave(filename = here("output/sentiment_sent_palestine.png"))


# Sentiment analysis, Hamas ----------------------------------------------------
# most positive sentences, Hamas
pos_sent_hms <- most_pos_sent(sent_sentiment, "hamas")
pos_sent_hms

for (i in rownames(pos_sent_hms)) {
  print(pos_sent_hms[i, "text"])
  print(pos_sent_hms[i, "compound"])
}

# most negative sentences, Hamas
neg_sent_hms <- most_neg_sent(sent_sentiment, "hamas")
neg_sent_hms

for (i in rownames(neg_sent_hms)) {
  print(neg_sent_hms[i, "text"])
  print(neg_sent_hms[i, "compound"])
}

# plot sentiment over time, Hamas
p <- plot_sentiment_sent(sent_sentiment, type = "hamas", group = "newspaper")
p

# save plot
ggsave(filename = here("output/sentiment_sent_hamas.png"))

# Sentiment analysis: NRC method -----------------------------------------------
# load NRC lexicon
lex <- read_tsv(
  here("data/lex/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt"), 
  col_names = c("word", "sentiment", "value")
) |> 
  filter(!sentiment %in% c("negative", "positive"))

# types of sentiment
unique(lex$sentiment)

sent_sentiments <- dfmat |> 
  tidy() |> # DFM to tidy format
  inner_join(lex, by = c("term" = "word"), relationship = "many-to-many") |> # combine DFM with lexicon
  mutate(value = count * value) |> # calculate sentiment scores
  group_by(document, sentiment) |>
  summarise(value = sum(value, na.rm = TRUE)) |> 
  ungroup()

# combine with metadata
sent_sentiments <- sent_sentiments |> 
  inner_join(
    dfmat@docvars |> select(docname_, newspaper, date, label, sentence),
    by = c("document" = "docname_")
  )

# Write function to plot daily sentiment ---------------------------------------
plot_nrc_sentiment_sent <- function(
    df, 
    type = "all", 
    group = "newspaper", 
    start_date = as_date("2023-09-07"), 
    end_date = as_date("2023-11-07")
    ) {
  
  # check input
  if (!type %in% c("all", "palestine/gaza", "israel", "hamas")) return("Invalid type. Please choose from 'all', 'palestine/gaza', 'israel', 'hamas'")
  
  # filter by label, if needed
  if (type != "all") {
    df <- df |> 
      filter(label == type)
  }
  
  # calculate daily averages
  daily_sentiments <- df |> 
    group_by(newspaper, date, sentiment) |> 
    summarize(value = mean(value, na.rm = TRUE)) |> 
    ungroup() |> 
    mutate(sentiment = str_to_title(sentiment))
  
  # determine plot order
  sentiment_order <- daily_sentiments |>
    group_by(date_type = ifelse(date < as_date("2023-10-07"), "pre_oct7", "post_oct7"), sentiment) |>
    summarize(avg_val = mean(value, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = date_type, values_from = avg_val) |>
    mutate(val_diff = post_oct7 - pre_oct7) |>
    arrange(desc(val_diff)) |>
    pull(sentiment)
  
  # plot sentiment over time
  out <- daily_sentiments |> 
    mutate(sentiment = factor(sentiment, levels = sentiment_order)) |> 
    ggplot(aes(date, group = date < as_date("2023-10-07"))) + # color = .data[[group]])) + 
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = as_date("2023-10-07"), color = "red", linetype = "longdash") + 
    geom_point(aes(y = value), alpha = .5) + 
    geom_smooth(aes(y = value), se = FALSE, method = "lm") + 
    #annotate("text", x = as_date("2023-10-12"), y = 1.75, angle = 90, label = "Oct 7th", color = "red") +
    facet_wrap(~sentiment, ncol = 2) +
    scale_x_date(
      limits = c(start_date, end_date),
      date_breaks = "1 week", 
      date_labels = "%d %b",
      date_minor_breaks = "1 week"
    ) +
    #scale_y_continuous(breaks = seq(0, 2.5, 0.5), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    scale_color_brewer(
      palette = "Set2",
      labels = c("Al Jazeera", "Die Welt", "South China Morning Post", "The Guardian", 
                 "The New York Times", "The Straits Times", "The Times of India")
    ) +
    labs(
      title = "Sentence-level Sentiment Scores",
      subtitle = case_when(
        type == "all" ~ "News coverage of 2023 Israel-Hamas War",
        type == "israel" ~ "News coverage mentioning Israel",
        type == "palestine/gaza" ~ "News coverage mentioning Palestine and/or Gaza",
        type == "hamas" ~ "News coverage mentioning Hamas"
      ),
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

# plot sentiments over time, overall 
plot_nrc_sentiment_sent(sent_sentiments)
# save plot 
ggsave(filename = here("output/sentiment_sent_nrc_all.png"))

# plot sentiments over time, Palestine/Gaza 
plot_nrc_sentiment_sent(sent_sentiments, type = "palestine/gaza")
# save plot 
ggsave(filename = here("output/sentiment_sent_nrc_palestine.png"))

# plot sentiments over time, Israel 
plot_nrc_sentiment_sent(sent_sentiments, type = "israel")
# save plot 
ggsave(filename = here("output/sentiment_sent_nrc_israel.png"))

# plot sentiments over time, Israel 
plot_nrc_sentiment_sent(sent_sentiments, type = "hamas")
# save plot 
ggsave(filename = here("output/sentiment_sent_nrc_hamas.png"))

# Write function to find the top articles by sentiment -------------------------
top_sentiment_text <- function(df, emotion) {
  
  if (!emotion %in% unique(lex$sentiment)) return("Enter a valid sentiment: 'anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust'")
  
  # top articles by sentiment
  top_df <- df |> 
    filter(sentiment == emotion) |> 
    select(newspaper, sentence, value) |> 
    arrange(desc(value)) 
  
  return(top_df)
  
} 

sent_sentiments_labeled <- corpus_labeled |> 
  group_by(title) |> 
  mutate(document = paste0("text", n())) |> 
  inner_join(sent_sentiments, relationship = "many-to-many") |> 
  ungroup()

# top articles by sentiment
top_sentiment_text(sent_sentiments_labeled, emotion = "anger") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "anticipation") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "disgust") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "fear") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "joy") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "sadness") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "surprise") |> head()
top_sentiment_text(sent_sentiments_labeled, emotion = "trust") |> head()
