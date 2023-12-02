# Explore data =================================================================
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
corpus <- readr::read_csv(here::here("data/vintages/corpus_2023-12-02.csv")) # latest version 
corpus <- corpus |> 
  mutate(doc_id = paste0("text", row_number())) |> 
  relocate(doc_id, .before = title) |> 
  rename(text = body, doc = title) |> 
  select(doc_id, doc, newspaper, date, length, load_date, text)

# Explore variables ============================================================
# newspaper --------------------------------------------------------------------
corpus |> 
  count(newspaper, sort = TRUE)

# title ------------------------------------------------------------------------
corpus |> 
  count(doc, sort = TRUE) |> 
  filter(n > 1) # TODO: 69 articles are duplicates; remove duplicate articles

# date & load_date -------------------------------------------------------------
# missings
corpus |> 
  filter(is.na(load_date)) # TODO: 4 missing load dates; looks like we can exclude all

# load dates fail to parse
parse_fail_ids <- corpus |> 
  mutate(load_date = mdy(load_date)) |> 
  filter(is.na(load_date)) |> 
  pull(doc_id)
corpus |> 
  filter(doc_id %in% parse_fail_ids) # TODO: need to remove "<a0>" pattern before load date

# time frame
corpus |> 
  mutate(load_date = mdy(load_date)) |>
  filter(date < as_date("2023-09-01") | load_date < as_date("2023-09-01")) # TODO: exclude articles published before 1 Sep 2023

# plot date
corpus |> # TODO: remove articles from before 1 Sep 2023 and after ???
  ggplot(aes(x = date, color = newspaper)) + 
  geom_vline(xintercept = as_date("2023-10-07"), color = "red", linetype = "dashed") +
  geom_freqpoly() + 
  scale_x_date(limits = c(as_date("2023-09-01"), as_date("2023-11-15"))) +
  labs(
    x = NULL,
    y = "# articles"
  ) +
  theme_minimal()

# plot load date
corpus |> # TODO: remove articles from before 1 Sep 2023 and after ???
  mutate(load_date = mdy(load_date)) |>
  ggplot(aes(x = load_date, color = newspaper)) + 
  geom_vline(xintercept = as_date("2023-10-07"), color = "red", linetype = "dashed") +
  geom_freqpoly() + 
  #scale_x_date(limits = c(as_date("2023-09-01"), as_date("2023-11-15"))) +
  labs(
    x = NULL,
    y = "# articles"
  ) +
  theme_minimal()

# lag time (load date - published date)
corpus |> # TODO: Al Jazeera appears to load articles up to 20 days after publishing
  filter(
    !is.na(load_date),
    !newspaper %in% remove_newspapers
  ) |> 
  mutate(
    load_date = str_remove(load_date, "<a0>"),
    load_date = mdy(load_date),
    lag = load_date - date
  ) |> 
  select(newspaper, date, load_date, lag) |> 
  ggplot(aes(x = lag, fill = newspaper)) +
  geom_bar() +
  facet_wrap(~newspaper, scales = "free_y") + 
  theme_minimal()

# length -----------------------------------------------------------------------
corpus |> 
  summary(length)

corpus |> 
  ggplot(aes(x = length)) +
  geom_density() +
  facet_wrap(~newspaper, scales = "free_x") + 
  theme_minimal()

# text -------------------------------------------------------------------------
corpus |> 
  filter(str_detect(text, "\\\\n")) # all texts include these line breaks

# which meta-symbols do we need to we need to remove from the text?
str_extract_all(corpus$text, "\\\\\\w") |> unlist() |> unique()
# TODO: remove these meta-symbols: "\\n" "\\I" NA    "\\x" (\\narrtl, \\niwrtl)

# 187 The Guardian articles with these blocks, ex: "\\nblock-time published-time 3.09am GMT" & "\\nblock-time updated-timeUpdated at 3.16am GMT"
corpus |> 
  filter(str_detect(text, "block-time updated-timeUpdated|block-time published-time")) |> 
  count(newspaper) # TODO: remove these meta texts

corpus |> 
  filter(str_detect(text, "End of Document")) |> 
  count(newspaper, sort = TRUE) # TODO: remove "End of Document"

corpus |> 
  filter(str_detect(text, "Photograph:")) |> 
  count(newspaper, sort = TRUE) # TODO: remove "Photograph:" from The Guardian articles

corpus |> 
  filter(str_detect(text, "\\?{7}")) |> 
  count(newspaper, sort = TRUE) # TODO: remove excessive question marks from The Guardian articles

corpus |> 
  filter(str_detect(text, "Sign up here for our daily newsletter, First Edition")) |> 
  count(newspaper, sort = TRUE) # TODO: remove references to newsletters


# TODO: take accent marks into consideration (ex: Médecins Sans Frontières, António Guterres)