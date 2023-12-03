#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  How has the Israel-Palestine conflict been covered in the media? >>>>>>>>>>>>>>>>>
#
#
#           --------------Data exploration---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Splitting into Israel, Palestine, Hamas
#                  3) TFIDF by Israel, Palestine and Hamas
# 
# 
# 
#
# Author: Kai Foerster, MSc Data Science for Public Policy
#
# Version date:	  01/12/2023
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

vintage <- "2023-11-30"
output_path     <- "/output/"
vintage_path     <- "/data/vintages/"
raw_path     <- "/data/raw/"


# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "ggplot2", "dplyr", "tidytext", "quanteda") #this version also loads packages from the country files as these seemed to fail loading.

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)


# ===========================================================================
#   1) Load data
# ===========================================================================

corpus_sent <- data.table(read.csv(paste0(wd, vintage_path, "corpus_sent_2023-12-02", ".csv")))
#corpus <- data.table(read.csv(paste0(wd, vintage_path, "corpus_2023-12-02", ".csv")))
#corpus_filtered <- data.table(read.csv(paste0(wd, vintage_path, "corpus_filtered_2023-12-02", ".csv")))


# ===========================================================================
#   1) Splitting into Israel, Hamas, Palestine
# ===========================================================================

corpus_sent[,body:=NULL]

# Assuming df is your corpus data frame
df <- corpus_sent %>%
  mutate(
    key = case_when(
      grepl("Palestin|Gaza", sentence, ignore.case = TRUE) & !grepl("Israel|Hamas", sentence, ignore.case = TRUE) ~ "Palestine",
      grepl("Israel", sentence, ignore.case = TRUE) & !grepl("Palestin|Hamas|Gaza", sentence, ignore.case = TRUE) ~ "Israel",
      grepl("Hamas", sentence, ignore.case = TRUE) & !grepl("Palestin|Israel|Gaza", sentence, ignore.case = TRUE) ~ "Hamas",
      grepl("Palestin|Gaza", sentence, ignore.case = TRUE) & grepl("Israel", sentence, ignore.case = TRUE) & !grepl("Hamas|Hammas", sentence, ignore.case = TRUE) ~ "Mix",
      grepl("Palestin|Gaza", sentence, ignore.case = TRUE) & grepl("Hamas", sentence, ignore.case = TRUE) & !grepl("Israel", sentence, ignore.case = TRUE) ~ "Mix",
      grepl("Israel", sentence, ignore.case = TRUE) & grepl("Hamas", sentence, ignore.case = TRUE) & !grepl("Palestin|Gaza", sentence, ignore.case = TRUE) ~ "Mix",
      TRUE ~ NA_character_  # Default value if none of the conditions are met
    )
  )


tab_data <- df %>%
  group_by(newspaper, key) %>%
  summarize(count = n()) %>%
  pivot_wider(names_from = key, values_from = count, values_fill = 0) 

tab_data$Total <- rowSums(tab_data[, -1])

df_long <- tab_data %>% 
  gather(key = "category", value = "count", -newspaper, -Total)

df_long <- as.data.table(df_long)

df_long[, share := 100*(count/Total)]
df_long$category <- factor(df_long$category, levels = c("NA", "Mix","Hamas", "Palestine", "Israel"))

# plot <- ggplot(df_long, aes(fill =category, y = share, x = reorder(newspaper, -share))) + 
#   geom_bar(position = "stack", stat = "identity") +
#   geom_text(aes(label = sprintf("%.1f%%", share), 
#                 y = share), 
#             position = position_stack(vjust = 0.5), 
#             size = 3, 
#             color = "white") +
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   labs(y = "Share", x = "Newspaper", fill = "Category")+
#   coord_flip()


plot <- ggplot(df_long, aes(fill = category, y = share, x = reorder(newspaper, -share))) + 
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", share), 
                y = share), 
            position = position_stack(vjust = 0.5), 
            size = 3, 
            color = "white") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +  # Move legend to top
  guides(fill = guide_legend(nrow = 1)) +  # Arrange legend items in a single row
  labs(y = "Share", x = "Newspaper", fill = "Category") +
  coord_flip()




  
print(plot)
ggsave(filename = paste0(wd, output_path, "Share_categories", ".png"), plot = plot, width = 10, height = 7)
# ===========================================================================
#   2) TFIDF by Israel, Palestine and Hamas
# ===========================================================================

library(textstem)
library(SnowballC)

all_words <- df %>% 
  unnest_tokens(word, sentence)

all_words[, word := stringi::stri_replace_all_regex(word, "[0-9]", "")]
all_words[, word := as.character(stringi::stri_replace_all_regex(word, "[[:punct:]]", ""))]

all_words <- all_words[word!="",]
all_words <- all_words[!(word %in% c("hamas" ,"hamass", "hamase" ,"israel" ,"israeli" ,"israels" ,"israelism" ,"israelite", "palestinian" ,"palestine" ,"palestines" ,"gaza" ,"gazas" ,"gazans" ,"gazan"))]
all_words <- all_words %>%
  anti_join(get_stopwords(), by = "word") 

all_words[, word2 := lemmatize_words(word)]
all_words <- all_words[!(word2 %in% c("also","al" ,"say", "hamas" ,"hamass", "hamase" ,"israel" ,"israeli" ,"israels" ,"israelism" ,"israelite", "palestinian" ,"palestine" ,"palestines" ,"gaza" ,"gazas" ,"gazans" ,"gazan"))]

tfidf_codes <- all_words %>%
  count(newspaper, key, word2, sort = T) %>%
  # Calculate TF-IDF
  bind_tf_idf(word2, key, n)


plot <- tfidf_codes[!is.na(key),] %>%
  group_by(key) %>%
  arrange(desc(n)) %>%  # Sort by tf_idf in descending order
  slice(1:15) %>%  # Select the top 10 observations
  ungroup() %>%
  ggplot(aes(x = reorder(word2, n), y = n, fill = as.factor(key))) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Count", title = paste("Count of words")) +
  facet_wrap(~key, ncol = 2, scales = "free_y") +
  coord_flip()+
  theme_minimal()

# Save the plot
print(plot)
ggsave(filename = paste0(wd, output_path, "WordCount", ".png"), plot = plot, width = 10, height = 6)


key <- unique(tfidf_codes$key)
newspaper <- unique(tfidf_codes$newspaper)

# Plot the top 10 words by TF-IDF for each party and year

for (p in newspaper) {
  plot <- tfidf_codes[!is.na(key),] %>%
    group_by(key) %>%
    filter(newspaper == p) %>%
    arrange(desc(n)) %>%  # Sort by tf_idf in descending order
    slice(1:15) %>%  # Select the top 10 observations
    ungroup() %>%
    ggplot(aes(x = reorder(word2, n), y = n, fill = as.factor(key))) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "Word count", title = paste("Word count for", p)) +
    facet_wrap(~newspaper+key, ncol = 2, scales = "free_y") +
    coord_flip()+
    theme_minimal()
  
  # Save the plot
  print(plot)
  ggsave(filename = paste0(wd, output_path, "WordCount_", p, ".png"), plot = plot, width = 10, height = 6)
}





