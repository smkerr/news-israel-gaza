#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  How has the Israel-Palestine conflict been covered in the media? >>>>>>>>>>>>>>>>>
#
#
#           --------------Data exploration---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) x
#                  3) xx
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
pckg_to_load <- c("data.table", "ggplot2") #this version also loads packages from the country files as these seemed to fail loading.

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
corpus <- data.table(read.csv(paste0(wd, vintage_path, "corpus_2023-12-02", ".csv")))


# ===========================================================================
#   1) Summary statistics and data
# ===========================================================================

# Frequency of articles by newspaper
article_counts <- corpus[, .(count = .N), by = newspaper]


# Create a bar chart using ggplot2 and add labels with actual counts
ggplot(article_counts, aes(x = reorder(newspaper, -count), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +  # Add labels
  labs(title = "Frequency of Newspaper Articles by Newspaper",
       x = "Newspaper", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(paste0(wd, output_path, "articles_frequency",".png"), width = 10, height = 8)

# Frequency of articles over time

# Calculate the frequency of articles by newspaper and date
article_counts <- corpus[, .N, by = .(date)]
article_counts$date <- as.Date(article_counts$date)
article_counts <- article_counts[date>as.Date("2023-09-01"),]

# Create a line plot to visualize the frequency over time
ggplot(article_counts, aes(x = date, y = N)) +
  geom_line(size = 1) +
  labs(title = "Frequency of Newspaper Articles Over Time",
       x = "Date", y = "Frequency") +
  scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 week", expand = c(0.02, 0.02)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Tilt date labels
  geom_vline(xintercept = as.Date("2023-10-07"), color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2023-10-14"), y = max(article_counts$N), label = "Start of Conflict", vjust = -0.5, hjust = 0.5, color = "black")# Add red vertical line


ggsave(paste0(wd, output_path, "articles_over_time",".png"), width = 10, height = 8)


# Frequency of articles over time and newspaper

# Calculate the frequency of articles by newspaper and date
article_counts <- corpus[, .N, by = .(newspaper, date)]
article_counts$date <- as.Date(article_counts$date)
article_counts <- article_counts[date>as.Date("2023-09-01"),]

ggplot(article_counts, aes(x = date, y = N, color = newspaper)) +
  geom_line(size = 1) +  # Increase line thickness
  labs(title = "Frequency of Newspaper Articles Over Time",
       x = "Date", y = "Frequency") +
  scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 week", expand = c(0.02, 0.02)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Tilt date labels
  geom_vline(xintercept = as.Date("2023-10-07"), color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2023-10-15"), y = max(article_counts$N), label = "Start of Conflict", vjust = -0.5, hjust = 0.5, color = "black")# Add red vertical line

ggsave(paste0(wd, output_path, "articles_over_time_newspaper",".png"), width = 10, height = 8)
