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

## To Do
## concatenate title, highlight and where byline or section includes title do that?


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
pckg_to_load <- c("data.table", "ggplot2", "dplyr") #this version also loads packages from the country files as these seemed to fail loading.

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
corpus_filtered <- data.table(read.csv(paste0(wd, vintage_path, "corpus_filtered_2023-12-02", ".csv")))


# ===========================================================================
#   1) Summary statistics and data
# ===========================================================================

# Frequency of articles by newspaper
article_counts <- corpus[, .(count = .N), by = newspaper]


# Create a bar chart using ggplot2 and add labels with actual counts
ggplot(article_counts, aes(x = reorder(newspaper, count), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = count), hjust = -0.2, size = 3) +  # Add labels
  labs(title = "",
       x = "Newspaper", y = "Frequency") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_flip()
 

ggsave(paste0(wd, output_path, "articles_frequency",".png"), width = 10, height = 8)

# Frequency of articles over time

# Calculate the frequency of articles by newspaper and date
article_counts <- corpus[, .N, by = .(date)]
article_counts$date <- as.Date(article_counts$date)
article_counts <- article_counts[date>as.Date("2023-09-01"),]

# Create a line plot to visualize the frequency over time
ggplot(article_counts, aes(x = date, y = N)) +
  geom_line(size = 1) +
  labs(title = "",
       x = "Date", y = "Frequency") +
  scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 week", expand = c(0.02, 0.02)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Tilt date labels
  geom_vline(xintercept = as.Date("2023-10-07"), color = "black", linetype = "dashed")+
  annotate("text", x = as.Date("2023-10-14"), y = max(article_counts$N), label = "Start of Conflict", vjust = -0.5, hjust = 0.5, color = "black")# Add red vertical line


ggsave(paste0(wd, output_path, "articles_over_time",".png"), width = 11, height = 8)


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


# Distribution of article length

# Create a boxplot of article length distribution for each newspaper
ggplot(corpus_filtered, aes(x = reorder(newspaper, length), y = length, fill = newspaper)) +
  geom_boxplot() +
  geom_hline(yintercept = 3000, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Newspaper", y = "Article Length") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(fill = FALSE)+
  annotate("text", x = 1.15, y = 2640, label = "Cutoff at 3000 words", color = "red")+
  coord_flip()

ggsave(paste0(wd, output_path, "boxplot_length",".png"), width = 10, height = 8)


# Length over time

corpus_filtered$date <- as.Date(corpus_filtered$date)
corpus_filtered <- corpus_filtered[date>as.Date("2023-09-01"),]
corpus_filtered[, mean_length := mean(length, na.rm=T ), by = date]

# Create a scatterplot of article length over time with a moving average and confidence bands
# ggplot(corpus_filtered, aes(x = date, y = mean_length)) +
#   geom_point(color = "blue") +  # Scatterplot points
#   geom_smooth(method = "lm", formula = y ~ poly(x, 10), color = "red", fill = "pink", alpha = 0.3, se = TRUE) +  # Polynomial regression line with confidence bands  labs(title = "Article Length Over Time",
#   labs(title = "Article Length Over Time",     
#        x = "Date", y = "Article Length") +
#   scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 week", expand = c(0.02, 0.02)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   geom_vline(xintercept = as.Date("2023-10-07"), color = "black", linetype = "dashed")+
#   annotate("text", x = as.Date("2023-10-14"), y = max(corpus_filtered$mean_length)-50, label = "Start of Conflict", vjust = -0.5, hjust = 0.5, color = "black")# Add red vertical line
# 

# Split the data into two subsets
before_break <- corpus_filtered %>% 
  filter(date < as.Date("2023-10-07"))
after_break <- corpus_filtered %>% 
  filter(date >= as.Date("2023-10-07"))

# Fit linear models
lm_before <- lm(mean_length ~ date, data = before_break)
lm_after <- lm(mean_length ~ date, data = after_break)

# Plot
ggplot() +
  geom_point(data = corpus_filtered, aes(x = date, y = mean_length), color = "blue") +
  geom_line(data = before_break, aes(x = date, y = predict(lm_before, newdata = before_break)), color = "red") +
  geom_line(data = after_break, aes(x = date, y = predict(lm_after, newdata = after_break)), color = "red") +
  labs(title = "", x = "Date", y = "Article Length") +
  scale_x_date(date_labels = "%d %b %Y", date_breaks = "1 week", expand = c(0.02, 0.02)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2023-10-07"), color = "black", linetype = "dashed") +
  annotate("text", x = as.Date("2023-10-14"), y = max(corpus_filtered$mean_length) - 50, label = "Start of Conflict", vjust = -0.5, hjust = 0.5, color = "black")


ggsave(paste0(wd, output_path, "length_overtime",".png"), width = 10.2, height = 8)

