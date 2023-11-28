
#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  Wrangling factiva data  >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and processing---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Create a function that loops over file and extracts articles with their meta data
# 
# 
#
# Author: Kai Foerster, ID: 214288
#
# Version date:	  28/11/2023
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================

# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table")

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)



# ===========================================================================
#   2) Create a function that loops over file and extracts articles with their meta data
# ===========================================================================



# Function to read the file and extract articles into a data.table
extract_articles <- function(filepath) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (line == "The Guardian (London)") {
      # Start of a new article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = prev_line, newspaper = line, date = "", section = "", length = NA, byline = "", highlight = "", body = "", load_date = "")
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- line
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Section:", line)) {
      current_article$section <- sub("Section:\\s*", "", line)
    } else if (grepl("^Length:", line)) {
      current_article$length <- as.numeric(gsub("\\D", "", line))
    } else if (grepl("^Byline:", line)) {
      current_article$byline <- sub("Byline:\\s*", "", line)
    } else if (grepl("^Highlight:", line)) {
      current_article$highlight <- sub("Highlight:\\s*", "", line)
    } else if (line == "Body") {
      reading_body <- TRUE
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
    } else if (reading_body) {
      current_article$body <- paste(current_article$body, line, sep = "\\n")
    }
    prev_line <- line
  }
  # Add the last article if exists
  if (length(current_article) > 0) {
    articles[[length(articles) + 1]] <- current_article
  }
  
  # Convert list of articles to data.table
  dt <- rbindlist(lapply(articles, as.data.table), fill = TRUE)
  return(dt)
}


# Path to the new file
filepath <- paste0(wd, "/data/raw/Guardian_test_exl_overview.txt")

# Extracting articles and creating a data.table
articles_dt <- extract_articles(filepath)

# Print or write the data.table to a file
print(articles_dt)
