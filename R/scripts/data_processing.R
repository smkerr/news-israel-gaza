#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  How has the Israel-Palestine conflict been covered in the media? >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and processing---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Retrieve latest newspaper level data from original sources
#                  3) Load and process data into one corpus
# 
# 
# 
#
# Author: Kai Foerster, MSc Data Science for Public Policy
#
# Version date:	  28/11/2023
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

#.................................MANUALLY UPDATE WHEN NEWPAPER IS ADDED OR REMOVED
#media_list <- c("The Guardian (London)", "Al Jazeera English", "The Straits Times (Singapore)", "The Times of India (TOI)", "South China Morning Post", "The New York Times", "Die Welt (English))
#media_list2 <- c("TGD", "ALJ", "TST", "TOI", "SCM", "NYT" "DWE")
media_list <- c("The Guardian (London)", "Al Jazeera English", "South China Morning Post", "The Straits Times (Singapore)")
media_abbrv<- c("TGD","ALJ","SCM", "TST")
#media_list <- c("The Straits Times (Singapore)")
#media_abbrv<- c("TST")
media_dict <- setNames(media_abbrv, media_list) # Create a dictionary mapping media names to abbreviations


col_date <-  c("date", "update_date")
today <- Sys.Date()

# Default settings and connections 
update_raw_data <- 1
update_vintage <- 0
load_raw_data   <- 1
first_ever_run <- 0
convert_rtf <- 0
tokenise_operation <-0 
vintage <- "2023-11-30"
output_path     <- "/data/"
vintage_path     <- "/data/vintages/"
raw_path     <- "/data/raw/"


# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table", "here", "lubridate", "stringr", "striprtf") #this version also loads packages from the country files as these seemed to fail loading.

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)

# Load functions
source("R/funcs/wrangling_function.R")

# ===========================================================================
#   1) Convert RTF to TXT files
# ===========================================================================
if (convert_rtf == 1) {
# define file names
files_in <- list.files(path = here("data/rtf")) # RTF files (input)
files_out <- str_replace(files_in, "RTF", "txt") # TXT files (output)

# for each RTF file...
for (i in seq_along(files_in)) {
  
  # skip if TXT file already exists
  if (file.exists(here("data/raw", files_out[i]))) next
  
  # read in RTF file
  temp_txt <- read_rtf(here("data/rtf", files_in[i]))
  
  # save as TXT file
  writeLines(temp_txt, here("data/raw", str_replace(files_out[i], "RTF", "txt")))
  
}
}
# ===========================================================================
#   2) Retrieve latest newspaper level data from original sources
# ===========================================================================

# Update data for selected newspapers ------

if (update_raw_data == 1) {
  for (cc in media_list) {
    i <- 1
    while (TRUE) {
      filepath <- paste0(wd, raw_path, cc, "_", i, ".txt")
      print(paste0("Updating latest data for ", cc, ", document ", i))
      if (file.exists(filepath)) {
        tryCatch({
          articles_dt <- get(paste0("extract_articles_", media_dict[cc]))(filepath, cc)
          csv_filepath <- paste0(wd, vintage_path, cc, "_", i, ".csv")
          write.csv(articles_dt, csv_filepath, row.names = FALSE, na = "")
        }, error = function(cond) {
          message(paste0("Code seems to break"))
          message("Here's the original error message:")
          message(cond)
        }, warning = function(cond) {
          message(paste0("Code returned warning"))
          message("Here's the original warning message:")
          message(cond)
        })
        i <- i + 1
      } else {
        # If a file does not exist, break the loop and move to the next newspaper
        break
      }
    }
  }
}

remove(articles_dt)
# ===========================================================================
#   3) Load and process data into one corpus
# ===========================================================================

# Load new data -----------------------------------------------------------


corpus_new <- data.table()

#Load full corpus
if (load_raw_data == 1) {
  for (cc in media_list) {
    i <- 1
    while (TRUE) {
      filepath <- paste0(wd, vintage_path, cc, "_", i, ".csv")
      
      if (!file.exists(filepath)) {
        # Break the loop if the file does not exist
        break
      }
      
      print(paste0(cc, " document: ", i))
      sub_corpus <- NULL
      
      # Import and remove redundant columns & rows
      try({
        sub_corpus <- as.data.table(read.csv(filepath))
        sub_corpus$date <- as.Date(sub_corpus$date)
        sub_corpus[, update_date := as.Date(today())]
        
        corpus_new <- rbind(corpus_new, sub_corpus)
      }, silent = TRUE)
      
      i <- i + 1
    }
  }
}

remove(sub_corpus)
# Load vintage data and update it with new data -----------------------------------------------------------


if (first_ever_run == 1){
  corpus <- corpus_new
  corpus[, (col_date):=lapply(.SD, as.character), .SDcols=col_date]
  write.csv(corpus, paste0(wd, vintage_path, "corpus_",today,".csv"), row.names = FALSE, na = "")
  
} else{
  corpus <-  as.data.table(read.csv(paste0(wd, vintage_path, "corpus_",vintage,".csv")))
  corpus$update_date <- as.Date(corpus$update_date)
  corpus$date <- as.Date(corpus$date)
  corpus_new <- corpus_new[!corpus, on = c("newspaper", "body")]
  corpus <- rbind(corpus, corpus_new)
  dup_rows <- corpus[duplicated(corpus[, c("newspaper", "body")]), ]
  if(nrow(dup_rows)>0){
    print("These are the duplicated rows")
    print(dup_rows)
    setkeyv(corpus, c("newspaper","body", "update_date")) # Convert your data.table to a keyed data.table based on "newspaper" and "title" and "update_date"
    corpus <- corpus[!duplicated(corpus[,.(newspaper, body)], fromLast = TRUE)] # Remove duplicates and keep the latest updated document
    setkey(corpus, NULL) # Clear the key from the data.table
    dup_rows <- corpus[duplicated(corpus[, c("newspaper", "body")]), ]
    
    if(nrow(dup_rows)>0){
      print("There are still duplicated rows..These are the ones:")
      print(dup_rows)
      stop("Please check duplicated country date rows manually to find out which row to pick. Automatic updating failed.")
    }
    
  } else{
    remove(dup_rows)
  }
  print("These is the newly retrieved data that is appended to the vintage data")
  print(corpus_new)
  
  if (update_vintage == 1){
    corpus[, (col_date):=lapply(.SD, as.character), .SDcols=col_date]
    write.csv(corpus, paste0(wd, vintage_path, "corpus_",today,".csv"), row.names = FALSE, na = "")
  }
}

remove(corpus_new)
remove(dup_rows)

# Data transformations  -----------------------------------------------------------

corpus <- corpus[order(newspaper, date),]
corpus <- unique(corpus)


write.csv(corpus, paste0(wd, output_path, "corpus",".csv"), row.names = FALSE, na = "")


# Converting articles to sentences -----------------------------------------------------------

if (tokenise_operation == 1){

filtered_df <- subset(corpus, length < 3000) # To avoid the really long articles I am capping these at 3000 characters but we can think of a better way to control for this

# now adding a coloumn called 'sentences' extracting each sentence from an article body and saving the resulting data in new_df

n_rows <- nrow(filtered_df)
new_rows <- list()

for (i in 1:n_rows) {
  tryCatch({
    # Tokenize the text into sentences
    sentences <- unlist(tokenize_sentences(filtered_df$body[i]))
    
    # Remove '\n' and split into sentences
    sentences <- unlist(strsplit(gsub("\\\\n", "", filtered_df$body[i]), "\\.\\s*"))
    
    # Remove sentences containing "End of Document"
    sentences <- sentences[!grepl("End of Document", sentences)]
    
    # Create new rows for each sentence
    for (sentence in sentences) {
      # Create a new row with the same values as the original row
      new_row <- filtered_df[i, ]
      # Assign the current sentence to the 'sentence' column
      new_row$sentence <- sentence
      # Append the new row to the list
      new_rows <- append(new_rows, list(new_row))
    }
  }, error = function(e) {
    # If an error occurs, create a row with NA for sentence
    new_row <- filtered_df[i, ]
    new_row$sentence <- NA
    new_rows <- append(new_rows, list(new_row))
  }, warning = function(w) {
    # If a warning occurs, create a row with NA for sentence
    new_row <- filtered_df[i, ]
    new_row$sentence <- NA
    new_rows <- append(new_rows, list(new_row))
  })
}

# Combine the new rows into a dataframe
new_df <- do.call(rbind, new_rows)


# Converting sentences to words -----------------------------------------------------------

# Adding another dimension of words by tokenizing the sentences

new_df_word <- new_df %>%
  mutate(text = sentence) %>%
  unnest_tokens(word, text, token = "words")
}
