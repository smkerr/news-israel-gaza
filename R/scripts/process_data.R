# Load and process data ========================================================
# TODO: articles from before September included...

# Setup ------------------------------------------------------------------------
# define new outlets 
#media_list <- c("The Guardian (London)", "Al Jazeera English", "The New York Times", 
#                #"The Straits Times (Singapore)", "The Times of India (TOI)", 
#                #"South China Morning Post", "Die Welt (English)"
#                )
news_list <- c("Al Jazeera English", "The Guardian") # test 

# params
convert_rtf <- FALSE # convert RTF files to TXT files
update_raw_data <- TRUE # convert TXT files to CSV files
load_raw_data   <- TRUE # combine CSV files into single corpus (CSV)
first_ever_run <- TRUE # whether to create new 'corpus.csv' from scratch
update_vintage <- FALSE # whether to save a snapshot of 'corpus.csv' 
vintage_date <- lubridate::today() # date of vintage you would like to use

# load packages
pacman::p_load(
  data.table, 
  dplyr,
  glue,
  here, 
  lubridate, 
  readr,
  stringr, 
  striprtf,
  tibble,
  tidyr
)

# load functions
source("R/funcs/extract_articles.R")

# 1) Convert RTF files to TXT files (if needed) --------------------------------

if (convert_rtf == TRUE) {
  
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

# 2) Retrieve text from news articles ------------------------------------------

# define TXT files
text_list <- list.files(here("data/raw"), full.names = FALSE)

if (update_raw_data == TRUE) {
  
  # for each news source in list ....
  for (news_source in news_list) {
    
    # create subset of texts for the news source
    text_sub <- text_list[str_detect(text_list, news_source)]
    
    for (text in text_sub) {
      
      # define I/O paths
      in_path <- here("data/raw", text) # file path
      out_path <- here("data/vintages", str_replace(text, "txt", "csv")) # output path
      
      # status message
      print(paste0("Updating latest data for ", text))
      
      tryCatch({
        articles_df <- extract_article_lines(in_path, news_source)
        write.csv(articles_df, out_path, row.names = FALSE, na = "")
      }, error = function(cond) {
        message(paste0("Code returned an error message:"))
        message(cond)
      }, warning = function(cond) {
        message(paste0("Code returned a warning message:"))
        message(cond)
      })
      
    }
    
  }
  
  # clean up global env
  remove(articles_df)
  
}

# 3) Combine CSV files into a single corpus ------------------------------------

# initialize empty dt
corpus_new <- data.table()

# define CSV files
csv_list <- list.files(here("data/vintages"), full.names = FALSE)

# create corpus
if (load_raw_data == TRUE) {
  
  # for each news source in list ....
  for (news_source in news_list) {
    
    # create subset of CSV files for the news source
    csv_sub <- csv_list[str_detect(csv_list, news_source)]
    
    for (csv in csv_sub) {
      
      # define file path
      file_path <- here("data/vintages", csv) # file path
      
      # status message
      print(paste0("Incorporating ", csv, " into corpus."))
      
      # load text data
      temp_corpus <- read_csv(file_path)
      
      # add col for date of update
      temp_corpus$update_date <- today()
      
      # append to corpus
      corpus_new <- rbind(corpus_new, temp_corpus)
     
    }
  }
  
  # clean up global env
  remove(temp_corpus)
  
}

# 4) Create corpus (by updating old data or replacing with new data) -----------

if (first_ever_run == TRUE) {
  
  corpus <- corpus_new # new corpus becomes our corpus
  write_csv(corpus, here("data", paste0("corpus_", today(), ".csv"))) # save corpus as csv
  
} else {
  
  corpus <- read_csv(here("data", paste0("corpus_", vintage_date, ".csv"))) # load latest corpus
  corpus_new <- corpus_new |> # extract new articles only
    filter(
      !newspaper %in% unique(corpus$newspaper),
      !title %in% unique(corpus$title)
      )
  corpus <- rbind(corpus, corpus_new) # combine latest corpus with new articles
  
  # status message
  print("These is the newly retrieved data that is appended to the vintage data")
  head(corpus_new)
  
  if (update_vintage == TRUE) {
    # save snapshot of corpus
    write_csv(corpus, here("data", paste0("corpus_", today(), ".csv")))
  }
  
}

# clean up global env
if (exists("corpus_new")) remove(corpus_new)

# 5) Convert corpus to Document-Feature Matrix ---------------------------------
library(quanteda)
library(tidytext)

# define custom stop words
custom_stopwords <- c("")

# create Document Feature Matrix
dfmat <- corpus |> 
  rename(text = line) |> 
  corpus() |> 
  tokens() |> 
  tokens_remove(pattern = c(stopwords("en"), custom_stopwords), min_nchar = 5) |> # remove English stopwords
  tokens_wordstem(language = "english") |> 
  dfm() |> 
  tidy()

# save DFM 
saveRDS(dfmat, here("data", paste0("DFM_", today(), ".rds")))
