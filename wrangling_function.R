# Function to read the file and extract articles into a data.table
# 'newspaper_name' parameter is the exact name of the newspaper as it appears in the file
extract_articles <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (line == newspaper_name) {
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

