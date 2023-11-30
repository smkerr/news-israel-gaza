# Function to read the file and extract articles into a data.table
# 'newspaper_name' parameter is the exact name of the newspaper as it appears in the file
extract_articles_TGD <- function(filepath, newspaper_name) {
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
      current_article$date <- as.Date(mdy_hm(line, tz = "GMT"))
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


extract_articles_ALJ <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  exclude_next_line <- FALSE  # Flag to exclude lines under "Notes"
  first_line_in_body <- TRUE  # Flag to skip the first line in the body
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (startsWith(line, "Notes")) {
      # Skip the "Notes" section in Al Jazeera articles and set the flag to exclude lines
      exclude_next_line <- TRUE
    } else if (startsWith(line, "Newstex Blogs")) {
      # Ignore "Newstex Blogs" line in Al Jazeera articles
      next
    } else if (startsWith(line, "Link to the original story.")) {
      # Ignore "Newstex Blogs" line in Al Jazeera articles
      next  
    } else if (startsWith(line, "Al Jazeera English")) {
      # Start of a new Al Jazeera article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = prev_line, newspaper = line, date = "", section = "", length = NA, byline = "", highlight = "", body = "", load_date = "")
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
      first_line_in_body <- TRUE  # Reset the flag when starting a new article
    } else if (capture_next_line_as_date) {
      # Parse Al Jazeera date format
      current_article$date <- as.Date(mdy_hm(line, tz = "EST"))
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Length:", line)) {
      current_article$length <- as.numeric(gsub("\\D", "", line))
    } else if (grepl("^Byline:", line)) {
      current_article$byline <- sub("Byline:\\s*", "", line)
    } else if (line == "Body") {
      reading_body <- TRUE
      exclude_next_line <- FALSE  # Reset the flag when entering the body
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
    } else if (reading_body && !exclude_next_line) {
      if (first_line_in_body) {
        first_line_in_body <- FALSE
        next  # Skip the first line in the body
      } else {
        current_article$body <- paste(current_article$body, line, sep = "\\n")
      }
    }
    prev_line <- line
  }
  
  # Add the last article if exists
  if (length(current_article) > 0) {
    articles[[length(articles) + 1]] <- current_article
  }
  
  # Convert list of Al Jazeera articles to data.table
  dt <- rbindlist(lapply(articles, as.data.table), fill = TRUE)
  return(dt)
}



extract_articles_SCM <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (grepl("^South China Morning Post", line)) {
      # Start of a new article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = "", newspaper = line, date = "", section = "", length = NA, byline = "", highlight = "", body = "", load_date = "")
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- as.Date(mdy(line))
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Section:", line)) {
      current_article$section <- sub("Section:\\s*", "", line)
    } else if (grepl("^Length:", line)) {
      #current_article$length <- paste0(as.numeric(gsub("\\D", "", line)), " words")
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
  }
  # Add the last article if it exists
  if (length(current_article) > 0) {
    articles[[length(articles) + 1]] <- current_article
  }
  
  # Convert list of articles to data.table
  dt <- rbindlist(lapply(articles, as.data.table), fill = TRUE)
  return(dt)
}


extract_articles_TST <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (startsWith(line, "The Straits Times (Singapore)")) {
      # Start of a new article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = prev_line, newspaper = line, date = "", section = "", length = NA, byline = "", highlight = "", body = "", load_date = "")
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- as.Date(mdy(line))
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Section:", line)) {
      current_article$section <- sub("Section:\\s*", "", line)
    } else if (grepl("^Length:", line)) {
      #current_article$length <- paste0(as.numeric(gsub("\\D", "", line)), " words")
      current_article$length <- as.numeric(gsub("\\D", "", line))
    } else if (grepl("^Byline:", line)) {
      current_article$highlight <- sub("Byline:\\s*", "", line)
    } else if (line == "Body") {
      reading_body <- TRUE
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
    } else if (reading_body) {
      current_article$body <- paste(current_article$body, line, sep = "\\n")
    }
    prev_line <- line
  }
  # Add the last article if it exists
  if (length(current_article) > 0) {
    articles[[length(articles) + 1]] <- current_article
  }
  
  # Convert list of articles to data.table
  dt <- rbindlist(lapply(articles, as.data.table), fill = TRUE)
  return(dt)
}


