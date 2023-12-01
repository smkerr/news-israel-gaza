# Function to read the file and extract articles into a data.table
# 'newspaper_name' parameter is the exact name of the newspaper as it appears in the file
extract_articles_TGD <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  end_of_article <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (line == newspaper_name) {
      # End of the current article
      end_of_article <- TRUE
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
      end_of_article <- FALSE
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
      end_of_article <- TRUE
    } else if (reading_body && !end_of_article) {
      current_article$body <- paste(current_article$body, line, sep = "\\n")
    }
    
    if (end_of_article) {
      reading_body <- FALSE
      end_of_article <- FALSE
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



extract_articles_TOI <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  end_of_article <- FALSE
  
  for (line in lines) {
    if (line == "") {
      next
    } else if (line == newspaper_name) {
      # End of the current article, process and remove unwanted lines
      if (reading_body) {
        current_article$body <- gsub("\\(via media reports\\) For Reprint Rights: timescontent.com", "", current_article$body)
        current_article$body <- gsub("For Reprint Rights: timescontent.com", "", current_article$body)
      }
      # Start of a new article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = prev_line, newspaper = line, date = "", section = "", length = NA, byline = NA, highlight = NA, body = "", load_date = NA)
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
      end_of_article <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- as.Date(mdy(line))
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Section:", line)) {
      current_article$section <- sub("Section:\\s*", "", line)
    } else if (grepl("^Length:", line)) {
      current_article$length <- as.numeric(gsub("\\D", "", line))
    } else if (line == "Body") {
      reading_body <- TRUE
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
      end_of_article <- TRUE
    } else if (reading_body && !end_of_article) {
      current_article$body <- paste(current_article$body, line, sep = "\\n")
    }
    
    if (end_of_article) {
      # Process and remove unwanted lines at the end of article
      current_article$body <- gsub("\\(via media reports\\) For Reprint Rights: timescontent.com", "", current_article$body)
      current_article$body <- gsub("For Reprint Rights: timescontent.com", "", current_article$body)
      reading_body <- FALSE
      end_of_article <- FALSE
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



extract_articles_DWE <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  reading_body <- FALSE
  end_of_article <- FALSE
  
  for (line in lines) {
    # Replace non-breaking spaces with regular spaces
    line <- gsub("<a0>", " ", line)
    
    if (line == "") {
      next
    } else if (line == newspaper_name) {
      # End of the current article, process and remove unwanted lines
      if (reading_body) {
        current_article$body <- gsub("Disclaimer: Translation automatically generated. Axel Springer is not liable for any automatically generated translation of written text,", "", current_article$body)
        current_article$body <- gsub("audio tracks or other translatable media items. Legally binding is exclusively the original German text or spoken word.", "", current_article$body)
        current_article$body <- gsub("Original full page PDF", "", current_article$body)
        current_article$body <- gsub("forum@welt.de \\(forum@welt.de\\)", "", current_article$body)
      }
      # Start of a new article
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = prev_line, newspaper = line, date = "", section = "", length = NA, byline = "", highlight = NA, body = "", load_date = NA)
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- as.Date(mdy(line))
      capture_next_line_as_date <- FALSE
    } else if (grepl("^Section:", line)) {
      current_article$section <- sub("\\?", "", sub("Section:\\s*", "", line))
    } else if (grepl("^Length:", line)) {
      current_article$length <- as.numeric(gsub("\\D", "", line))
    } else if (grepl("^Byline:", line)) {
      current_article$byline <- sub("\\?", "", sub("Byline:\\s*", "", line))
    } else if (line == "Body") {
      reading_body <- TRUE
    } else if (grepl("^Load-Date:", line)) {
      current_article$load_date <- sub("Load-Date:\\s*", "", line)
      end_of_article <- TRUE
    } else if (reading_body && !end_of_article) {
      current_article$body <- paste(current_article$body, line, sep = "\\n")
    }
    
    if (end_of_article) {
      # Process and remove unwanted lines at the end of article
      current_article$body <- gsub("Disclaimer: Translation automatically generated. Axel Springer is not liable for any automatically generated translation of written text,", "", current_article$body)
      current_article$body <- gsub("audio tracks or other translatable media items. Legally binding is exclusively the original German text or spoken word.", "", current_article$body)
      current_article$body <- gsub("Original full page PDF", "", current_article$body)
      current_article$body <- gsub("forum@welt.de \\(forum@welt.de\\)", "", current_article$body)
      reading_body <- FALSE
      end_of_article <- FALSE
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


extract_articles_NYT <- function(filepath, newspaper_name) {
  lines <- readLines(filepath, warn = FALSE)
  articles <- list()
  current_article <- list()
  capture_next_line_as_date <- FALSE
  capture_next_line_as_title <- FALSE
  reading_body <- FALSE
  end_of_article <- FALSE
  skip_line <- FALSE
  convert_date <- function(date_string) {
    # Extract just the "Month DD, YYYY" part of the string
    extracted_date <- str_extract(date_string, "[[:alpha:]]+ [[:digit:]]{1,2}, [[:digit:]]{4}")
    
    # Convert the extracted string to a date
    date_object <- mdy(extracted_date)
    
    return(date_object)
  }
  
  for (line in lines) {
    if (line == "") {
      next
      
    }else if (grepl("^Copyright 2023 The New York Times Company", line)){
      next
      
    }else if (grepl("^The New York Times", line) && !grepl("on the Web", line) && !grepl("on the Web", line) && 
              !grepl("has documented other episodes", line) && !grepl("has verified that", line) &&
              !grepl("requested an interview", line) && !grepl("was unable to verify", line)) {
      if (length(current_article) > 0) {
        articles[[length(articles) + 1]] <- current_article
      }
      current_article <- list(title = "", newspaper = line, date = "", section = "", length = NA, byline = "", highlight = NA, body = "", load_date = NA)
      capture_next_line_as_date <- TRUE
      reading_body <- FALSE
    } else if (capture_next_line_as_date) {
      current_article$date <- convert_date(line)  # Capturing the date
      capture_next_line_as_date <- FALSE
      capture_next_line_as_title <- TRUE  # Prepare to check for the title in the next line
    } else if (capture_next_line_as_title) {
      if (grepl("^Section:", line)) {
        # The line is a section header, indicating no title present
        capture_next_line_as_title <- FALSE
        current_article$section <- sub("Section:\\s*", "", line)
      } else {
        current_article$title <- line  # Capturing the title
        capture_next_line_as_title <- FALSE
      }
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
      end_of_article <- TRUE
    } else if (reading_body && !end_of_article) {
      if (grepl("^Graphic", line) || grepl("PHOTOS", line) || grepl("PHOTO", line) || 
          grepl("The Times is committed to publishing", line) || 
          grepl("Follow The New York Times Opinion section on", line) ||
          grepl("\\bcontributed reporting from\\b", line)) {
        skip_line <- TRUE
      }
      if (!skip_line) {
        current_article$body <- paste(current_article$body, line, sep = "\\n")
      }
    }
    
    if (end_of_article) {
      reading_body <- FALSE
      end_of_article <- FALSE
      skip_line <- FALSE
    }
    
  }
  if (length(current_article) > 0) {
    articles[[length(articles) + 1]] <- current_article
  }
  
  dt <- rbindlist(lapply(articles, as.data.table), fill = TRUE)
  return(dt)
}





