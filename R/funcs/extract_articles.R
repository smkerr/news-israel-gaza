# Function to extract articles =================================================

extract_article_lines <- function(filepath, news_source) {
  
  # check whether news source is valid
  if (!news_source %in% c("Al Jazeera English", "The Guardian", "The New York Times")) {
    return("News source not available.")
  }
  
  # read in lines
  lines <- readLines(filepath, warn = FALSE)
  
  # Al Jazeera -----------------------------------------------------------------
  if (news_source == "Al Jazeera English") {
    
    # discard everything before start of relevant text
    first_line <- lines[lag(lines, n = 1) == "" & # define start of relevant text ...
                          lag(lines, n = 2) == "" & 
                          lag(lines, n = 3) == "" & 
                          lag(lines, n = 4) == ""] # ... relevant text always starts after four empty lines
    first_line <- first_line[!is.na(first_line)] # remove NAs
    line_start <- match(first_line[1], lines) # index first relevant line
    lines <- lines[line_start:length(lines)] # start at relevant text
    
    # wrangle data 
    df <- lines |> 
      as_tibble_col(column_name = "line") |> 
      mutate(
        newspaper = "Al Jazeera English", # news source: Al Jazeera
        title = ifelse(str_detect(lead(line, default = "Newstex Blogs"), "^Newstex Blogs $"), line, NA), # extract article title
        published_date = ifelse(str_detect(lag(line), "^Al Jazeera English$"), line, NA), # extract date published
        load_date = ifelse(str_detect(line, "^Load"), line, NA), # extract article date loaded
        length = ifelse(str_detect(line, "^Length:"), line, NA), # extract article length
        byline = ifelse(str_detect(line, "^Byline:"), line, NA), # extract article byline 
        section = NA # add section column (not relevant here but necessary to merge with other news sources' dfs)
      ) |> 
      # fill in metadata
      fill(title, published_date, load_date, length, byline, .direction = "downup") |> 
      # fix data formats
      mutate(
        published_date = mdy_hm(published_date, tz = "EST"), # convert to date
        load_date = str_remove(load_date, "^Load-Date:") |> mdy(), # convert to date
        length = str_remove(length, "Length:"), # extract length
        length = str_remove(length, " words"),
        length = as.numeric(str_trim(length)), # convert to numeric
        byline = str_remove(byline, "^Byline:") # extract byline (for Al Jazeera: author name)
      )  |> 
      # remove irrelevant lines
      slice(-1:-12, .by = title) |> # remove metadata
      filter(
        line != "", # remove empty lines
        !str_detect(line, "( Al Jazeera English  — Delivered by  Newstex )"), # remove boilerplate
        !str_detect(line, "Link to the original story."),
        !str_detect(line, "Notes"),
        !str_detect(line, "^Load-Date"),
        !str_detect(line, "End of Document"),
        !str_detect(line, "^The views expressed in any and all content distributed by Newstex and its re-distributors")
      ) |> 
      as.data.table()
    
    return(df)
    
  } else if (news_source == "The Guardian") {
    
    # discard everything before start of relevant text
    first_line <- lines[lag(lines, n = 1) == "" & # define start of relevant text ...
                          lag(lines, n = 2) == "" & 
                          lag(lines, n = 3) == "" & 
                          lag(lines, n = 4) == ""] # ... relevant text always starts after four empty lines
    first_line <- first_line[!is.na(first_line)] # remove NAs
    line_start <- match(first_line[1], lines) # index first relevant line
    lines <- lines[line_start:length(lines)] # start at relevant text
    
    # wrangle data 
    df <- lines |> 
      as_tibble_col(column_name = "line") |> 
      mutate(
        newspaper = "The Guardian", # news source: The Guardian
        title = ifelse(str_detect(dplyr::lead(line, default = NA), "^The Guardian \\(London\\)"), line, NA), # extract article title
        published_date = ifelse(str_detect(dplyr::lag(line), "^The Guardian \\(London\\)"), line, NA), # extract date published
        load_date = ifelse(str_detect(line, "^Load\\-Date:"), line, NA), # extract article date loaded
        length = ifelse(str_detect(line, "^Length:"), line, NA), # extract article length
        byline = ifelse(str_detect(line, "^Byline:"), line, NA), # extract article byline 
        section = ifelse(str_detect(line, "^Section:"), line, NA) # extract article section 
      ) |> 
      # fill in metadata
      fill(title, published_date, load_date, length, byline, section, .direction = "downup") |> 
      # fix data formats
      mutate(
        published_date = mdy_hm(published_date, tz = "EST"), # convert to date
        load_date = str_remove(load_date, "^Load\\-Date:") |> mdy(), # convert to date
        length = str_remove(length, "Length:"), # extract length
        length = str_remove(length, " words"),
        length = str_trim(length), 
        byline = str_remove(byline, "^Byline:") # extract byline (for Al Jazeera: author name)
      ) |>
      # remove irrelevant lines
      slice(-1:-15, .by = title) |> # remove metadata
      filter(
        line != "", # remove empty lines
        !str_detect(line, "^Load\\-Date"),
        !str_detect(line, "End of Document")
      ) |> 
      as.data.table()
    
    return(df)
    
  } else if (news_source == "The New York Times") {
    
    next
    
  } else {
    
    return("News source provided is not valid.")
    
  }
  
}

# test: The Guardian
#filepath <- here("data/raw/The Guardian_1.txt")
#extract_article_lines(filepath, news_source = "The Guardian")
