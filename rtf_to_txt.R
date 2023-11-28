# Script to convert RTF files to TXT files -------------------------------------

# load packages
pacman::p_load(
  here, 
  stringr,
  striprtf,
  tictoc
  )

# define file names
files_in <- list.files(path = here("data/rtf")) # RTF files (input)
files_out <- str_replace(files_in, "RTF", "txt") # TXT files (output)

# start time
tictoc::tic()

# for each RTF file...
for (i in seq_along(files_in)) {
  
  # skip if TXT file already exists
  if (file.exists(here("data/raw", files_out[i]))) next
  
  # read in RTF file
  temp_txt <- read_rtf(here("data/rtf", files_in[i]))
  
  # save as TXT file
  writeLines(temp_txt, here("data/raw", str_replace(files_out[i], "RTF", "txt")))
  
}

# end time 
tictoc::toc()
