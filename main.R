#===================================================================================================== 
#
#  >>>>>>>>>>>>>>>>  How has the Israel-Palestine conflict been covered in the media? >>>>>>>>>>>>>>>>>
#
#
#           --------------Data Loading and processing---------------
#
# 
#                  1) Some settings on the loading and processing of data
#                  2) Retrieve latest country level data from original sources (sourcing country R files)
#                  3) Load and process data into one master file
#                  4) Upload to DARWIN
#                  5) commented out template for downloading data to Excel to create Excel based charts
# 
# 
# 
#
# Author: Kai Foerster, DGMF/MAP
#         Wojciech Pachowiak, DGIS/EBS (Many thanks for your support in setting up the selenium dynamic webscraping for IT and ES)
#
# Version date:	  16/02/2023
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Controls ----------------------------------------------------------------

#.................................MANUALLY UPDATE WHEN COUNTRY IS ADDED OR REMOVED
media_list <- c("The Guardian (London)")

# Default settings and connections 
update_raw_data <- 1
# update_vintage <- 0
# load_raw_data   <- 1
# upload_darwin   <- 1
# first_ever_run <- 0
# first_upload    <- 0
# vintage <- "2023-07-03"
output_path     <- "/data/"
vintage_path     <- "/data/vintages/"
raw_path     <- "/data/raw/"
filtered_path <- "/data/filtered/"


# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("data.table") #this version also loads packages from the country files as these seemed to fail loading.

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# Setting directories ----------------------------------------------------
wd <- getwd() # "your path"
setwd(wd)

# Load functions
source("wrangling_function.R")


# ===========================================================================
#   2) Retrieve latest country level data from original sources (sourcing country R files)
# ===========================================================================

# Update data for selected newspapers ------

if (update_raw_data == 1){
  for (cc in media_list){
    
    print(paste0("Updating latest data for ", cc))
    tryCatch(source(paste0("code/country codes/",cc,".R")), error = function(cond){
      message(paste0("Code seems to break"))
      message("Here's the original error message:")
      message(cond)
      
    },warning=function(cond) {
      message(paste0("Code returned warning"))
      message("Here's the original warning message:")
      message(cond)
    })
  }
  
}





# ===========================================================================
#   3) Load and process data into one master file
# ===========================================================================

# Load new data -----------------------------------------------------------


rrep_hf_dt_new <- data.table()

#Load full data
if (load_raw_data == 1){
  for (cc in ctr_list){
    
    print(cc)
    
    #import and remove redundant columns & rows
    try(data_ctr<- as.data.table(read.csv(paste0(wd, vintage_path, cc,".csv"))), silent = TRUE)
    data_ctr$ref_date <- as.Date(data_ctr$ref_date)
    data_ctr <- data_ctr[!is.na(rrep_hf),min_date:= min(ref_date)]
    data_ctr <- data_ctr[ref_date >= min_date]
    data_ctr[,update_date:= as.Date(Sys.Date())]
    
    rrep_hf_dt_new <- rbind(rrep_hf_dt_new,data_ctr)
  }
  
}

# Load vintage data -----------------------------------------------------------

if (first_ever_run == 1){
  rrep_hf_dt <- rrep_hf_dt_new
  rrep_hf_dt[, (col_date):=lapply(.SD, as.character), .SDcols=col_date]
  write.xlsx(rrep_hf_dt, paste0(wd, vintage_path, "rrep_hf_",today,".xlsx"), rowNames = FALSE)
  
} else{
  rrep_hf_dt <-  as.data.table(read.xlsx(paste0(wd, vintage_path, "rrep_hf_",vintage,".xlsx")))
  rrep_hf_dt[, (col_date):=lapply(.SD, as.Date), .SDcols=col_date]
  rrep_hf_dt_new <- rrep_hf_dt_new[!rrep_hf_dt, on = c("country", "ref_date", "rrep_hf")]
  rrep_hf_dt <- rbind(rrep_hf_dt, rrep_hf_dt_new)
  dup_rows <- rrep_hf_dt[duplicated(rrep_hf_dt[, c("ref_date", "country")]), ]
  if(nrow(dup_rows)>0){
    print("These are the duplicated rows")
    print(dup_rows)
    setkeyv(rrep_hf_dt, c("country", "ref_date", "update_date")) # Convert your data.table to a keyed data.table based on "country" and "ref_date" and "update_date"
    rrep_hf_dt <- rrep_hf_dt[!duplicated(rrep_hf_dt[,.(country, ref_date)], fromLast = TRUE)] # Remove duplicates and keep the latest updated value of rrep_hf
    setkey(rrep_hf_dt, NULL) # Clear the key from the data.table
    dup_rows <- rrep_hf_dt[duplicated(rrep_hf_dt[, c("ref_date", "country")]), ]
    
    if(nrow(dup_rows)>0){
      print("There are still duplicated rows..These are the ones:")
      print(dup_rows)
      stop("Please check duplicated country date rows manually to find out which row to pick. Automatic updating failed.")
    }
    
  } else{
    remove(dup_rows)
  }
  print("These is the newly retrieved data that is appended to the vintage data")
  print(rrep_hf_dt_new)
  
  if (update_vintage == 1){
    rrep_hf_dt[, (col_date):=lapply(.SD, as.character), .SDcols=col_date]
    write.xlsx(rrep_hf_dt, paste0(wd, vintage_path, "rrep_hf_",today,".xlsx"), rowNames = FALSE)
  }
}

remove(rrep_hf_dt_new)

# Data transformations  -----------------------------------------------------------

rrep_hf_dt <- rrep_hf_dt[,max_date:= Sys.Date()]
rrep_hf_dt <- rrep_hf_dt[order(country, ref_date),.(ref_date, country, rrep_hf)]
rrep_hf_dt <- unique(rrep_hf_dt)
rrep_hf_dt <- rrep_hf_dt[country!= "SK",rrep_hf_g1m := map_g(rrep_hf, c(1, "m")), by = country]
rrep_hf_dt <- rrep_hf_dt[country== "SK",rrep_hf_g1m := map_g(rrep_hf, c(1, "q")), by = country]
rrep_hf_dt <- rrep_hf_dt[country!= "SK",rrep_hf_g3m := map_g(rrep_hf, c(3, "m")), by = country]
rrep_hf_dt <- rrep_hf_dt[country== "SK",rrep_hf_g3m := map_g(rrep_hf, c(1, "q")), by = country]
rrep_hf_dt <- rrep_hf_dt[country!= "SK",rrep_hf_g12m := map_g(rrep_hf, c(12, "m")), by = country]
rrep_hf_dt <- rrep_hf_dt[country== "SK",rrep_hf_g12m := map_g(rrep_hf, c(4, "q")), by = country]
rrep_hf_dt <- rrep_hf_dt[country != "SK", rrep_hf_idx:= 100*(rrep_hf/rrep_hf[ref_date==as.Date("2021-12-31")]), by = country]
rrep_hf_dt <- rrep_hf_dt[country == "SK", rrep_hf_idx:= 100*(rrep_hf/rrep_hf[ref_date==as.Date("2021-12-31")]), by = country]
rrep_hf_dt$ref_date <- as.character(rrep_hf_dt$ref_date)
write.xlsx(rrep_hf_dt, paste0(wd, output_path, "rrep_hf.xlsx"), rowNames = FALSE)


rrep_hf_dt_MAD <- rrep_hf_dt[country != "SK", .(country, ref_date, rrep_hf, rrep_hf_idx)]
write.xlsx(rrep_hf_dt_MAD, paste0(wd, output_path, "rrep_hf_MAD.xlsx"), rowNames = FALSE)

# ===========================================================================
#   4) Upload to DARWIN
# ===========================================================================

# upload to DARWIN ------------------------- 

if (upload_darwin==1 & first_upload ==1){
  
  print("A file should already be on DARWIN")
  
} else if (upload_darwin==1 & first_upload ==0){
  filename1 <- paste0(wd, output_path,"rrep_hf_MAD.xlsx")
  file.update.DARWIN(file = filename1, Darwin_file)
  
  rm(filename1)
  
}


filename1 <- paste0(wd, output_path,"rrep_hf.xlsx")
file.update.DARWIN(file = filename1, PAC_Darwin_file_long)
rm(filename1)

# ===========================================================================
#   5) Template for downloading data to Excel to create Excel based charts
# ===========================================================================

# Export for SN ---------------------------------
rrep_hf_dt$ref_date <- as.Date(rrep_hf_dt$ref_date)
rrep_hf_w <- rrep_hf_dt[, .(country, ref_date, rrep_hf_idx)]
rrep_hf_w2 <- dcast(rrep_hf_w, ref_date ~ country, value.var = "rrep_hf_idx")
write.xlsx(rrep_hf_w2, paste0(wd, output_path, "rrep_hf_wide.xlsx"), rowNames = FALSE)


filename1 <- paste0(wd, output_path,"rrep_hf_wide.xlsx")
file.update.DARWIN(file = filename1, PAC_Darwin_file_wide)

rm(filename1)

