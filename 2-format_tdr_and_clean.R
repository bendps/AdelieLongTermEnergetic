rm(list = ls())
library(tidyverse)
library(data.table)
library(tidymodels)
library(vroom)

if(Sys.info()["sysname"] == "Linux"){
  my_root <- "/media/bdupuis/1091_PhD_BD/"
}else{ my_root <- "E:/"}

#This script goal is to unify, format and clean TDR data as they were recorded with different loggers, variables names etc.

source(paste0(my_root,"Data/R_functions_toolbox.R")) #This sub-script contains some custom function for ZOC or converting pressure to depth for instance

#1 - format TDR ######################################################################################################################################
deployment <- fread("data/bird_info_tdr.csv")
deployment$tag_id <- NA

tdr_dir <- paste0(my_root, "Data/TDR_raw_data")
tdr_season <- list.files(tdr_dir) %>%
  str_subset("2014", negate = T)
my_season <- tdr_season[9]
for(my_season in tdr_season){
  #Load and format data for the different type of loggers
  if(my_season %in% c("CE_1998-99")){ #LL old logger
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/LL_pdt/"), pattern = "*.txt")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_pdt/", my_tdr_file))
      colnames(my_tdr_data) <- "depth"
      #get deployment info
      bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add timestamp to tdr data
        start_timestamp <- dmy_hm(sub_deployment$date_start_logger)
        end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
        my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
        
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "LLpdt", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2001-02")){ #LL old logger
    log_types <- list.files(paste0(tdr_dir, "/", my_season, "/CR/"))
    for (my_logger in log_types) {
      my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/", my_logger,"/"), pattern = "*.txt")
      for (my_tdr_file in my_tdr_raw_file) {
        #load tdr
        my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/", my_logger,"/", my_tdr_file))
        colnames(my_tdr_data) <- "depth"
        #get deployment info
        bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]
        
        sub_deployment <- deployment %>%
          filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
                 id == bird_id)
        
        if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
          #Add timestamp to tdr data
          start_timestamp <- dmy_hm(sub_deployment$date_start_logger)
          end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
          my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
          
          #Add a unique ID to tdr and deployment for match in future
          unique_id <- paste("ade",bird_id, gsub("_","",my_logger), sub_deployment$logger_id, my_season, sep = "_")
          my_tdr_data$tag_id <- unique_id
          deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                    deployment$id == bird_id)] <- unique_id
          saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
        }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
      }
    }
  }
  if(my_season %in% c("CE_2009-10")){ #LL logger
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/"), pattern = "*.TXT")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 10, col_names = "depth")
      
      #Add timestamp to tdr data
      timestamp_info <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 6, n_max = 2, col_names = F)
      start_timestamp <- dmy_hms(paste(str_replace_all(timestamp_info[1,3], "\"", ""), str_replace_all(timestamp_info[2,3], "\"", "")))
      end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
      my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, "_\\s*(.*?)\\s*-")[,2]
      
      sub_deployment <- deployment %>%
        filter(logger_id == sub("\\_.*", "", my_tdr_file),
               season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "LL",sub("\\_.*", "", my_tdr_file), my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        deployment$tag_id[which(deployment$logger_id == sub("\\_.*", "", my_tdr_file) &
                                deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2010-11")){ #LL but file sorted differently
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/"), pattern = "*.TXT")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 10, col_names = "depth")
      
      #Add timestamp to tdr data
      timestamp_info <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 6, n_max = 2, col_names = F)
      start_timestamp <- dmy_hms(paste(str_replace_all(timestamp_info[1,3], "\"", ""), str_replace_all(timestamp_info[2,3], "\"", "")))
      end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
      my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, "-ID\\s*(.*?)\\s*-")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "LL", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2011-12")){ #LL but file sorted differently
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/"), pattern = "*.TXT")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 10, col_names = "depth")
      
      #Add timestamp to tdr data
      timestamp_info <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 6, n_max = 2, col_names = F)
      start_timestamp <- dmy_hms(paste(str_replace_all(timestamp_info[1,3], "\"", ""), str_replace_all(timestamp_info[2,3], "\"", "")))
      end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
      my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, " ID\\s*(.*?)\\s* ")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "LL", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2012-13")){ #LL but file sorted differently
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/"), pattern = "*.TXT")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 10, col_names = "depth")
      
      #Add timestamp to tdr data
      timestamp_info <- read_table(paste0(tdr_dir, "/", my_season, "/CR/LL_M190/", my_tdr_file), skip = 6, n_max = 2, col_names = F)
      start_timestamp <- dmy_hms(paste(str_replace_all(timestamp_info[1,3], "\"", ""), str_replace_all(timestamp_info[2,3], "\"", "")))
      end_timestamp <- start_timestamp + seconds(nrow(my_tdr_data) - 1)
      my_tdr_data$timestamp <- seq(from = start_timestamp, to = end_timestamp, by = 1)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, "-\\s*(.*?)\\s*-Depth")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "LL", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2015-16")){ #WACU logger
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/Wacu/"), pattern = "*.txt")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- fread(paste0(tdr_dir, "/", my_season, "/CR/Wacu/", my_tdr_file))
      
      #convert timstamp to have date and time
      my_tdr_data$timestamp <- force_tz(as.POSIXct(as.numeric(my_tdr_data$DateTimeW), origin = "1904-01-01 11:00"),tz = "UTC") #DDU local time
      my_tdr_data$depth <- pressure2depth(my_tdr_data$mBar)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == paste0(bird_id, "\\"))
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "WACU", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        my_tdr_data <- my_tdr_data %>% dplyr::select(tag_id, timestamp, depth)
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == paste0(bird_id, "\\"))] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2016-17", "CE_2019-20", "CE_2020-21", "CE_2021-22", "CE_2022-23")){ #Axy logger
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/Axy/"), pattern = "*.csv")
    my_tdr_file <- my_tdr_raw_file[6] #temp for testing script ######################################################################################
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- fread(paste0(tdr_dir, "/", my_season, "/CR/Axy/", my_tdr_file))
      
      #remove useless data
      my_tdr_data <- my_tdr_data %>% drop_na(Pressure)
      my_tdr_data$Timestamp <- dmy_hms(my_tdr_data$Timestamp)
      
      my_tdr_data <- my_tdr_data %>%
        dplyr::select("Timestamp", "Pressure")
      colnames(my_tdr_data) <- tolower(colnames(my_tdr_data))
      
      #Get depth and remove pressure
      my_tdr_data$depth <- pressure2depth(my_tdr_data$pressure)
      
      #get deployment info
      if(my_season %in% c("CE_2016-17", "CE_2022-23", "CE_2023-24")){bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]}
      if(my_season %in% c("CE_2019-20", "CE_2021-22")){bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*-")[,2]}
      if(my_season %in% c("CE_2020-21")){bird_id <- str_match(my_tdr_file, "_\\s*(.*?)\\s*_")[,2]}
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "AXY", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        my_tdr_data <- my_tdr_data %>% dplyr::select(tag_id, timestamp, depth)
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2023-24")){ #Axy logger
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/Axy/"), pattern = "*.csv")
    my_tdr_file <- my_tdr_raw_file[6] #temp for testing script ######################################################################################
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- fread(paste0(tdr_dir, "/", my_season, "/CR/Axy/", my_tdr_file))
      
      #remove useless data
      my_tdr_data <- my_tdr_data %>% drop_na(Pressure)
      my_tdr_data$Timestamp <- dmy_hms(paste(my_tdr_data$Date, my_tdr_data$Time))
      
      my_tdr_data <- my_tdr_data %>%
        dplyr::select("Timestamp", "Pressure")
      colnames(my_tdr_data) <- tolower(colnames(my_tdr_data))
      
      #Get depth and remove pressure
      my_tdr_data$depth <- pressure2depth(my_tdr_data$pressure)
      
      #get deployment info
      if(my_season %in% c("CE_2016-17", "CE_2022-23", "CE_2023-24")){bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]}
      if(my_season %in% c("CE_2019-20", "CE_2021-22")){bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*-")[,2]}
      if(my_season %in% c("CE_2020-21")){bird_id <- str_match(my_tdr_file, "_\\s*(.*?)\\s*_")[,2]}
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "AXY", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        my_tdr_data <- my_tdr_data %>% dplyr::select(tag_id, timestamp, depth)
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2017-18")){ #Axy when sep of header and rest is different
    my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/Axy/"), pattern = "*.csv")
    for (my_tdr_file in my_tdr_raw_file) {
      #load tdr
      my_tdr_data <- vroom(paste0(tdr_dir, "/", my_season, "/CR/Axy/", my_tdr_file), num_threads = 18, show_col_types = F, delim = ",", col_names = F, skip = 1)
      tdr_header <- fread(paste0(tdr_dir, "/", my_season, "/CR/Axy/", my_tdr_file), nrows = 1, sep = ";", header = FALSE)
      colnames(my_tdr_data) <- tdr_header[1,]
      #remove useless data
      my_tdr_data <- my_tdr_data[,1:10] %>% filter(row_number() %% 25 == 1)
      my_tdr_data$Timestamp <- dmy_hms(my_tdr_data$Timestamp)
      
      my_tdr_data <- my_tdr_data %>%
        dplyr::select("Timestamp", "Pressure")
      colnames(my_tdr_data) <- tolower(colnames(my_tdr_data))
      
      #Get depth and remove pressure
      my_tdr_data$depth <- pressure2depth(my_tdr_data$pressure)
      
      #get deployment info
      bird_id <- str_match(my_tdr_file, "\\s*(.*?)\\s*_")[,2]
      
      sub_deployment <- deployment %>%
        filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
               id == bird_id)
      
      if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
        #Add a unique ID to tdr and deployment for match in future
        unique_id <- paste("ade",bird_id, "AXY", sub_deployment$logger_id, my_season, sep = "_")
        my_tdr_data$tag_id <- unique_id
        my_tdr_data <- my_tdr_data %>% dplyr::select(tag_id, timestamp, depth)
        deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                  deployment$id == bird_id)] <- unique_id
        saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
      }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
    }
  }
  if(my_season %in% c("CE_2018-19")){ #several loggers type this years because of cameras and dlw 
    my_folders <- list.files(paste0(tdr_dir, "/", my_season, "/CR/"))
    data_folder <- my_folders[4]  #temp for testing script ######################################################################################
    for (data_folder in my_folders) {
      my_tdr_raw_file <- list.files(paste0(tdr_dir, "/", my_season, "/CR/", data_folder,"/"), pattern = "*.csv")
      my_tdr_file <- my_tdr_raw_file[5] #temp for testing script ######################################################################################
      for (my_tdr_file in my_tdr_raw_file) {
        #load tdr
        my_tdr_data <- fread(paste0(tdr_dir, "/", my_season, "/CR/", data_folder,"/", my_tdr_file))
        #remove useless data
        my_tdr_data <- my_tdr_data %>% drop_na(Pressure)
        my_tdr_data$Timestamp <- dmy_hms(my_tdr_data$Timestamp)
        
        my_tdr_data <- my_tdr_data %>%
          dplyr::select("Timestamp", "Pressure")
        colnames(my_tdr_data) <- tolower(colnames(my_tdr_data))
        
        #Get depth and remove pressure
        my_tdr_data$depth <- pressure2depth(my_tdr_data$pressure)
        
        #get deployment info
        bird_id <- str_match(my_tdr_file, "_\\s*(.*?)\\s*_")[,2]
        
        sub_deployment <- deployment %>%
          filter(season == gsub("-","_",substr(my_season, 6, nchar(my_season))),
                 id == bird_id)
        
        if(nrow(sub_deployment) == 1 && !is.na(sub_deployment$mass_start_kg)){ #Only if we have deployment info
          #Add a unique ID to tdr and deployment for match in future
          unique_id <- paste("ade",bird_id, toupper(data_folder), sub_deployment$logger_id, my_season, sep = "_")
          my_tdr_data$tag_id <- unique_id
          my_tdr_data <- my_tdr_data %>% dplyr::select(tag_id, timestamp, depth)
          deployment$tag_id[which(deployment$season == gsub("-","_",substr(my_season, 6, nchar(my_season))) &
                                    deployment$id == bird_id)] <- unique_id
          saveRDS(my_tdr_data, paste0("data/raw_tdr/", unique_id, ".rds"))
        }else{print(paste("No deployment data for", my_tdr_file, "season", my_season))}
      } 
    }
  }
}

deployment_filtered <- deployment %>% filter(!is.na(tag_id))
write.csv(deployment_filtered, "data/bird_info_w_id_filtered.csv")

#2 - Add variables and check data ######################################################################################################################################
#Now that TDRs are clean, with only TIMESTAMP, DEPTH and TAGID, we add/compute variables we need for further analyses
##2.1 - Check/screen sensor data
deployment_filtered <- read_csv("data/bird_info_w_id_filtered.csv")

#For DEE
deployment_filtered$daily_dive_duration_hrs <- NA
deployment_filtered$daily_vert_mvt_sub_surface_m <- NA

raw_tdr_files <- list.files("data/raw_tdr/", pattern = "*.rds")
raw_tdr_files <- raw_tdr_files[-which(tools::file_path_sans_ext(raw_tdr_files) %in% unique(deployment_filtered$tag_id[which(!is.na(deployment_filtered$daily_dive_duration_hrs))]))]
my_tdr_file <- raw_tdr_files[2]

for(my_tdr_file in raw_tdr_files){
  my_tdr_data <- read_rds(paste0("data/raw_tdr/", my_tdr_file))
  
  visual_dive <- ggplot(data = my_tdr_data, aes(x = timestamp, y = -depth)) +
    scattermore::geom_scattermore() +
    scale_x_datetime(date_breaks = "1 days", date_labels = "%y/%m/%d") +
    labs(title = my_tdr_file)
  
  plot(visual_dive)
  
  #reset prompt
  dive_ok <- "n"
  
  deploy_ok <- readline(prompt = "Is the deployment useable ? (y/n) ")
  while (deploy_ok !=  "y" & deploy_ok != "n") {
    deploy_ok <- readline(prompt = "Is the deployment useable ? (y/n) ")
  }
  if(deploy_ok == "y"){
    cut_trip <- readline(prompt = "Several trip in data ? (y/n) ") #cut to keep only the 1st trip
    while (cut_trip !=  "y" & cut_trip != "n") {
      cut_trip <- readline(prompt = "Several trip in data ? (y/n) ") #cut to keep only the 1st trip
    }
    while(cut_trip == "y"){
      max_timestamp <- ymd_h(readline(prompt = "Max timestamp to keep ? (yy/mm/dd hh) "))
      first_trip <- my_tdr_data %>% filter(timestamp < max_timestamp)
      
      visual_dive <- ggplot() +
        scattermore::geom_scattermore(data = my_tdr_data, aes(x = timestamp, y = -depth), col = "grey") +
        scattermore::geom_scattermore(data = first_trip, aes(x = timestamp, y = -depth), col = "red") +
        labs(title = my_tdr_file)
      plot(visual_dive)
      cut_trip <- readline(prompt = "Several trip in data ? (y/n) ")
      if(cut_trip == "n"){
        my_tdr_data <- first_trip
        
        visual_dive <- ggplot(data = my_tdr_data, aes(x = timestamp, y = -depth)) +
          scattermore::geom_scattermore() +
          scale_x_datetime(date_breaks = "1 days", date_labels = "%y/%m/%d") +
          labs(title = my_tdr_file)
        
        plot(visual_dive)
        }
    }
    my_tdr_data$original_depth <- my_tdr_data$depth
    while (dive_ok == "n") {
      dive_ok <- readline(prompt = "Dive profile looks OK ? (y/n) ")
      old_depth <- my_tdr_data$depth
      
      while(dive_ok == "n"){
        reset_start <- readline(prompt = "Enter number of hours from start you want to reset depth to 0 --> ")
        reset_end <- readline(prompt = "Enter number of hours from end you want to reset depth to 0 --> ")
        
        my_tdr_data$depth[which(my_tdr_data$timestamp < (my_tdr_data$timestamp[1] + hours(reset_start)))] <- mean(my_tdr_data$depth[which(my_tdr_data$depth < 4)])
        my_tdr_data$depth[which(my_tdr_data$timestamp > (my_tdr_data$timestamp[nrow(my_tdr_data)] - hours(reset_end)))] <- mean(my_tdr_data$depth[which(my_tdr_data$depth < 4)])
        
        visual_dive <- ggplot() +
          scattermore::geom_scattermore(data = my_tdr_data, aes(x = timestamp, y = -original_depth), col = "black") +
          scattermore::geom_scattermore(data = my_tdr_data, aes(x = timestamp, y = -depth), col = "red") +
          labs(title = my_tdr_file)
        plot(visual_dive)
        
        dive_ok <- readline(prompt = "Dive profile looks OK ? (y/n) ")
        if(dive_ok == "n"){my_tdr_data$depth <- old_depth}
      }
    }
    
    my_tdr_clean <- my_tdr_data %>%
      select(-original_depth) %>% 
      relocate(tag_id, timestamp, depth) %>% 
      filter(!is.na(depth))
    my_tdr_clean$depth <- clean0depth(my_tdr_clean$depth)
    my_tdr_clean <- my_tdr_clean %>% filter(!is.na(depth))
    
    visual_dive <- ggplot() +
      scattermore::geom_scattermore(data = my_tdr_clean %>% filter(depth < 5), aes(x = timestamp, y = -depth)) +
      labs(title = my_tdr_file)
    plot(visual_dive)
    
    my_tdr_clean$dive <- ifelse(my_tdr_clean$depth >= 1, TRUE, FALSE) # Dive is 1M
    my_tdr_clean$change_bout <- ifelse(my_tdr_clean$dive != lag(my_tdr_clean$dive), 1, 0)
    my_tdr_clean$change_bout[1] <- 1
    my_tdr_clean$bout_id <- cumsum(my_tdr_clean$change_bout) #Here a bout is just a continuous period of dive/surface behavior
    
    #Type of bout info
    dive_label <- my_tdr_clean %>%
      group_by(bout_id) %>% 
      summarise(max_dive_depth = max(depth, na.rm = T), duration_sec = n(),
                timestamp_max_dive_depth = min(timestamp[which(depth == max(depth, na.rm = T))], na.rm = T))
    
    dive_label$type <- ifelse(dive_label$max_dive_depth <= 1, "surface",
                              ifelse(dive_label$max_dive_depth > 1 & dive_label$max_dive_depth <= 2, "sub-surface", "dive"))
    
    dive_label$type[1] <- "land"
    dive_label$type[nrow(dive_label)] <- "land"
    dive_label$type[which(dive_label$duration_sec > 21600)] <- "land" #if more than 6 hours bout, means its on land
    my_tdr_clean <- left_join(my_tdr_clean, dive_label[,c("bout_id", "type")], by = "bout_id")
    
    #Get param for RF
    #Create a vector to have the depth of t-1 in the same row to calculate the difference (vertical speed)
    prev_depth <- c(NA, my_tdr_clean$depth)
    my_tdr_clean$prev_depth <- prev_depth[-length(prev_depth)]
    my_tdr_clean <- my_tdr_clean %>% mutate(vert_speed = depth - prev_depth) #Calculate vertical speed
    
    #Same but the vertical acceleration
    prev_change <- c(NA, my_tdr_clean$vert_speed)
    my_tdr_clean$prev_change <- prev_change[-length(prev_change)]
    my_tdr_clean <- my_tdr_clean %>% mutate(vert_accel = vert_speed - prev_change) #Calculate vertical acceleration
    
    #Sd/mean of the vertical acceleration/speed over 5 sec
    my_tdr_clean$roll_sd_vert_accel <- rollapply(data = my_tdr_clean$vert_accel, width = 5, FUN = sd, fill = NA)
    my_tdr_clean$roll_mean_vert_accel <- rollapply(data = my_tdr_clean$vert_accel, width = 5, FUN = mean, fill = NA)
    my_tdr_clean$roll_mean_vert_speed <- rollapply(data = my_tdr_clean$vert_speed, width = 5, FUN = mean, fill = NA)
    
    my_tdr_clean <- my_tdr_clean %>% select(-c("prev_change", "prev_depth"))
    
    #Get dive_duration
    dive_dur_tibble <- my_tdr_clean %>% filter(depth >= 2) %>% group_by(bout_id) %>% summarise(dive_duration = n())
    my_tdr_clean <- left_join(my_tdr_clean, dive_dur_tibble)
    
    #Get DEE param
    deployment_duration_days <- as.numeric(difftime(max(my_tdr_clean$timestamp[which(my_tdr_clean$depth > 2)]), min(my_tdr_clean$timestamp[which(my_tdr_clean$depth > 2)]), units = "days"))
    phase_duration_hrs <- my_tdr_clean %>%
      group_by(type) %>%
      summarise(dur_tot_sec = n()) %>%
      mutate(dur_tot_hrs = dur_tot_sec/(60*60))
    dive_duration_hrs <- phase_duration_hrs$dur_tot_hrs[which(phase_duration_hrs$type == "dive")]
    deployment_filtered$daily_dive_duration_hrs[which(deployment_filtered$tag_id == my_tdr_clean$tag_id[1])] <- dive_duration_hrs/deployment_duration_days
    
    mvmt_summary <- my_tdr_clean %>%
      group_by(type) %>% 
      summarise(tot_vert_mvt = sum(abs(vert_speed), na.rm = T))
    vert_mvmt_sub_surface_m <- mvmt_summary$tot_vert_mvt[which(mvmt_summary$type == "sub-surface")]
    deployment_filtered$daily_vert_mvt_sub_surface_m [which(deployment_filtered$tag_id == my_tdr_clean$tag_id[1])] <- vert_mvmt_sub_surface_m/deployment_duration_days
    
    #Group dive data for RF
    #ade_track <- rbind(ade_track, my_tdr_clean) #%>% filter(depth >= 2))
    saveRDS(my_tdr_clean, paste0("data/clean_tdr/clean_", my_tdr_file))
  }
}

write_csv(deployment_filtered, "data/deployment_filtered_after_cleaning.csv")

#3 - Final check of cleaned data ######################################################################################################################################
list_tdr_files <- list.files("data/clean_tdr/", pattern = "*.rds")
deployment_table <- read_csv("data/deployment_filtered_after_cleaning.csv")

for (my_tdr_file in list_tdr_files[1:length(list_tdr_files)]) {
  tdr_data <- read_rds(paste0("data/clean_tdr/", my_tdr_file))
  
  #Auto timezone correction
  if(!is.na(deployment_table$date_start_logger[which(deployment_table$tag_id == tdr_data$tag_id[1])])){
    if(abs(
      as.numeric(
        round(
          difftime(dmy_hm(deployment_table$date_start_logger[which(deployment_table$tag_id == tdr_data$tag_id[1])]),tdr_data$timestamp[1]),0))) == 9){
      tdr_data$timestamp <- tdr_data$timestamp + hours(9) 
    }
    if(abs(
      as.numeric(
        round(
          difftime(dmy_hm(deployment_table$date_start_logger[which(deployment_table$tag_id == tdr_data$tag_id[1])]),tdr_data$timestamp[1]),0))) == 10){
      tdr_data$timestamp <- tdr_data$timestamp + hours(10) 
    }
  }
  
  if(!is.na(deployment_table$date_start_logger[which(deployment_table$tag_id == tdr_data$tag_id[1])]) &&
     abs(
       as.numeric(
         round(
           difftime(dmy_hm(deployment_table$date_start_logger[which(deployment_table$tag_id == tdr_data$tag_id[1])]),tdr_data$timestamp[1], units = "hours"),0))) < 1 &&
     deployment_table$n_chick_equip[which(deployment_table$tag_id == tdr_data$tag_id[1])] %in% c(1, 2) &&
     max(tdr_data$depth) > 50 &&
     !is.na(deployment_table$mass_start_kg[which(deployment_table$tag_id == tdr_data$tag_id[1])])
  ){
    print(paste("File", which(list_tdr_files == my_tdr_file), "out of", length(list_tdr_files)))
    write_csv(tdr_data, paste0("data/filtered_clean_tdr/", tdr_data$tag_id[1], ".csv"))
  }else{
    plot_dive <- ggplot(tdr_data, aes(y = -depth, x = timestamp)) +
      scattermore::geom_scattermore() +
      scale_x_datetime(date_breaks = "6 hours", date_labels = "%d/%m/%y %H:%M") +
      labs(title = tdr_data$tag_id[1])
    
    plot(plot_dive)
    
    print(paste("Visual check needed for file", which(list_tdr_files == my_tdr_file), "out of", length(list_tdr_files)))
    print(deployment_table[which(deployment_table$tag_id == tdr_data$tag_id[1]),
                           c("date_start_logger", "date_end_logger", "date_equip", "date_return", "mass_start_kg", "flipper_length_mm", "n_chick_equip")])
    print(paste("Logger starts at",tdr_data$timestamp[1]))
    print(paste("Trip duration is ", as.numeric(round(difftime(tdr_data$timestamp[nrow(tdr_data)],tdr_data$timestamp[1], units = "days"),2)), "days"))
    keep_trip <- readline(prompt = "Keep this foraging trip ? Check dive profile and deployment data ! (y/n/adjust) ")
    while(keep_trip != "y" && keep_trip != "n" && keep_trip != "adjust"){
      keep_trip <- readline(prompt = "Wrong input use only (y/n/adjust) ")
    }
    while(keep_trip == "adjust"){
      adjust_what <- readline(prompt = "Adjust what ? \n(1) Timezone \n(2) Start time \n(3) End time")
      if(adjust_what == 1){
        n_hours <- readline(prompt = "Number of hours to add to correct timestamp ? ")
        old_timestamp <- tdr_data$timestamp
        tdr_data$timestamp <- tdr_data$timestamp + hours(n_hours)
        print(paste("Logger starts at",tdr_data$timestamp[1]))
      }
      if(adjust_what == 2){
        
      }
      keep_trip <- readline(prompt = "Keep this foraging trip ? Check dive profile and deployment data ! (y/n/adjust) ")
    }
    if(keep_trip == "y"){
      write_csv(tdr_data, paste0("data/filtered_clean_tdr/", tdr_data$tag_id[1], ".csv"))
    }
  }
}

write_csv(deployment_table, "data/deployment_filtered_after_cleaning.csv")


#4 - Merge data, cut trip and get DEE param ######################################################################################################################################

list_tdr_files <- list.files("data/filtered_clean_tdr/", pattern = "*.csv")
deployment_table <- read_csv("data/deployment_filtered_after_cleaning.csv")
ade_track <- tibble()

for (my_tdr_file in list_tdr_files[1:length(list_tdr_files)]){
  print(paste("File", which(list_tdr_files == my_tdr_file), "out of", length(list_tdr_files)))
  tdr_data_uncut <- fread(paste0("data/filtered_clean_tdr/", my_tdr_file))
  
  #Cut from first dive (2m) to last one
  tdr_data_cut <- tdr_data_uncut[min(which(tdr_data_uncut$depth >= 2)):max(which(tdr_data_uncut$depth >= 2)),]
  
  cut_dive_plot <- ggplot(tdr_data_cut, aes(x = timestamp, y = -depth)) +
    scattermore::geom_scattermore() +
    labs(title = tdr_data_cut$tag_id[1])
  print(cut_dive_plot)
  
  #Get DEE param
  deployment_duration_days <- nrow(tdr_data_cut)/60/60/24
  phase_duration_hrs <- tdr_data_cut %>%
    group_by(type) %>%
    summarise(dur_tot_sec = n()) %>%
    mutate(dur_tot_hrs = dur_tot_sec/(60*60))
  dive_duration_hrs <- phase_duration_hrs$dur_tot_hrs[which(phase_duration_hrs$type == "dive")]
  deployment_table$daily_dive_duration_hrs[which(deployment_table$tag_id == tdr_data_cut$tag_id[1])] <- dive_duration_hrs/deployment_duration_days
  
  mvmt_summary <- tdr_data_cut %>%
    group_by(type) %>% 
    summarise(tot_vert_mvt = sum(abs(vert_speed), na.rm = T))
  vert_mvmt_sub_surface_m <- mvmt_summary$tot_vert_mvt[which(mvmt_summary$type == "sub-surface")]
  deployment_table$daily_vert_mvt_sub_surface_m [which(deployment_table$tag_id == tdr_data_cut$tag_id[1])] <- vert_mvmt_sub_surface_m/deployment_duration_days
  
  #Merge TDR data for RF
  ade_track <- rbind(ade_track, tdr_data_cut)
  
  #Save cut TDR
  write_csv(tdr_data_cut, paste0("data/clean_cut_filtered_tdr/", my_tdr_file))
}

ade_deploy <- deployment_table %>% filter(tag_id %in% tools::file_path_sans_ext(list_tdr_files))
write_csv(ade_deploy, "data/ade_deploy_1998-2024_clean.csv")

#Remove birds on eggs and the ones which underwent physio experiments
update_deployment <- read.csv("data/bird_info_w_id_filtered.csv")

update_deployment <- update_deployment %>%
  filter(n_chick_return %in% c("0","1","2", NA),
         n_chick_equip %in% c("0","1","2", NA))

ade_track <- ade_track %>%
  filter(tag_id %in% unique(update_deployment$tag_id))

write_rds(ade_track, "data/ade_tdr_1998-2024_clean.rds")

#Get DEE
#Check range
ggplot(ade_deploy, aes(y = daily_dive_duration_hrs, x = season)) +
  geom_boxplot() +
  geom_point()

ggplot(ade_deploy, aes(y = daily_vert_mvt_sub_surface_m, x = season)) +
  geom_boxplot() +
  geom_point() 

#One indiv has really high subsurface distance
ade_track %>%
  filter(tag_id == "ade_C48_AXY_87_CE_2019-20") %>% 
  ggplot(aes(x = timestamp, y = depth, col = type)) + 
  geom_point() +
  coord_cartesian(ylim = c(0,5))

dee_model <- readRDS("data/best_exp_model.rds")
ade_deploy <- ade_deploy %>% dplyr::rename(bm_init = mass_start_kg)
ade_deploy$dee <- predict(dee_model, ade_deploy)

write_csv(ade_deploy, "data/ade_deploy_1998-2024_clean_w_nrj.csv")

ggplot(ade_deploy, aes(y = dee, x = season)) +
  geom_boxplot() +
  geom_point() + 
  labs(title = "DEE accross 15 seasons - Adélie penguins")

#Get Foraging
ade_track <- read_rds("data/ade_tdr_1998-2024_clean.rds")

#4. Check variable range to validate the use of the RF
ade_track$season <- str_match(ade_track$tag_id, "_CE_\\s*(.*?)\\s*-")[,2]

#Roll SD vert accel
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = roll_sd_vert_accel)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$roll_sd_vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$roll_sd_vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Roll SD vertical acceleration")

#Roll mean vert speed
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = roll_mean_vert_speed)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$roll_mean_vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$roll_mean_vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Roll mean vertical speed")

  #Depth
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = depth)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$depth[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$depth[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Depth")

#Dive duration
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = dive_duration)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$dive_duration[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$dive_duration[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Dive duration")

#Vertical acceleration
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = vert_accel)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Vertical acceleration")

#Vertical speed
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = vert_speed)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Vertical speed")


#Some off values of vert speed and accel in 2001/2009. We filter and remove the tracks based on max speed of adélies (~5m/s)
#-   Discard T7 and T17 from 2001-02 because too many logger errors (same logger)
#-   Remove values depth change rate \> 5 m.s (max speed of adélie penguins)

#remove t17 and t7
ade_track <- ade_track %>%
  filter(tag_id != "ade_DDUt7_LLpdt_7C215_CE_2001-02" &
           tag_id != "ade_DDUt17_LLpdt_7C215_CE_2001-02")  

#Remove values > 5 m.s (max speed of adélie penguins) and > 10 m.s-2 for acceleration
off_speed <- which(abs(ade_track$vert_speed) > 5)
ade_track <- ade_track %>% filter(! tag_id %in% unique(ade_track$tag_id[off_speed]))

off_accel <- which(abs(ade_track$vert_accel) > 10)
if(length(off_accel) > 0){
  ade_track <- ade_track %>% filter(! tag_id %in% unique(ade_track$tag_id[off_accel]))
}

#Roll SD vert accel
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = roll_sd_vert_accel)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$roll_sd_vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$roll_sd_vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Roll SD vertical acceleration")

#Roll mean vert speed
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = roll_mean_vert_speed)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$roll_mean_vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$roll_mean_vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Roll mean vertical speed")

#Depth
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = depth)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$depth[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$depth[which(ade_track$season == "2018" & ade_track$type == "dive")]), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Depth")

#Dive duration
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = dive_duration)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$dive_duration[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$dive_duration[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Dive duration")

#Vertical acceleration
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = vert_accel)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$vert_accel[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Vertical acceleration")

#Vertical speed
ggplot(ade_track %>% filter(type == "dive"), aes(x = season, y = vert_speed)) +
  geom_scattermore() +
  geom_hline(yintercept = max(ade_track$vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  geom_hline(yintercept = min(ade_track$vert_speed[which(ade_track$season == "2018" & ade_track$type == "dive")], na.rm = T), col = "purple", linetype = "dashed") +
  labs(x = "Season", y = "Vertical speed")


rf_model <- readRDS("data/rf_best_model.rds")
for(my_tag_id in unique(ade_track$tag_id)){
  print(paste("File", which(unique(ade_track$tag_id) == my_tag_id), "out of", length(unique(ade_track$tag_id))))
  sub_ade_track <- ade_track %>% filter(tag_id == my_tag_id)
  
  my_pred <- predict(rf_model, sub_ade_track %>% drop_na(roll_mean_vert_accel) %>% filter(depth >= 2))
  sub_ade_track <- sub_ade_track %>% drop_na(roll_mean_vert_accel) %>% filter(depth >= 2) %>% mutate(state = my_pred$.pred_class)
  
  write_csv(sub_ade_track, paste0("data/tdr_after_rf/", my_tag_id, ".csv"))
}

#Get One big table of TDR, paste all tracks together
all_tdr_files <- list.files("data/tdr_after_rf", pattern = "*.csv")
ade_track <- tibble()
for(my_tdr_file in all_tdr_files){
  tdr_sub_track <- read.csv(paste0("data/tdr_after_rf/", my_tdr_file))
  ade_track <- rbind(ade_track, tdr_sub_track)
  print(paste("File", which(all_tdr_files == my_tdr_file), "out of", length(all_tdr_files)))
}
ade_track$timestamp <- ymd_hms(ade_track$timestamp)
write_rds(ade_track, "data/ade_tdr_1998-2024_clean_after_RF_2m.rds")
ade_rf <- read_rds("data/ade_tdr_1998-2024_clean_after_RF_2m.rds")

time_foraging_per_dive <- ade_rf %>% 
  filter(state == "Hunting") %>% 
  group_by(tag_id, bout_id, dive_duration) %>% 
  summarise(time_foraging_sec = n()) %>% 
  filter(time_foraging_sec >= 5) %>% 
  mutate(foraging_per_dive = time_foraging_sec/dive_duration) %>% 
  group_by(tag_id) %>% 
  summarise(mean_prop_foraging_per_dive = mean(foraging_per_dive),
            mean_time_foraging_per_dive = mean(time_foraging_sec))

max_depth_foraging_dive <- ade_rf %>% 
  filter(state == "Hunting") %>% 
  group_by(tag_id, bout_id, dive_duration) %>% 
  summarise(time_foraging_sec = n(),
            max_depth = max(depth)) %>% 
  filter(time_foraging_sec >= 5) %>% 
  group_by(tag_id) %>% 
  summarise(mean_max_depth_foraging_dive = mean(max_depth))

depth_foraging <- ade_rf %>% 
  filter(state == "Hunting") %>% 
  group_by(tag_id) %>% 
  summarise(mean_depth_foraging = mean(depth))

time_foraging <- ade_rf %>% 
  filter(state == "Hunting") %>% 
  group_by(tag_id) %>% 
  summarise(time_foraging_sec = n())

n_foraging_dive <- ade_rf %>% 
  filter(state == "Hunting") %>% 
  group_by(tag_id, bout_id) %>% 
  summarise(time_foraging_sec = n()) %>% 
  filter(time_foraging_sec >= 5) %>% 
  group_by(tag_id) %>% 
  summarise(n_foraging_dive_5s = n())

#Group by individual track and get summary of diving parameters
track_summary <- ade_rf %>%
  filter(type == "dive") %>% 
  group_by(tag_id) %>% 
  summarise(mean_depth = mean(depth),
            start_trip = min(timestamp),
            end_trip = max(timestamp)
  ) %>% 
  mutate(trip_duration_days = as.numeric(difftime(end_trip, start_trip, units = "days")),
         year = str_match(tag_id, "_CE_\\s*(.*?)\\s*-")[,2])

depth_summary <- ade_rf %>%
  group_by(tag_id, bout_id) %>%
  summarise(max_depth = max(depth),
            dive_duration = n()) %>%
  group_by(tag_id) %>%
  summarise(mean_max_depth = mean(max_depth),
            mean_dive_duration = mean(dive_duration),
            n_dive = n())

track_summary <- left_join(track_summary, depth_summary) %>% dplyr::select(-year)

ade_deploy <- ade_deploy %>% filter(tag_id %in% unique(ade_rf$tag_id))
ade_deploy <- left_join(ade_deploy, track_summary)
ade_deploy <- left_join(ade_deploy,
                        left_join(time_foraging,
                                  left_join(n_foraging_dive,
                                            left_join(max_depth_foraging_dive,
                                                      left_join(depth_foraging,time_foraging_per_dive)))))

ade_deploy <- ade_deploy %>%
  mutate(daily_time_foraging_sec = time_foraging_sec/trip_duration_days,
         daily_n_foraging_dive_5s = n_foraging_dive_5s/trip_duration_days)

write_csv(ade_deploy, "data/ade_deploy_1998-2024_final_wo_env.csv")
