# loading packages
library(tidyverse)
library(readxl)

# seed for reproducibility
set.seed(493)



### Preparing qualtrics dataset ### --------------------------------------------

# preparing the questions that I want to select
vector <- c("Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", 
            "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")

# reading and wrangling data
qualtrics <- read_xlsx("qualtrics.xlsx") %>% 
    #clean_names() %>% 
    slice(-1, -3) %>% 
    select(ResponseId, Q4:Q12, all_of(vector), ends_with(c("Submit", "start", "end")))

# dealing with character columns
date_cols <- colnames(select(qualtrics, ends_with(c("end", "start")))) 

qualtrics <- qualtrics %>%
    mutate_at(all_of(date_cols), funs(gsub("T", "", as.character(.)))) %>% 
    mutate_at(all_of(date_cols), funs(gsub("Z", "", as.character(.)))) %>% 
    mutate_at(all_of(date_cols), funs(substring(., 1, nchar(.)-4))) #%>% 

# converting data into long format
dat_long <- gather(qualtrics, key = "condition", value = "timestamp", c(ends_with(c("end", "start")))) %>% 
    #separate(key, into = c("ts_type","qid"), sep = "_") %>% 
    #spread(ts_type,value) %>% 
    arrange(ResponseId,timestamp)

# converting columns from character to timestamp
dat_long$timestamp2 <- strptime(dat_long$timestamp, tz = "Europe/Paris", format = "%Y-%m-%d%H:%M:%S")

getOption("scipen")
options(scipen=999)

# converting timestamp to numeric
dat_long$timestamp_num <- as.numeric(dat_long$timestamp2)

# filling timestamp data
dat_long_full <- dat_long %>% group_by(ResponseId) %>% 
    complete(timestamp_num = full_seq(timestamp_num, period = 1, tol=1))

# converting numeric to timestamp
dat_long_full$timestamp2 <- dat_long_full$timestamp_num %>% 
    as.POSIXct(origin = "1970-01-01", tz = "Europe/Paris")



### Simulating temp data ### --------------------------------------------------

# data simulation
temp_data_pp1 <- data.frame(temp = rnorm(n = 95, mean = 30, sd = 4),
                        data_point = seq_len(95),
                        timestamp = as.POSIXct("2023-01-03 10:09:15", tz = "Europe/Paris", format = "%Y-%m-%d %H:%M:%S"),
                        pp = 1)

temp_data_pp2 <- data.frame(temp = rnorm(n = 95, mean = 30, sd = 4),
                            data_point = seq_len(95),
                            timestamp = as.POSIXct("2023-01-04 09:01:53", tz = "Europe/Paris", format = "%Y-%m-%d %H:%M:%S"),
                            pp = 2)

temp_data_pp1$timestamp <- temp_data_pp1$timestamp + temp_data_pp1$data_point
temp_data_pp2$timestamp <- temp_data_pp2$timestamp + temp_data_pp2$data_point

temp_data <- rbind(temp_data_pp1, temp_data_pp2)
remove(temp_data_pp1, temp_data_pp2, dat_long)

temp_data <- arrange(temp_data, timestamp)



### Reading temp data ### -----------------------------------------------------
# Here instead of simulating data, we'll load temp data for each pp
#setwd("/Users/belleminronan/Documents/TER/TER_R/data_1 copie")

# import data
#purplexus_files_list <- lapply(Sys.glob("pp*_temperatures.csv"), read.csv)

# bind all dataframes of the list
#purplexus <- as.data.frame(do.call("rbind",purplexus_files_list))

# append all labels with purplexus_ to avoid confusion later
#names(purplexus) <- c("purplexus_timestamp", "purplexus_temp")

# set same time zone
#purplexus$purplexus_timestamp <- as.POSIXct(purplexus$purplexus_timestamp, 
#                                           tz = "Europe/Paris", format = "%Y-%m-%d %H:%M:%S")

# Check for NA temperature readings or times
#any(is.na(purplexus$purplexus_timestamp))
#any(is.na(purplexus$purplexus_temp))

# Remove lists
#rm(purplexus_files_list, isp_files_list)



### Matching by timestamp temp data & qualtrics data ### -----------------------

library("MALDIquant") # only for the match.closest function
dat_long_full2 <- dat_long_full
dat_long_full2$timestamp_temp_data <-temp_data$timestamp[match.closest(dat_long_full2$timestamp2, temp_data$timestamp, tolerance = .1)]

dat_long_full2$temp <- temp_data$temp[match.closest(dat_long_full2$timestamp2, temp_data$timestamp, tolerance = .05)]

dat_long_full2 <- fill(dat_long_full2, condition,.direction="down")

dat_long_full2$condition <-
    case_when(dat_long_full2$condition == "happinessARs_start" ~ "happinessARs",
              dat_long_full2$condition =="happinessARd_start" ~ "happinessARd",
              dat_long_full2$condition == "happinessCS_start" ~ "happinessCS",
              dat_long_full2$condition == "happinessEM_start" ~ "happinessEM",
              dat_long_full2$condition =="happinessMP_start" ~ "happinessMP",
              dat_long_full2$condition == "angerARs_start" ~ "angerARs",
              dat_long_full2$condition =="angerARd_start" ~ "angerARd",
              dat_long_full2$condition =="angerCS_start" ~ "angerCS",
              dat_long_full2$condition =="angerEM_start" ~ "angerEM",
              dat_long_full2$condition =="angerMP_start" ~ "angerMP",
              dat_long_full2$condition == "sadnessARs_start" ~ "sadnessARs",
              dat_long_full2$condition =="sadnessARd_start" ~ "sadnessARd",
              dat_long_full2$condition == "sadnessCS_start" ~ "sadnessCS",
              dat_long_full2$condition =="sadnessEM_start" ~ "sadnessEM",
              dat_long_full2$condition == "sadnessMP_start" ~ "sadnessMP",
              dat_long_full2$condition == "happinessARs_end" ~ "happinessARs",
              dat_long_full2$condition =="happinessARd_end" ~ "happinessARd",
              dat_long_full2$condition == "happinessCS_end" ~ "happinessCS",
              dat_long_full2$condition == "happinessEM_end" ~ "happinessEM",
              dat_long_full2$condition =="happinessMP_end" ~ "happinessMP",
              dat_long_full2$condition == "angerARs_end" ~ "angerARs",
              dat_long_full2$condition =="angerARd_end" ~ "angerARd",
              dat_long_full2$condition =="angerCS_end" ~ "angerCS",
              dat_long_full2$condition =="angerEM_end" ~ "angerEM",
              dat_long_full2$condition =="angerMP_end" ~ "angerMP",
              dat_long_full2$condition == "sadnessARs_end" ~ "sadnessARs",
              dat_long_full2$condition =="sadnessARd_end" ~ "sadnessARd",
              dat_long_full2$condition == "sadnessCS_end" ~ "sadnessCS",
              dat_long_full2$condition =="sadnessEM_end" ~ "sadnessEM",
              dat_long_full2$condition == "sadnessMP_end" ~ "sadnessMP")
