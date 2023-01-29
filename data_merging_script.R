# loading packages
library(tidyverse)
library(readxl)

# seed for reproducibility
set.seed(493)



### Preparing qualtrics dataset ### --------------------------------------------

# preparing the questions that I want to select
vector <- c("Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", 
            "Q23", "Q24", "Q25", "Q26", "Q27", "Q28")

# the following Qualtrics dataset is just a test to see if the script is working
# you can find it on the GitHub repo of this project: https://github.com/ronanbellemin/bellemin-ijzerman_emotion-relational-models
# reading and wrangling data
qualtrics <- read_xlsx("qualtrics_test.xlsx") %>% 
    #clean_names() %>% 
    slice(-1, -3) %>%
    select(ResponseId, Q4:Q12, all_of(vector), ends_with(c("Submit", "start", "end"))) %>% 
    mutate(participant.id = seq_len(nrow(.)))

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



### Simulating temp data mathcing with qualtrics data ### ----------------------

# data simulation
temp_data_pp1 <- data.frame(temp = rnorm(n = 95, mean = 30, sd = 4),
                        data_point = seq_len(95),
                        timestamp = as.POSIXct("2023-01-03 10:09:15", tz = "Europe/Paris", format = "%Y-%m-%d %H:%M:%S"))

temp_data_pp2 <- data.frame(temp = rnorm(n = 95, mean = 30, sd = 4),
                            data_point = seq_len(95),
                            timestamp = as.POSIXct("2023-01-04 09:01:53", tz = "Europe/Paris", format = "%Y-%m-%d %H:%M:%S"))

# adding up timestamp and datapoints 
temp_data_pp1$timestamp <- temp_data_pp1$timestamp + temp_data_pp1$data_point
temp_data_pp2$timestamp <- temp_data_pp2$timestamp + temp_data_pp2$data_point

# binding first participant's temperature data with second participant's temperature data
temp_data <- rbind(temp_data_pp1, temp_data_pp2)
remove(temp_data_pp1, temp_data_pp2, dat_long)

temp_data <- arrange(temp_data, timestamp)



### Reading temp data ### -----------------------------------------------------
# Here instead of the simulated data above, we'll load temp data for each pp

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

# matching temperature data with timestamp from qualtrics
dat_long_full2$temp <- temp_data$temp[match.closest(dat_long_full2$timestamp2, temp_data$timestamp, tolerance = .05)]

# filling in the condition column
dat_long_full2 <- fill(dat_long_full2, condition,.direction="down")

# filling in the participant column
dat_long_full2 <- fill(dat_long_full2, participant.id,.direction="down")

# renaming conditions as the same for "_start" and "_end"
# e.g., happinessARs_start & hapiness_ARs_end become: "happiness_ARs"
dat_long_full2$condition <- 
    case_when(grepl("happinessARs", dat_long_full2$condition) ~ "happinessARs",
              grepl("happinessARd", dat_long_full2$condition) ~ "happinessARd",
              grepl("happinessCS", dat_long_full2$condition) ~ "happinessCS",
              grepl("happinessEM", dat_long_full2$condition) ~ "happinessEM",
              grepl("happinessMP", dat_long_full2$condition) ~ "happinessMP",
              grepl("angerARs", dat_long_full2$condition) ~ "angerARs",
              grepl("angerARd", dat_long_full2$condition) ~ "angerARd",
              grepl("angerCS", dat_long_full2$condition) ~ "angerCS",
              grepl("angerEM", dat_long_full2$condition) ~ "angerEM",
              grepl("angerMP", dat_long_full2$condition) ~ "angerMP",
              grepl("sadnessARs", dat_long_full2$condition) ~ "sadnessARs",
              grepl("sadnessARd", dat_long_full2$condition) ~ "sadnessARd",
              grepl("sadnessCS", dat_long_full2$condition) ~ "sadnessCS",
              grepl("sadnessEM", dat_long_full2$condition) ~ "sadnessEM",
              grepl("sadnessMP", dat_long_full2$condition) ~ "sadnessMP")

# saving the full dataset
#writexl::write_xlsx(dat_long_full2, "final_full_data.xlsx")



### Splitting participants between exploratory or confirmatory dataset ### -----

# making this sampling backward-compatible
RNGkind(sample.kind = "Rounding")
set.seed(38)

# randomly selecting which to put in exploratory set
expl_pp <- sample(unique(dat_long_full2$participant.id), 1) # here we'll adapt according to N participants/2
print(expl_pp)

# participants x, x, x, x, x, ... to exploratory set, rest to confirmatory set
exploratory_dataset <- filter(dat_long_full2, participant.id %in% expl_pp)
confirmatory_dataset <- filter(dat_long_full2, !(participant.id %in% expl_pp))

# saving exploratory and confirmatory datasets
#writexl::write_xlsx(exploratory_dataset, "exploratory_dataset.xlsx")
#writexl::write_xlsx(confirmatory_dataset, "confirmatory_dataset.xlsx")
