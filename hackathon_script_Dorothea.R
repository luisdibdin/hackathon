## install packages ############################################################

install.packages("ggplot2", dependencies = TRUE, type = "win.binary")
library(ggplot2)

install.packages("tidyverse", dependencies = TRUE, type = "win.binary")

install.packages("devtools", dependencies = TRUE, type = "win.binary")

library(lubridate)

################################################################################


## load data and save needed ###################################################

setwd("~/UN-Hackathon-2022")

#EU <- read.csv("2019-10-29-2022-11-07-Europe.csv")
ACLED_UK <- read.csv("2019-10-29-2022-11-07-Europe-United_Kingdom.csv")


lloyds <- read.table(
  "UKDA-8739-tab/tab/lrf_public_file_review.tab",
  sep="\t", header=TRUE)

lloyds_UK <- lloyds[lloyds$Country == "United Kingdom",]

UK_data_uncleaned <- list(ACLED_UK, lloyds_UK)

save(UK_data_uncleaned, file = "UK_data_uncleaned.Rdata")

library(purrr)

unique_groups <- purrr::map(.x = UK$assoc_actor_1, 
                            .f = ~ stringr::str_split(.x, pattern = ";")) %>%
  unlist() %>%
  unique()

save(unique_groups, file = "hackathon/unique_groups.Rdata")

write.csv(unique_groups, file = "hackathon/unique_groups.csv")

################################################################################


## clean ACLED data ############################################################


ACLED_UK_clean <- ACLED_UK 


ACLED_UK_clean <- tidyr::separate(data = ACLED_UK_clean, 
                into = as.character(1:17),
                col = assoc_actor_1,
                sep = "; ")

ACLED_UK_clean <- tidyr::gather(data = ACLED_UK_clean, 
                                key = "event_code_per_group",
                                value = "action_group",
                                as.character(1:17))
# remove those with NA in action_group
ACLED_UK_clean <- ACLED_UK_clean[!is.na(ACLED_UK_clean$action_group),]

## add times
# convert to be made to date
ACLED_UK_clean <- tidyr::separate(data = ACLED_UK_clean,
                into = c("day", "month", NA),
                col = event_date,
                sep = " ")

# make months 1:12
ACLED_UK_clean$month <- ifelse(ACLED_UK_clean$month == "January", "01",
                               ifelse(ACLED_UK_clean$month == "February", "02",
                                      ifelse(ACLED_UK_clean$month == "March", "03",
                                             ifelse(ACLED_UK_clean$month == "April", "04",
                                                    ifelse(ACLED_UK_clean$month == "May", "05",
                                                           ifelse(ACLED_UK_clean$month == "June", "06",
                                                                  ifelse(ACLED_UK_clean$month == "July", "07",
                                                                         ifelse(ACLED_UK_clean$month == "August", "08",
                                                                                ifelse(ACLED_UK_clean$month == "September", "09",
                                                                                       ifelse(ACLED_UK_clean$month == "October", "10",
                                                                                              ifelse(ACLED_UK_clean$month == "November", "11", "12")))))))))))


ACLED_UK_clean$date <- paste(ACLED_UK_clean$year, ACLED_UK_clean$month, ACLED_UK_clean$day, sep = "-")

ACLED_UK_clean$date <- lubridate::as_date(ACLED_UK_clean$date)

write.csv(ACLED_UK_clean, file = "hackathon/ACLED_UK_clean.csv")
  
################################################################################


## add action group categorisations column #####################################

ACLED_UK_clean <- read.csv("hackathon/ACLED_UK_clean.csv", row.names = 1)

group_cat <- read.csv("hackathon/unique_groups_categorised.csv", row.names = 1, skip = 1)

group_cat <- group_cat[, 1:2]
colnames(group_cat)[2] <- "climate" 

ACLED_UK_clean <- dplyr::left_join(ACLED_UK_clean, group_cat, by = c("action_group" = "Group"))

write.csv(ACLED_UK_clean, file = "hackathon/ACLED_UK_clean.csv")

################################################################################