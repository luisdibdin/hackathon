
### Cleaning LLoyds risk poll data

# Then will feed into a regression to see what characteristics relate to concern on climate change

## Found data dictionary here (https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8739#!/documentation)

library(tidyverse)

lloyds <- read.csv("D:/Joel/Docs/Hackathon/LRF_World-Risk-Poll-2021_resilience-data (1).csv")


colnames(lloyds)



test <- lloyds[, 1:21]

head(lloyds)

## we have region data too!!
lloyds %>% count(REGION_GBR)

lloyds %>% count(Country)


### Get columns and UK only data
to_keep <- c(colnames(lloyds[, 3:21]), "REGION_GBR", "Q3")

lloyds_uk <- lloyds %>% 
  select(to_keep) %>% 
  filter(Country == "United Kingdom")

lloyds_uk <- lloyds_uk %>% 
  select(!c("WGT", "GlobalRegion", "country.in.both.waves"))

colnames(lloyds_uk)


## Check Q3 (outcome)

lloyds_uk %>% count(Q3) %>% mutate(prop = n/sum(n))
lloyds_uk %>% group_by(Year) %>% count(Q3) %>% mutate(prop = n/sum(n)) ## This macthes last years published estimates so all good
lloyds %>% group_by(Year) %>% count(Q3) %>% mutate(prop = n/sum(n))

### Tidy/group variables

lloyds_uk %>% count(AgeGroups4)

## Get each age group
lloyds_uk %>% 
  group_by(AgeGroups4) %>% 
  summarise(min_age = min(Age),
            max_age = max(Age))


lloyds_uk %>% count(Education)
lloyds_uk %>% count(Gender)
lloyds_uk %>% count(INCOME_5)
lloyds_uk %>% count(HouseholdSize)
lloyds_uk %>% count(ChildrenInHousehold)
lloyds_uk %>% count(Year)
lloyds_uk %>% count(EMP_2010) ## <- leave this, not clear what it is
lloyds_uk %>% count(REGION_GBR) ## <- NOT SURE ON THE either, try and work out later if there is time

lloyds_uk_clean<- lloyds_uk %>% 
  mutate(
    Age = as.numeric(Age),
    AgeGroup = case_when(
      AgeGroups4 == 1 ~ "15-29",
      AgeGroups4 == 2 ~ "30-49",
      AgeGroups4 == 3 ~ "50-64",
      AgeGroups4 == 4 ~ "65-94"
    ),
    education_level = case_when(Education == 1 ~ "Low", #"8 years or below",
                          Education == 2 ~ "Medium", #"9-15 years",
                          Education == 3 ~ "High"), #"4 years beyond high school"),
  
    Gender = case_when(Gender == 1 ~ "Male",
                       Gender == 2 ~ "Female"),
    
    INCOME_5_categories = case_when(INCOME_5 == 1 ~ "Poorest 20%",
                                    INCOME_5 == 2 ~ "Second 20%",
                                    INCOME_5 == 3 ~ "Third 20%",
                                    INCOME_5 == 4 ~ "Fourth 20%",
                                    INCOME_5 == 5 ~ "Richest 20%"),
    
    Urbanicity = case_when(Urbanicity == 1 ~ "Rural",
                           Urbanicity == 2 ~ "Urban"),
    
    HouseholdSize = case_when(HouseholdSize == 1 ~ "1-2",
                              HouseholdSize == 2 ~ "3-4",
                              HouseholdSize %in% c(3,4) ~ "5+"),
    
    ChildrenInHousehold = case_when(ChildrenInHousehold == 0 ~ "No",
                                    ChildrenInHousehold > 0 ~ "Yes"),
    
    Year = as_factor(Year),
    
    climate_threat = case_when(Q3 %in% c(1, 2) ~ 1,
                               Q3 %in% c(3, 98) ~ 0)
  )


lloyds_uk_clean %>% count(climate_threat)

colnames(lloyds_uk_clean)

lloyds_uk_clean <- lloyds_uk_clean %>% 
  select(Year, Age, AgeGroup, Gender, education_level, Income = INCOME_5, Income_categories = INCOME_5_categories,
         Urbanicity, HouseholdSize, ChildrenInHousehold, climate_threat, Q3)



