library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

rm(list = ls())
setwd("C:/Users/husky/OneDrive/Desktop/System Analysis and design/Data332/Project-Insect-Carnivore/Data")


### read the excel and csv file 

df_scandata <- read_csv("Scan Ladybug Data.csv")

### dplyr mutate is used to make sure naming is consistent throughout the data

df_ladybugdata <- read_excel("Ladybug Data.xlsx", sheet = 1) %>% 
  dplyr::rename("catalogNumber" = "SCAN CODE") %>% 
  left_join(df_scandata, by = c("catalogNumber")) %>% 
  dplyr::mutate(stateProvince = ifelse(stateProvince =="Ia","Iowa",stateProvince)) %>% 
  dplyr::mutate(stateProvince = ifelse(stateProvince =="IA","Iowa",stateProvince)) %>% 
  dplyr::mutate(stateProvince = ifelse(stateProvince =="IL","Iowa",stateProvince)) %>%
  dplyr::mutate(country = ifelse(country =="United Arab Emirates","United States",country)) %>% 
  dplyr::mutate(recordedBy = ifelse(recordedBy =="o. ruffatto","Olivia Ruffatto",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="O. Ruffatto","Olivia Ruffatto",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="Ruffatto O.","Olivia Ruffatto",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="Cervantes V.","Veronica Cervantes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="V. Cervantes","Veronica Cervantes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="v. cervantes","Veronica Cervantes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="V. cervantes","Veronica Cervantes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="Gorsegner M.","Marissa Gorsegner",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="M.","Marissa Gorsegner",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="M. Gorsegner","Marissa Gorsegner",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="m. gorsegner","Marissa Gorsegner",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="Hughes J.","Jack Hughes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="j. hughes","Jack Hughes",recordedBy)) %>%
  dplyr::mutate(recordedBy = ifelse(recordedBy =="J. Hughes","Jack Hughes",recordedBy)) %>%
  dplyr::mutate(Species = ifelse(Species =="cycloneda munda","Cycloneda munda",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Cycloneda munda","Cycloneda munda",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Cycloneda Munda","Cycloneda munda",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="harmonia axyridis","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Harmonia axyridis","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Harminia axyridis","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Harmonia axyrids","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="harmonia axyrids","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Harmonia axyrisis","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Propylea quatuordecimpuncata","Propylea quatuordecimpunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coccinella septempunctata","Coccinella septempunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coleomegilla maculata","Coleomegilla maculata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="Colemegilla maculata","Coleomegilla maculata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="Hippodamia covergence","Hippodamia convergens",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="hippodamia convergens","Hippodamia convergens",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="hippodamia parenthesis","Hippodamia parenthesis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coccinella septempunctata","Coccinella septempunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="Coccinella Septempunctata","Coccinella septempunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="Coccinella semtempuncata","Coccinella septempunctata",Species)) %>%
  dplyr::select("Species", "date", "basisOfRecord", "year","country","stateProvince",
                "recordedBy")

### formatting the date in to year, month, day then selecting only the year

df_ladybugdata$date <- ymd(df_ladybugdata$date)
df_ladybugdata$date <- format(df_ladybugdata$date, format = '%Y')


ladybug_species <- df_ladybugdata %>%
  dplyr::group_by(Species, recordedBy) %>%
  filter(!is.na(stateProvince)) %>% 
  filter(!is.na(recordedBy)) %>% 
  dplyr::summarise(number = n())

Jack <- ladybug_species %>% 
  dplyr::filter(recordedBy == "Jack Hughes")

recordby <- paste("Jack Hughes")
average = sum(Jack$number)/11
df_jack <- data.frame(recordby,average)

olivia <- ladybug_species %>% 
  dplyr::filter(recordedBy == "Olivia Ruffatto")

recordby <- paste("Olivia Ruffatto")
average = sum(olivia$number)/13
df_olivia <- data.frame(recordby,average)

### merge data into one data frame

total <- rbind(df_jack, df_olivia)

t.test(total$average,mu = 15)

### After doing the t-test, we notice that the p-value < 0.05 so we can reject 
### the null hypothesis and there is a 95% chance that the true mean is between'
### -4.06 and 26.85