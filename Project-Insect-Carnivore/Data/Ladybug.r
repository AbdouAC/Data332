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
  dplyr::mutate(Species = ifelse(Species =="Harminia axyridis","Harmonia axyridis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species =="Propylea quatuordecimpuncata","Propylea quatuordecimpunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coccinella septempunctata","Coccinella septempunctata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coleomegilla maculata","Coleomegilla maculata",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="Hippodamia covergence","Hippodamia convergens",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="hippodamia parenthesis","Hippodamia parenthesis",Species)) %>%
  dplyr::mutate(Species = ifelse(Species=="coccinella septempunctata","Coccinella septempunctata",Species)) %>%
  dplyr::select("Species", "date", "basisOfRecord", "year","country","stateProvince",
                "recordedBy")

### formatting the date in to year, month, day then selecting only the year

df_ladybugdata$date <- ymd(df_ladybugdata$date)
df_ladybugdata$date <- format(df_ladybugdata$date, format = '%Y')

### group data by state, species and who recorded the data 
### filter out the null values 

ladybug_group <- df_ladybugdata %>%
  dplyr::group_by(stateProvince, Species, recordedBy) %>%
  filter(!is.na(stateProvince)) %>% 
  filter(!is.na(recordedBy)) %>% 
  dplyr::summarise(number = n())

### Number of Ladybug Per species

species_chart <- ggplot(ladybug_group, aes(y = number, x = Species, fill = Species)) +
  geom_bar(stat = "identity")+
  xlab("species")+
  ylab("Number of ladybug")+
  ggtitle("Number of Ladybug Per species")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(species_chart)

### Number of Ladybug Per Sate

state_chart <- ggplot(ladybug_group, aes(y = number, x = stateProvince, fill = stateProvince)) +
  geom_bar(stat = "identity")+
  xlab("state")+
  ylab("Number of ladybug")+
  ggtitle("Number of Ladybug Per State")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(state_chart)

### Number of Ladybug Recorded Per Person

record_chart <- ggplot(ladybug_group, aes(y = number, x = recordedBy, fill = recordedBy)) +
  geom_bar(stat = "identity")+
  xlab("Recorded by")+
  ylab("Number of ladybug")+
  ggtitle("Number of Ladybug Recorded Per Person")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(record_chart)


ladybug_species <- df_ladybugdata %>%
  dplyr::group_by(Species, recordedBy) %>%
  filter(!is.na(stateProvince)) %>% 
  filter(!is.na(recordedBy)) %>% 
  dplyr::summarise(number = n())

Jack <- ladybug_species %>% 
  dplyr::filter(recordedBy == "Jack Hughes")


#number = count(df_ladybugdata, vars = df_ladybugdata$recordedBy)

