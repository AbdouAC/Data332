library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

rm(list = ls())
setwd("C:/Users/husky/OneDrive/Desktop/System Analysis and design/Data332/cabbageButterflyrcodes")

### read files and clean data 
df_pierisData <- read_excel("CompletePierisData_2022-03-09.xlsx", sheet = 1) %>%
  dplyr::rename("country"="dwc:country") %>% 
  dplyr::rename("year"="dwc:year") %>% 
  dplyr::rename("continent"="dwc:continent") %>% 
  dplyr::rename("decimallatitude"="dwc:decimalLatitude") %>% 
  dplyr::rename("decimallongitude"="dwc:decimalLongitude")

df_fixed <- df_pierisData %>% 
  dplyr::mutate(country = ifelse(country=="U.S.A.","USA",country)) %>% 
  dplyr::mutate(country = ifelse(country=="United States","USA",country))  

df_CleanedData <- read_excel("Cleaned Data LWA .xlsx", sheet = 1) %>% 
  dplyr::rename("coreid"="core ID") %>% 
  left_join(df_fixed, by = c("coreid")) %>% 
  dplyr::rename("LWlength"="LW length") %>% 
  dplyr::select("coreid","sex", "LWlength", "LW width" , "RW length", "RW width"
                ,"LBlackPatchApex", "continent", "country", "year")
df_CleanedData$LBlackPatchApex <- as.numeric(df_CleanedData$LBlackPatchApex)
df_CleanedData$year <- as.numeric(df_CleanedData$year )

### find min, max, and average for left wing of males

malebutterfly_sex <- df_CleanedData %>% 
  dplyr::filter(sex == "male")

femalebutterfly_sex <- df_CleanedData %>%
  dplyr::filter(sex == "female") 


wing_min <- min(malebutterfly_sex$LWlength)
wing_max <- max(malebutterfly_sex$LWlength)
wing_avg <- mean(malebutterfly_sex$LWlength)
butterfly_sex <- paste("Male")
df_male <- data.frame(butterfly_sex,wing_avg, wing_max, wing_min)

wing_min <- min(femalebutterfly_sex$LWlength)
wing_max <- max(femalebutterfly_sex$LWlength)
wing_avg <- mean(femalebutterfly_sex$LWlength)
butterfly_sex <- paste("Female")
df_female <- data.frame(butterfly_sex,wing_avg, wing_max, wing_min)

### merge data into one data frame

total <- rbind(df_male, df_female)

t.test(total$wing_avg,mu = 30)
