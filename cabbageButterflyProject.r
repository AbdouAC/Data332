library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

rm(list = ls())
setwd("C:/Users/husky/OneDrive/Desktop/System Analysis and design")


df_PierisData <- read_excel("CompletePierisData_2022-03-09.xlsx", sheet = 1) %>%
  dplyr::rename("country"="dwc:country") %>% 
  dplyr::rename("year"="dwc:year") %>% 
  dplyr::rename("continent"="dwc:continent") %>% 
  dplyr::rename("decimallatitude"="dwc:decimalLatitude") %>% 
  dplyr::rename("decimallongitude"="dwc:decimalLongitude")

df_CleanedData <- read_excel("Cleaned Data LWA .xlsx", sheet = 1)

df <- df_PierisData %>% 
  dplyr::mutate(country = ifelse(country=="U.S.A.","USA",country)) %>% 
  dplyr::mutate(country = ifelse(country=="United States","USA",country))
  
