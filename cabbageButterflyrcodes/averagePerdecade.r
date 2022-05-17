library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)


rm(list = ls())
setwd("C:/Users/husky/OneDrive/Desktop/System Analysis and design/Data332/cabbageButterflyrcodes")


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

df_CleanedData$year <- substr(df_CleanedData$year,1,3)
df_CleanedData$year <- paste0(df_CleanedData$year,"0")
df_CleanedData$LBlackPatchApex <- as.numeric(df_CleanedData$LBlackPatchApex)

df <- df_CleanedData %>%
  dplyr::group_by(year) %>%
  filter(!is.na(year)) %>%
  dplyr::summarise(number_please = mean(LBlackPatchApex)) 


butter_chart3 <- ggplot(df, aes(y = number_please, x = year, fill = year)) +
  geom_bar(stat = "identity")+
  xlab("Year")+
  ylab("Average Apex Area")+
  ggtitle("Average Apex Area Per Decade")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(butter_chart3)

