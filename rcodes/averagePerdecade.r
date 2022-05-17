library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)


rm(list = ls())
setwd("C:/Users/brantschulze19/Documents/GitHub/Data332/rcodes")


df_pierisData <- read_excel("CompletePierisData_2022-03-09.xlsx", sheet = 1) %>%
  dplyr::rename("country"="dwc:country") %>% 
  dplyr::rename("year"="dwc:year") %>% 
  dplyr::rename("continent"="dwc:continent") %>% 
  dplyr::rename("decimallatitude"="dwc:decimalLatitude") %>% 
  dplyr::rename("decimallongitude"="dwc:decimalLongitude")



df_pierisData$year <- substr(df_pierisData$year,1,3)
df_pierisData$year <- paste0(df_pierisData$year,"0")
df_pierisData$LBlackPatchApex <- as.numeric(df_pierisData$LBlackPatchApex)

df <- df_pierisData %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(number_please = mean(LBlackPatchApex)) 


butter_chart3 <- ggplot(df, aes(y = number_please, x = year, fill = year)) +
  geom_bar(stat = "identity")+
  xlab("Year")+
  ylab("Average Apex Area")+
  ggtitle("Average Apex Area Per Decade")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(butter_chart3)

