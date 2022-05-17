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
#### Visualization

butterfly_country <- df_CleanedData %>%
  dplyr::group_by(country) %>%
  filter(!is.na(country)) %>%
  dplyr::summarise(number = n())

### hist(butterfly_country$number)

butterfly_chart <- ggplot(butterfly_country, aes(y = number, x = country, fill = country)) +
  geom_bar(stat = "identity")+
  xlab("country")+
  ylab("Number of butterfly Per Country")+
  ggtitle("Number of Butterfly Per Country")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(butterfly_chart)

