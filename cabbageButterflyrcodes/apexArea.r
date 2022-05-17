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


malebutterfly_sex <- df_CleanedData %>% 
  dplyr::filter(sex == "male")


femalebutterfly_sex <- df_CleanedData %>%
  dplyr::filter(sex == "female") 

apex_min <- min(malebutterfly_sex$LBlackPatchApex)
apex_max <- max(malebutterfly_sex$LBlackPatchApex)
apex_avg <- mean(malebutterfly_sex$LBlackPatchApex)
butterfly_sex <- paste("Male")
df_male2 <- data.frame(butterfly_sex,apex_avg, apex_max, apex_min)

apex_min <- min(femalebutterfly_sex$LBlackPatchApex)
apex_max <- max(femalebutterfly_sex$LBlackPatchApex)
apex_avg <- mean(femalebutterfly_sex$LBlackPatchApex)
butterfly_sex <- paste("Female")
df_female2 <- data.frame(butterfly_sex,apex_avg,apex_max, apex_min)

total2 <- rbind(df_male2, df_female2)

butter_chart2 <- ggplot(total2, aes(y = apex_avg, x = butterfly_sex, fill = butterfly_sex)) +
  geom_bar(stat = "identity")+
  xlab("sex")+
  ylab("Average Apex Area")+
  ggtitle("Average Apex Area Per Sex")+
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(butter_chart2)

