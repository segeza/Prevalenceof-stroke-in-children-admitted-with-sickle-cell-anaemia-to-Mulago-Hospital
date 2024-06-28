library(tidyverse)
library(reshape2)

#Importing Data and renaming columns for easy work.

library(readxl)
hospitalized_scd <- read_excel("~/Downloads/12883_2016_704_MOESM1_ESM.xls")%>% 
  rename("primary_diagnosis"="Primary Diagnosis",
         "secondary_diagnosis"="Secondary Diagnosis")
View(hospitalized_scd)

#calculating frequency table for primary diagnosis(stroke prevalence)
hospitalized_scd %>% 
  group_by(primary_diagnosis) %>% 
  summarise(freq=n()) %>% 
  arrange(desc(freq))

#prevalence of srtoke
180/2869*100
#=6.27%

#data Cleaning
#selecting hospitalized cases with a diagnosis of stroke

stroke_scd <- hospitalized_scd %>% 
  filter(primary_diagnosis == "stroke")
view(stroke_scd)

ggplot(data=stroke_scd)+
  geom_bar(mapping=aes(x=Sex))

#age of children with scd with stroke 
ggplot(data=stroke_scd)+
  geom_freqpoly(mapping = aes(x=Age, color=Sex), binwidth=0.9)+
  labs(title = paste("Ages of Children admitted with Sickle Cell at anemia at Mulago Hospital"))

#Filter all stroke diagnoses without a secondary diagnois
secondary_dx <- stroke_scd %>% 
  filter(secondary_diagnosis!="NA")

secondary_dx_only <- secondary_dx %>% 
  select(secondary_diagnosis)

view(secondary_dx_only)
table(secondary_dx_only)/length(secondary_dx_only)

#calculating frequency table for secondary diagnosis
secondary_dx %>% 
  group_by(secondary_diagnosis) %>% 
  summarise(freq=n()) %>% 
  arrange(desc(freq))

#prevalence of severe anemia in patents with stroke
30/106*100
#=28.3

table(secondary_dx)

ggplot(data=secondary_dx)+
  geom_bar(mapping = aes(x=secondary_diagnosis, fill=Sex))+
  labs(title = paste("Secondary diagnosis for SCD children with stroke"), 
       subtitle = paste("(children admitted with sickle cell anaemia to Mulago Hospital)"),
       y="number of patients",
       x="secondary diagnosis", color="sex")

  
chisq.test(secondary_dx)
cor(secondary_dx$primary_diagnosis, secondary_dx$secondary_diagnosis)
