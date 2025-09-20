setwd("C:/Users/Admin/OneDrive/Desktop/R_Revision")

library(tidyr)
library(dplyr)
#Import the dataset in csv
Mpox_data <- read.csv("Mpox_Linelist_Kenya_2025.csv")
#inspect the dataset
head(Mpox_data)
tail(Mpox_data)
summary(Mpox_data)
glimpse(Mpox_data)

# convert data types
Mpox_data$Date_Onset <- as.Date(Mpox_data$Date_Onset,"%d/%m/%Y")
glimpse(Mpox_data)

Mpox_data <- Mpox_data %>% 
  mutate(
    County=as.factor(County),
    Sex=as.factor(Sex),
    Occupation=as.factor(Occupation),
    Case_Status=as.factor(Case_Status),
    Lab_Diagnosis=as.factor(Lab_Diagnosis),
    Travel_History=as.factor(Travel_History),
    Travel_Destination=as.factor(Travel_Destination),
    age_group=as.factor(age_group),
    age_cohort=as.factor(age_cohort)
  )
summary(Mpox_data)
#Checking for missing values
sum(is.na(Mpox_data))
colSums(is.na(Mpox_data))
rowSums(is.na(Mpox_data))

#Create new variables
Mpox_data <- Mpox_data %>% 
  mutate(age_group=if_else(Age<35,"young adult", "age adult"))

Mpox_data <- Mpox_data %>% 
  mutate(age_cohort=case_when(
  Age<=25~"0-25", Age<=40~"26-40", Age>=41~"41+"
  ))
#Subset and filter functions creating variables
Busia_males <-subset(Mpox_data, Sex=="Male" & County=="Busia")

Busia_males_2 <- Mpox_data %>% 
  filter(Sex=="Male" & County=="Busia")

Busia <- Mpox_data %>% 
  filter(County=="Busia")

glimpse(Mpox_data)  
summary(Mpox_data)
summary(Busia)
#Create dataframe using summarise function
County_summary <- Mpox_data %>% 
  group_by(County,Sex) %>% 
  summarise(mean_age=round(mean(Age, na.rm=TRUE,2)),total_cases= n())
print(County_summary)

#How to save cleam dataset of Mpox
write.csv(Mpox_data,"Cleaned_Mpox_Data.csv", row.names = FALSE)



