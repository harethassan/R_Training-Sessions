install.packages("tidyverse")
install.packages("epiR")
install.packages("readxl")
install.packages("haven")
install.packages("lubridate")
install.packages("epitools")
install.packages("survival")
install.packages("table1")
install.packages("sf")

#loading
library(tidyverse)
library(epiR)
library(lubridate)

setwd("C:/Users/Peter Gatitu Mwangi/Downloads/r_programming")

options(timeout = 300)                     # 5 min instead of 60 s
install.packages("sf", type = "win.binary",
                 repos = "https://cloud.r-project.org")

#handling 
data <- read.csv("mpox.csv")
head(data)
head(data, n=12)

tail(data)

summary(data)

sum(is.na(data))

colSums(is.na(data))

rowSums(is.na(data))


data <- data %>%
  mutate(
    Date_Onset=as.Date(Date_Onset, format="%m/%d/%Y"),
    County=as.factor(County),
    sex= as.factor(Sex),
    Occupation = as.factor(Occupation),
    Case_Status=as.factor(Case_Status),
    Lab_Diagnosis=as.factor(Lab_Diagnosis),
    Travel_History = as.factor(Travel_History),
    Travel_Destination=as.factor(Travel_Destination)
  )

sum(data$Travel_Destination == "")
sum(is.na(data$Travel_Destination))

library(stringr)
data <- data %>% 
  mutate(Travel_Destination = ifelse(str_squish(Travel_Destination) == "not travelled",
                                     0,
                                     Travel_Destination
                                     ))

table(data$Travel_Destination)  
  
  
  
