setwd("C:/Users/peter/Desktop/kap_analysis")

install.packages("table1")

library(tidyverse)
library(readxl)
library(table1)

naks_kap <- read_excel("KAP_Nakuru_Final_data.xlsx")

glimpse(naks_kap)

names(naks_kap)

demo <- naks_kap %>% 
  select(
    Country = "1. Country of Origin",
    County = "2. County of residence",
    Sub_County = "Sub county",
    Sex = "3. Sex",
    Age = "4. Age",
    Occupation = "5. Occupation of the Interviewee",
    Education = "6. Highest level of education"
  )

colSums(is.na(demo))

demo <- demo %>% 
  mutate(across(everything(), ~replace_na(.x, "none")))

view(demo)

str(demo)

#20th October 2025
glimpse(demo)# know the data type
colSums(is.na(demo))#view missing data

demo <- demo %>% drop_na(county) #remove this function if necessary
remove(test)
demo %>%
  mutate(across(everything(), ~replace_na(.x,"none"))) #applies transformation to all columns&replaces "na" to none
view(demo)  
#Descriptive analysis  

Country_Occupation <- demo %>% 
  count(country,occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(country,occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)") 

#Age group analysis (option 1)
age_occupation <-naks_kap %>%
  select(occupation="5. Occupation of the Interviewee", 
         age="4. Age")
glimpse(age_occupation) 
colSums(is.na(age_occupation))

age_occupation <- age_occupation %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 27 ~ "18–27",
    age >= 28 & age <= 37 ~ "28–37",
    age >= 38 & age <= 47 ~ "38–47",
    age >= 48 & age <= 57 ~ "48–57",
    age >= 58 & age <= 67 ~ "58–67",
    age >= 68 ~ "68+",
    TRUE ~ NA_character_
  ))
#option(1)
age_occupation %>% 
  count(age_group,occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(age_group,occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)")

#option(2) this formula gives occupation(column) against age-group(observations) only % without "n"
age_occupation%>%
  tabyl(age_group,occupation) %>% 
  adorn_percentages("col") %>%
  adorn_totals("row") %>%
  adorn_pct_formatting(digits = 1)
#option(3) this formula gives occupation(column) against age-group(observations) with n outside 
age_occupation %>%
  tabyl(age_group, occupation) %>%
  adorn_totals("row") %>%                           # Add total row
  adorn_percentages("col") %>%                      # Calculate column percentages
  adorn_pct_formatting(digits = 1) %>%              # Format as %
  adorn_ns(position = "front") 

#option(4) this formula gives occupation(column) against age-group(observations) with n inside the baracket
age_summary <- age_occupation %>%
  tabyl(age_group,occupation) %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "rear")

#distribution of occupation
age_occupation %>% 
  count(occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)")


age_occupation %>%
  tabyl(occupation) %>%                           # Get counts
  adorn_totals("row") %>%                         # Optional: add total row
  adorn_percentages("col") %>%                    # Compute column percentages
  adorn_pct_formatting(digits = 2) %>%            # Format as %
  rename(Frequency = n, `Percent` = percent)
#sex by occupation
sex_Occupation <- demo %>% 
  count(sex,occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(sex,occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)") 
#Highest level of education
edu_Occupation <- demo %>% 
  count(education,occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(education,occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)") 
#County of residence vs the occupation
county_residence <- demo %>% 
  count(county,occupation) %>% 
  group_by(occupation) %>% 
  mutate(pct=round(100*n/sum(n),1),
         label=paste0(pct,"%(",n,")")) %>% 
  select(county,occupation,label) %>% 
  pivot_wider(names_from = occupation,values_from = label, values_fill = "0%(0)") 



# Convert categorical variables to factors

demo <- demo %>% 
  mutate(across(c(County, Country, Sub_County, Occupation, Sex, Education), as.factor))


# Table 1a: Occupation by Country
table1(~ Country + County + Sub_County + Sex + Education | Occupation, data = demo, 
       caption = "Table 1b: Demographic of Information of NAKURU")





