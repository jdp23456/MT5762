library(dplyr)
library(tidyverse)
library(crunch)
library(ggthemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)


babies <- read.csv("babies23.data", sep = "")
#babies <- read.csv("babies.csv")
babies

#Remove the plurality, outcome, date, and sex columns from the dataframe 
babies <- subset(babies, select = -c(pluralty, outcome, date, sex))

#Rename mother's weight because R assigns wt.1 when there are 2 "wt" columns (wt is the birth weight of the baby)
babies <- babies %>% rename(mothers_weight = wt.1)



babies %>% filter(wt == 999)
babies %>% filter(gestation == 999)
babies %>% filter(race == 99 | race == 10)
babies %>% filter(inc == 98 | inc == 99)
babies %>% filter(age == 99)
babies %>% filter(ed == 9)
babies %>% filter(ht == 99)
babies %>% filter(smoke == 9)
babies %>% filter(time == 98 | time == 99 | time == 9)
babies %>% filter(drace == 99 | drace == 10)
babies %>% filter(dage == 99)
babies %>% filter(ded == 9)
babies %>% filter(dht == 99)
babies %>% filter(number == 98 | number == 99 | number == 9)
babies %>% filter(dwt == 999)
babies %>% filter(mothers_weight == 999)

#Replace unknown values with NA
babies$wt[babies$wt == 999] = NA
babies$gestation[babies$gestation == 999] = NA
babies$race[babies$race == 99] = NA
babies$race[babies$race == 10] = NA
babies$inc[babies$inc == 98] = NA
babies$inc[babies$inc == 99] = NA
babies$age[babies$age == 99] = NA
babies$ed[babies$ed == 9] = NA
babies$ht[babies$ht == 99] = NA
babies$smoke[babies$smoke == 9] = NA
babies$time[babies$time == 98] = NA
babies$time[babies$time == 99] = NA
babies$time[babies$time == 9] = NA
babies$number[babies$number == 98] = NA
babies$number[babies$number == 99] = NA
babies$number[babies$number == 9] = NA
babies$drace[babies$drace == 99] = NA
babies$drace[babies$drace == 10] = NA
babies$dage[babies$dage == 99] = NA
babies$ded[babies$ded == 9] = NA
babies$dht[babies$dht == 99] = NA
babies$dwt[babies$dwt == 999] = NA
babies$mothers_weight[babies$mothers_weight == 999] = NA


#Set all mother's and father's race from 0-5 as 5, because they are all white
#Set all Mother's and Father's education for "Trade school, HS unclear" as 6 because 6 and 7 are interchangable
babies$race[babies$race %in% 0:5] = 5
babies$drace[babies$drace %in% 0:5] = 5
babies$ed[babies$ed %in% 6:7] = 6
babies$ded[babies$ed %in% 6:7] = 6
babies$parity[babies$parity %in% 10:13]=10


#Write out variables as factors to ease confusion for what each number means 

babies$smoke <- factor(babies$smoke, 
                       levels = c(0, 1, 2, 3), 
                       labels = c("Never", "Smokes Now", "Smoked Until Current Pregnancy", "Once smoked, doesn't now"))
babies$time <- factor(babies$time, 
                      levels = c(0, 1, 2, 3, 4, 5, 6, 7), 
                      labels = c("Never smoked", "Still smokes", "During current pregnancy", "Within 1 year", "1 to 2 years ago", "2 to 3 years ago","3 to 4 years ago", "5 to 9 years ago"))
babies$inc <- factor(babies$inc, 
                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                     labels = c("Under 2500", "2500 to 4999", "5000 to 6250", "6250 to 7500", "7500 to 8750", "8750 to 10000", "10,000 to 11250", "11250 to 12500", "12500 to 14999", "Over 15000" ))  
babies$number <- factor(babies$number, 
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                        labels = c("0", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 29", "30 to 39", "40 to 60", "Over 60", "smoke but dont know"))
babies$ed <- factor(babies$ed,
                    levels = c(1, 2, 3, 4, 5, 6), 
                    labels = c( "8th to 12th grade and did not graduate", "HS graduate but no other schooling", "HS and trade","HS and some college", "College graduate", "Trade school HS unclear"))
babies$ded <- factor(babies$ded,
                     levels = c(1, 2, 3, 4, 5, 6), 
                     labels = c( "8th to 12th grade and did not graduate", "HS graduate but no other schooling", "HS and trade","HS and some college", "College graduate", "Trade school HS unclear"))
babies$race <- factor(babies$race,
                      levels = c(5, 6, 7, 8, 9), 
                      labels = c("White", "Mexican", "Black", "Asian", "Mixed"))
babies$drace <- factor(babies$drace,
                       levels = c(5, 6, 7, 8, 9), 
                       labels = c("White", "Mexican", "Black", "Asian", "Mixed"))
babies$marital <- factor(babies$marital, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("Married", "Legally separated", "Divorced", "Widowed", "Never married"))


#Plotting :For mothers who smoked, plotting the quantity of cigarettes smoked per day vs baby birth weight
smokes_now <- babies %>% filter(smoke == "Smokes Now")

smokes_now %>% 
  gather(age, dage, ht, dht, gestation, dwt, mothers_weight, key = "param", value = "value") %>% 
  ggplot(aes(x = value, y= wt, colour = number)) +
  geom_smooth() + facet_wrap(~param, scales = "free") + theme_bw()


babies %>% 
  gather(age, ht, gestation, mothers_weight, key = "param", value = "value") %>% 
  ggplot(aes(x = value, y= wt, colour = smoke)) +
  geom_smooth() + facet_wrap(~param, scales = "free") + theme_bw()





#Rename headers to further explain what each column means
babies <- babies %>% rename(Birth_Weight = wt)
babies <- babies %>% rename(mothers_education = ed)
babies <- babies %>% rename(mothers_race = race)
babies <- babies %>% rename(mothers_age = age)
babies <- babies %>% rename(mothers_height = ht)
babies <- babies %>% rename(fathers_education = ded)
babies <- babies %>% rename(fathers_height = dht)
babies <- babies %>% rename(fathers_weight = dwt)
babies <- babies %>% rename(fathers_race = drace)
babies <- babies %>% rename(fathers_age = dage)
babies <- babies %>% rename(Family_annual_income = inc)
babies <- babies %>% rename(number_of_Cigs_per_day = number)
babies <- babies %>% rename(Number_of_previous_pregnancies = parity)
babies <- babies %>% rename(Time_since_mother_quit = time)
babies <- babies %>% rename(Length_of_Gestation_Days = gestation)




#Write out new csv file with all NA values accounted for, and better explanations of each variable in the dataset
write.csv(babies, file = "BabiesData.csv")



