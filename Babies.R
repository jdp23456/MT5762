library(dplyr)
library(tidyverse)
library(crunch)

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
babies$ded[babies$ded %in% 6:7] = 6


#Write out variables as factors to ease confusion for what each number means 

babies$smoke <- factor(babies$smoke, 
                       levels = c(0, 1, 2, 3), 
                       labels = c("Never", "Smokes Now", "Smoked Until Current Pregnancy", "Once smoked, doesn't now"))

babies$time <- factor(babies$time, 
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 0),
                      labels = c("0", "0.5", "1", "1.5", "2.5", "3.5", "7", "11", "20"))
              #"Still smokes", "During current pregnancy", "Within 1 year", "1 to 2 years ago", 
              #"2 to 3 years ago","3 to 4 years ago", "5 to 9 years ago", "Never smoked"))

#Family income for the household, on average
babies$inc <- factor(babies$inc, 
                     levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 
                     labels = c("2000", "3750", "5750", "6875", "8125", "9375", "10625", "11875", "13750", "17500"))  

#Number of cigarettes smoked per day by the mother, on average
babies$number <- factor(babies$number, 
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8), 
                        labels = c("O", "2.5", "7", "12", "17", "25", "35", "50", "65"))

#Number of years of education mother took, averaged
babies$ed <- factor(babies$ed,
                    levels = c(1, 2, 6, 3, 4, 5), 
                    labels = c("10", "12", "12", "13", "14", "16"))
# c("8th to 12th grade and did not graduate", "HS graduate but no other schooling", 
# "Trade school HS unclear","HS and trade", "HS and some college", "College graduate"))

#Number of years of education father took 
babies$ded <- factor(babies$ded,
                     levels = c(1, 2, 6, 3, 4, 5), 
                     labels = c("10", "12", "12", "13", "14", "16"))

babies$race <- factor(babies$race,
                      levels = c(5, 6, 7, 8, 9), 
                      labels = c("White", "Mexican", "Black", "Asian", "Mixed"))

babies$drace <- factor(babies$drace,
                       levels = c(5, 6, 7, 8, 9), 
                       labels = c("White", "Mexican", "Black", "Asian", "Mixed"))

babies$marital <- factor(babies$marital, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("Married", "Legally separated", "Divorced", "Widowed", "Never married"))


# #Rename headers to further explain what each column means
# babies <- babies %>% rename(Birth_Weight = wt)
# babies <- babies %>% rename(mothers_education = ed)
# babies <- babies %>% rename(mothers_race = race)
# babies <- babies %>% rename(mothers_age = age)
# babies <- babies %>% rename(mothers_height = ht)
# babies <- babies %>% rename(fathers_education = ded)
# babies <- babies %>% rename(fathers_height = dht)
# babies <- babies %>% rename(fathers_weight = dwt)
# babies <- babies %>% rename(fathers_race = drace)
# babies <- babies %>% rename(fathers_age = dage)
# babies <- babies %>% rename(Family_annual_income = inc)
# babies <- babies %>% rename(number_of_Cigs_per_day = number)
# babies <- babies %>% rename(Number_of_previous_pregnancies = parity)
# babies <- babies %>% rename(Time_since_mother_quit = time)
# babies <- babies %>% rename(Length_of_Gestation_Days = gestation)

#Plot number of cigarettes per day vs baby birth weight
ggplot(babies, aes(number, wt)) + geom_boxplot()

#Plot the number of cigaretttes per day vs family income
ggplot(babies, aes(inc, wt)) + geom_boxplot()

#Plot gestation time in days vs baby weight
ggplot(babies, aes(gestation, wt)) + geom_point()
#Results show the optimal gestation period is about 294 days
#Full term babies are at 42 weeks gestation


ggplot(babies, aes(dage, wt)) + geom_smooth()

ggplot(babies, aes(ed, wt)) + geom_boxplot()


#Write out new csv file with all NA values accounted for, and better explanations of each variable in the dataset
write.csv(babies, file = "BabiesData.csv")



