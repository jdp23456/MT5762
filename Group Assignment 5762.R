library(dplyr)
library(tidyverse)
library(crunch)
library(ggthemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(caret)
library(parallel)
library(dplyr)
library(tidyverse)
# import this package to use dredge() to get subsets of models
library(MuMIn)
# import this package to use Anova(), ncvTest(), durbinWatsonTest() and vif()
library(car)
#-----------------------------------------------------------
#DATA CLEANING, PLOTTING, SUMMARIZING
#Read in the babies datafile
babies <- read.csv("babies23.data", sep = "")


#Remove the plurality, outcome, date, and sex columns from the dataframe as they are not useful for our analysis
babies <- subset(babies, select = -c(pluralty, outcome, date, sex))

#Rename mother's weight because R assigns wt.1 when there are 2 "wt" columns (wt is also the birth weight of the baby)
babies <- babies %>% rename(mothers_weight = wt.1)

#Filter all of the values that are unknown or not applicable to our analysis
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

#Replace all the unknown or non-applicable values with NA
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


#Write out variables as factors to ease confusion for what each number means for each column

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
babies$parity <- factor(babies$parity)


#Only looking at mothers who smoke, so we filter on smokes now
smokes_now <- babies %>% filter(smoke == "Smokes Now")

#Plotting :For mothers who smoked, plotting the quantity of cigarettes smoked per day vs baby birth weight
smokes_now %>% 
  gather(age, dage, ht, dht, gestation, dwt, mothers_weight, key = "param", value = "value") %>% 
  ggplot(aes(x = value, y= wt, colour = number)) +
  geom_point() + facet_wrap(~param, scales = "free") + theme_bw()


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



#------------------------------------------------------------------------
#MODEL SELECTION

# import this package to use dredge() to get subsets of models
library(MuMIn)
# import this package to use Anova(), ncvTest(), durbinWatsonTest() and vif()
library(car)

# read .csv data
raw_data <- read.csv("BabiesData.csv")
# omit NA value
processed_data <- na.omit(raw_data)
# number_of_data <- ceiling(nrow(processed_data) * 0.8)
# set.seed(1111)
# sequence_data <- sample(nrow(processed_data), number_of_data)
# model_data <- processed_data[sequence_data, ]
# test_data <- processed_data[-sequence_data, ]

# generate linear model
raw_model <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) + 
                  mothers_race + mothers_age + mothers_education + mothers_height + 
                  mothers_weight + fathers_race + fathers_age + fathers_education + 
                  fathers_height + fathers_weight + marital + Family_annual_income + 
                  smoke + Time_since_mother_quit + number_of_Cigs_per_day, 
                data = processed_data, na.action = "na.fail")

summary(raw_model)
Anova(raw_model)

# delete variable smoke because it is highly related to other variables
# delete variables which have very high p-value
raw_model <- update(raw_model, .~. - smoke - marital - Family_annual_income - fathers_age - mothers_age)
# delete variable number_of_Cigs_per_day because there are NAs in summary
# we can see from summary(raw_model), it shows that 'Coefficients: (1 not defined because of singularities)'
# NAs in the coefficients table means there are too few subjects to assess the influence of number_of_Cigs_per_days
# we can simplify this model by removing it
raw_model <- update(raw_model, .~. - number_of_Cigs_per_day)
summary(raw_model)
Anova(raw_model)

# raw_model <- dredge(raw_model)
# raw_model <- head(raw_model, n=10)

# try to add interactions in the raw_model
second_model <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) + 
                     mothers_race + mothers_education + mothers_height + mothers_weight + 
                     fathers_race + fathers_education + fathers_height + fathers_weight + 
                     Time_since_mother_quit + mothers_education:Length_of_Gestation_Days +
                     mothers_race:Length_of_Gestation_Days, data = processed_data, na.action = "na.fail")
summary(second_model)
Anova(second_model)

# using dredge() to find all possible subsets
# select top4 models with AIC value
new_model <- dredge(second_model)
models <- head(new_model, n=4)

# model_1 <- models[1]
# model_1 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + 
#                  mothers_race + mothers_education + mothers_height + fathers_weight + 
#                  Time_since_mother_quit + mothers_education:Length_of_Gestation_Days + 
#                  mothers_race:Length_of_Gestation_Days, data = model_data, 
#               na.action = "na.fail")

model_1 <- models[1]
model_1 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
                mothers_height + fathers_race + fathers_weight + Time_since_mother_quit,
              data = processed_data, na.action = "na.fail")

# model_2 <- models[2]
# model_2 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + mothers_race + mothers_education +
#                  mothers_height + fathers_height + fathers_weight + 
#                  Time_since_mother_quit + mothers_education:Length_of_Gestation_Days + 
#                  mothers_race:Length_of_Gestation_Days, data = model_data, 
#               na.action = "na.fail")

model_2 <- models[2]
model_2 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
                mothers_height + mothers_weight + fathers_race + fathers_weight + Time_since_mother_quit,
              data = processed_data, na.action = "na.fail")
# 
# model_3 <- models[3]
# model_3 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + mothers_race + 
#                  mothers_education + mothers_height + mothers_weight + fathers_weight + 
#                  Time_since_mother_quit + mothers_education:Length_of_Gestation_Days + 
#                  mothers_race:Length_of_Gestation_Days, data = model_data, 
#               na.action = "na.fail")

model_3 <- models[3]
model_3 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
                mothers_education + mothers_height + fathers_race + fathers_weight +
                Time_since_mother_quit + mothers_education:Length_of_Gestation_Days,
              data = processed_data, na.action = "na.fail")

# model_4 <- models[4]
# model_4 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + mothers_race + mothers_education + 
#                  mothers_height + fathers_race + fathers_weight + 
#                  Time_since_mother_quit + mothers_education:Length_of_Gestation_Days + 
#                  mothers_race:Length_of_Gestation_Days, data = model_data, 
#               na.action = "na.fail")

model_4 <- models[4]
model_4 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
                mothers_race + mothers_education + mothers_height + fathers_weight +
                Time_since_mother_quit + mothers_education:Length_of_Gestation_Days +
                mothers_race:Length_of_Gestation_Days, data = processed_data, na.action = "na.fail")

# Checking model assumptions


# Error Distribution

# p-value is 0.7971, fail to deny H0, so residuals of model obey normal distribution
qqnorm(resid(model_1))
qqline(resid(model_1))
shapiro.test(resid(model_1))
hist(resid(model_1))
# p-value is 0.7252, fail to deny H0, so residuals of model obey normal distribution
qqnorm(resid(model_2))
qqline(resid(model_2))
shapiro.test(resid(model_2))
hist(resid(model_2))
# p-value is 0.4678, fail to deny H0, so residuals of model obey normal distribution
qqnorm(resid(model_3))
qqline(resid(model_3))
shapiro.test(resid(model_3))
hist(resid(model_3))
# p-value is 0.4153, fail to deny H0, so residuals of model obey normal distribution
qqnorm(resid(model_4))
qqline(resid(model_4))
shapiro.test(resid(model_4))
hist(resid(model_4))

# constant spread

# p-value is 0.47521, fail to deny H0, so rediduals have constant variance
ncvTest(model_1)
# p-value is 0.35188, fail to deny H0, so rediduals have constant variance
ncvTest(model_2)
# p-value is 0.64445, fail to deny H0, so rediduals have constant variance
ncvTest(model_3)
# p-value is 0.90684, fail to deny H0, so rediduals have constant variance
ncvTest(model_4)

# Independence

# p-value is 0.06, fail to deny H0, so residuals are independent
durbinWatsonTest(model_1)
# p-value is 0.06, fail to deny H0, so residuals are independent
durbinWatsonTest(model_2)
# p-value is 0.07, fail to deny H0, so residuals are independent
durbinWatsonTest(model_3)
# p-value is 0.038, deny H0, so residuals are not independent
durbinWatsonTest(model_4)

# Collinearity

# Problem to be fixed
# What is the relation between interaction terms and collinearity?
# interaction variables in model_3 and model_4 seem to have high collinearity
vif(model_1)
vif(model_2)
vif(model_3)
vif(model_4)

#--------------------------------------------------------------------------
#VALIDATION OF FINAL VERSION

#we use cleaned up data 'processed_data'
babies_df <- processed_data

#according to the result of AIC we choose top two models as model_1 and model_2
model1 <- Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
  mothers_height + fathers_race + fathers_weight + Time_since_mother_quit

model2 <- Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
  mothers_height + mothers_weight + fathers_race + fathers_weight + Time_since_mother_quit

# VALIDATION ON 20% OF DATA
set.seed(111)
sample <- sample(seq_len(nrow(babies_df)),size = floor(0.8*nrow(babies_df)),replace=F)
training_sample <- babies_df[sample,]
validation_sample <- babies_df[-sample,]

##train model on training sample and predict using validation sample
#MODEL_1
train_model1 <- lm(formula=model1,data=training_sample) #train model_1
pred_model1 <- train_model1 %>% predict(validation_sample) #predict by model_1
mse_model1 <- mean((validation_sample$Birth_Weight - pred_model1)^2) #calculate mse for model1

#MODEL_2
train_model2 <- lm(model2,data=training_sample) #fit model2
pred_model2 <- train_model2%>%predict(validation_sample) #validate fitted model2
mse_model2 <- mean((validation_sample$Birth_Weight - pred_model2)^2) #calculate mse for model2

#----------------------------5-FOLD CROSS VALIDATION----------------------------------------------------------#
#(STEP-BY-STEP, i.e. splitting data-fitting model-predicting-calculate mse)
library(caret)
k=5
mse_five_fold <- NULL
#if number of data points in each level for Number_of_Pregnancies <5, remove 
subset(babies_df,Number_of_previous_pregnancies==11)
subset(babies_df,Number_of_previous_pregnancies==10)
subset(babies_df,Number_of_previous_pregnancies==9)
subset(babies_df,Number_of_previous_pregnancies==7)
a <- subset(babies_df, Number_of_previous_pregnancies <=6)
a <- rbind(a,subset(babies_df,Number_of_previous_pregnancies==9))

k_folds_cv <- function(k,model_to_fit){
  #split the data into 5 'folds'; assign one fold as validation set, and the remaining as training set
  folds <- createFolds(a$Birth_Weight,k=k,list=T,returnTrain = T) 
  for (i in 1:k){
    #fit model to the training set
    train_model <-lm(model_to_fit,data=a[folds[[i]],],)
    #use fitted model to predict values on validation set
    pred <- predict(object = train_model, newdata=a[-folds[[i]],])
    #calculate and store mse values
    mse_five_fold[i] <- mean((a[-folds[[i]],]$Birth_Weight-pred)^2)
  }
  print(mse_five_fold) #outputs mse for each cv (where each 'fold' was assigned the sample)
  mean(mse_five_fold) #outputs average of the mse values
}

#run k-fold cv function from above on both models to compare mse values
set.seed(111)
k_folds_cv(5,model1)
k_folds_cv(5,model2)

best_model <- model1
#END#

#---------------------------------------------------------
#BOOTSTRAPPING CI

# Detect number of cores

nCores<-detectCores()

# Define cluster you make

myClust<-makeCluster(nCores-1,type="FORK")

# Bootstrap confidence intervals for all covariates and parameters

inputData <-  processed_data

lmBoot <- function(inputData, nBoot,regmodel){
  
  # Create variable myformula to use in regression model later
  myformula<-as.formula(regmodel)
  
  # Create variable called input data
  nrowdata<-nrow(inputData)
  
  # Create a list for compiling Bootstraps
  bootList<-list()
  
  # Create a for loop to run multiple bootstraps
  for(i in 1:nBoot){
    
    # Create a Sample Index
    sampleIndex <- sample(1:nrowdata, nrowdata, replace = T)
    
    # Sample the dataset
    bootData<- inputData[sampleIndex,]
    
    #Compile all bootstrap samples into a list called bootData
    bootList[[i]]<-bootData
    
  }
  
  ourBootReg<- parLapply(myClust,bootList,function(itemFromList){coef(lm(myformula,data=itemFromList))})
  
  dataframeOfBootCoefs<-plyr::ldply(ourBootReg)
  
  # take central 95% of each columns esimates (simulated sampling dists) 
  
  parApply(myClust,dataframeOfBootCoefs,2,quantile,probs=c(0.025,0.975))
  
}

# Stop cluster used by lmBoot

stopCluster(myClust)

