# import this package to use dredge() to get subsets of models
library(MuMIn)
# import this package to use Anova(), ncvTest(), durbinWatsonTest() and vif()
library(car)

# read .csv data
raw_data <- read.csv("BabiesData.csv")
# omit NA value
processed_data <- na.omit(raw_data)

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

model_1 <- models[1]
model_1 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) + 
                                  mothers_height + fathers_race + fathers_weight + Time_since_mother_quit, 
                                  data = processed_data, na.action = "na.fail")
model_2 <- models[2]
model_2 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) + 
                                  mothers_height + mothers_weight + fathers_race + fathers_weight + Time_since_mother_quit, 
                                  data = processed_data, na.action = "na.fail")
model_3 <- models[3]
model_3 <- lm(formula = Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) + 
                                  mothers_education + mothers_height + fathers_race + fathers_weight + 
                                  Time_since_mother_quit + mothers_education:Length_of_Gestation_Days,  
                                  data = processed_data, na.action = "na.fail")
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