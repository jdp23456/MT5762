library(car)

# read .csv data
raw_data <- read_csv("BabiesData.csv")
# omit NA value
processed_data <- na.omit(raw_data)

# generate linear model
raw_model <- lm(formula = Birth_Weight ~ ., data = processed_data)
# get rid of columns 'id' and 'X1' because they are not related
raw_model <- update(raw_model, . ~ . - id - X1)
# original summary of raw_model
summary(raw_model)
# p-value of each property
anova(raw_model)

# use CIA to perform model selection by step
raw_model <- step(raw_model)
summary(raw_model)
anova(raw_model)
# we can see from summary(raw_model), it shows that 'Coefficients: (1 not defined because of singularities)'
# NAs in the coefficients table means there are too few subjects to assess the influence of number_of_Cigs_per_days
# we can simplify this model by removing it
raw_model <- update(raw_model, .~. -number_of_Cigs_per_day)
summary(raw_model)

# **************************************************************************************

# Consider interactions between numeric and categorical covariates

# mothers_age * mothers_race
# fail
a<-lm(formula = Birth_Weight ~ mothers_age * mothers_race, data = processed_data)
summary(a)
# mothers_age * Time_since_mother_quit
# fail
a<-lm(formula = Birth_Weight ~ mothers_age * Time_since_mother_quit, data = processed_data)
summary(a)
# mothers_age * number_of_Cigs_per_day
# fail
a<-lm(formula = Birth_Weight ~ mothers_age * number_of_Cigs_per_day, data = processed_data)
summary(a)

# Consider interactions between numeric covariates

# mothers_age * Length_of_Gestation_Days
# fail
a<-lm(formula = Birth_Weight ~ mothers_age * Length_of_Gestation_Days, data = processed_data )
summary(a)
# mothers_height * mothers_weight
# fail
a<-lm(formula = Birth_Weight ~ mothers_height * mothers_weight, data = processed_data )
summary(a)
# mothers_height * Length_of_Gestation_Days
# fail
a<-lm(formula = Birth_Weight ~ mothers_height * Length_of_Gestation_Days, data = processed_data )
summary(a)

# Consider interactions between categorical covariates

# mothers_education * fathers_education
# fail
a<-lm(formula = Birth_Weight ~ mothers_education * fathers_education, data = processed_data )
summary(a)
# Family_annual_income * number_of_Cigs_per_day
# fail
a<-lm(formula = Birth_Weight ~ Family_annual_income * number_of_Cigs_per_day, data = processed_data )
summary(a)
# mothers_race * fathers_race
# fail
a<-lm(formula = Birth_Weight ~ mothers_race * fathers_race, data = processed_data )
summary(a)


# **************************************************************************************
# Model Dignostics

# Error Distribution
qqnorm(resid(raw_model))
qqline(resid(raw_model))
# p-value is 0.3683, fail to deny H0, so residuals of model obey normal distribution
shapiro.test(resid(raw_model))
# error shape
hist(resid(raw_model))
# constant spread
# p-value is 0.96137, fail to deny H0, so rediduals have constant variance
ncvTest(raw_model)
# independence
# p-value is 0.018, deny H0, that is not what I want
durbinWatsonTest(raw_model)

# Collinearity with VIFs
# no need to alter model
vif(raw_model)



