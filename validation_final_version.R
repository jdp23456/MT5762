install.packages("dplyr") 
library(dplyr) 
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