
library(parallel)
library(dplyr)
library(tidyverse)

# Detect number of cores

nCores<-detectCores()

# Define cluster you make

myClust<-makeCluster(nCores-1,type="FORK")

# Bootstrap confidence intervals for all covariates and parameters

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

lmBoot(inputData,100,Birth_Weight ~ Length_of_Gestation_Days + as.factor(Number_of_previous_pregnancies) +
         mothers_height + fathers_race + fathers_weight + Time_since_mother_quit)

# Stop cluster used by lmBoot

stopCluster(myClust)