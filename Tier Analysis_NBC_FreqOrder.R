### Import data

library(readr)
FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier <- read_csv("FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier.csv", 
                                                           col_types = cols(X1 = col_skip()))
glimpse(FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier)


#NA function
pMISS <- function(x){sum(is.na(x))/length(x)*100}
apply(FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier, 2, pMISS) #less than 10% CRSNAME1:4


# Create new cols PriorInd and aveGPAInd
FreOrderTIERS <- FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier %>% 
  mutate(aveGPAInd=ifelse(FIRST_FALL_GPA >=2.77,"above2.77","below2.77" )) 

aveGPANB<- FreOrderTIERS %>% 
  select("APPLICANT_TIER", "ENTRY_COLLEGE", "ENTRY_DEPARTMENT","AP_CREDITS","COUNTY","GENDER", "ENTRY_PROGRAM" ,"HIGH_SCHOOL_NAME","ETHNICITY",
         "CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
         # "CRSNAME5",  "CRSNAME6",
         "HOURS_BROUGHT_TO_UNIVERSITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT","AGE_AT_ENTRY",
         "aveGPAInd") %>% 
  na.omit() # NA omitted #5646
# mutate(NEWCOUNTY=ifelse(COUNTY == "Escambia","ESCA", ifelse(COUNTY=="Santa Rosa","SANTAROSA",ifelse(COUNTY=="Okaloosa","OKAL",ifelse(COUNTY=="Non-Florida","NONFL","OTHERS"))))) %>% 
#  mutate(NEWETHNICITY=ifelse(ETHNICITY=="White","White",
#                            ifelse(ETHNICITY=="Hispanic","Hispanic",
#                                  ifelse(ETHNICITY=="African American","AA",
#                                          ifelse(ETHNICITY=="Two or More","Two/More",
#                                                ifelse(ETHNICITY=="Asian","Asian","OTHERS"))))))  
library(Hmisc)
Hmisc::describe(aveGPANB)

## FACTOR ################################       
aveGPANB$APPLICANT_TIER <- as.numeric(aveGPANB$APPLICANT_TIER)
aveGPANB$aveGPAInd <- as.factor(aveGPANB$aveGPAInd)
aveGPANB$GENDER <- as.factor(aveGPANB$GENDER)
#aveGPANB$NEWETHNICITY <- as.factor(aveGPANB$NEWETHNICITY)
aveGPANB$CRSNAME1 <- as.factor(aveGPANB$CRSNAME1)
aveGPANB$CRSNAME2 <- as.factor(aveGPANB$CRSNAME2)
aveGPANB$CRSNAME3 <- as.factor(aveGPANB$CRSNAME3)
aveGPANB$CRSNAME4 <- as.factor(aveGPANB$CRSNAME4)
aveGPANB$HIGH_SCHOOL_NAME <- as.factor(aveGPANB$HIGH_SCHOOL_NAME)
#aveGPANB$NEWCOUNTY <- as.factor(aveGPANB$NEWCOUNTY)
aveGPANB$ENTRY_COLLEGE <- as.factor(aveGPANB$ENTRY_COLLEGE)

## Data Partitions ############################
library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o
# Performs stratified random split of the data set
# selecting group
DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER <= 5) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()
### modelinh
library(naivebayes)
aveGPANB_model1 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model1t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf1 <- confusionMatrix(predict(aveGPANB_model1), aveGPAIndtTrainingSet$aveGPAInd) 
conf1t <- confusionMatrix(predict(aveGPANB_model1t), aveGPAIndTestingSet$aveGPAInd) 


DDF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER <= 4) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model2 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model2t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf2 <- confusionMatrix(predict(aveGPANB_model2), aveGPAIndtTrainingSet$aveGPAInd) 
conf2t <- confusionMatrix(predict(aveGPANB_model2t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER <= 3) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model3 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model3t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf3 <- confusionMatrix(predict(aveGPANB_model3), aveGPAIndtTrainingSet$aveGPAInd) 
conf3t <- confusionMatrix(predict(aveGPANB_model3t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER <= 2)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
aveGPANB_model4 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model4t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf4 <- confusionMatrix(predict(aveGPANB_model4), aveGPAIndtTrainingSet$aveGPAInd) 
conf4t <- confusionMatrix(predict(aveGPANB_model4t), aveGPAIndTestingSet$aveGPAInd) 


DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER <= 1)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model5 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model5t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf5 <- confusionMatrix(predict(aveGPANB_model5), aveGPAIndtTrainingSet$aveGPAInd) 
conf5t <- confusionMatrix(predict(aveGPANB_model5t), aveGPAIndTestingSet$aveGPAInd) 

#train dataset
conf1$overall
conf2$overall
conf3$overall
conf4$overall
conf5$overall
#test dataset
conf1t$overall
conf2t$overall
conf3t$overall
conf4t$overall
conf5t$overall

models <- c("5:1","4:1","3:1","2:1","1")
models
accuracy_Train <- c(round(conf1$overall["Accuracy"], 3),
                    round(conf2$overall["Accuracy"], 3),
                    round(conf3$overall["Accuracy"], 3),
                    round(conf4$overall["Accuracy"], 3),
                    round(conf5$overall["Accuracy"], 3))
accuracy_Test <- c(round(conf1t$overall["Accuracy"], 3),
                   round(conf2t$overall["Accuracy"], 3),
                   round(conf3t$overall["Accuracy"], 3),
                   round(conf4t$overall["Accuracy"], 3),
                   round(conf5t$overall["Accuracy"], 3))

accyFreq <- data.frame(models, accuracy_Train, accuracy_Test)


######################################################
######### Backward ##################################
#####################################################

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER >= 5) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()
### modelinh
library(naivebayes)
aveGPANB_model1 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model1t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf1 <- confusionMatrix(predict(aveGPANB_model1), aveGPAIndtTrainingSet$aveGPAInd) 
conf1t <- confusionMatrix(predict(aveGPANB_model1t), aveGPAIndTestingSet$aveGPAInd) 


DDF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER >= 4) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model2 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model2t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf2 <- confusionMatrix(predict(aveGPANB_model2), aveGPAIndtTrainingSet$aveGPAInd) 
conf2t <- confusionMatrix(predict(aveGPANB_model2t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER >= 3) %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model3 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model3t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf3 <- confusionMatrix(predict(aveGPANB_model3), aveGPAIndtTrainingSet$aveGPAInd) 
conf3t <- confusionMatrix(predict(aveGPANB_model3t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER >= 2)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
aveGPANB_model4 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model4t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf4 <- confusionMatrix(predict(aveGPANB_model4), aveGPAIndtTrainingSet$aveGPAInd) 
conf4t <- confusionMatrix(predict(aveGPANB_model4t), aveGPAIndTestingSet$aveGPAInd) 


DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER >= 1)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_model5 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model5t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

conf5 <- confusionMatrix(predict(aveGPANB_model5), aveGPAIndtTrainingSet$aveGPAInd) 
conf5t <- confusionMatrix(predict(aveGPANB_model5t), aveGPAIndTestingSet$aveGPAInd) 

#train dataset
conf1$overall
conf2$overall
conf3$overall
conf4$overall
conf5$overall
#test dataset
conf1t$overall
conf2t$overall
conf3t$overall
conf4t$overall
conf5t$overall


models <- c("5","4:5","3:5","2:5","1:5")
models
accuracy_Train <- c(round(conf1$overall["Accuracy"], 3),
                    round(conf2$overall["Accuracy"], 3),
                    round(conf3$overall["Accuracy"], 3),
                    round(conf4$overall["Accuracy"], 3),
                    round(conf5$overall["Accuracy"], 3))
accuracy_Test <- c(round(conf1t$overall["Accuracy"], 3),
                   round(conf2t$overall["Accuracy"], 3),
                   round(conf3t$overall["Accuracy"], 3),
                   round(conf4t$overall["Accuracy"], 3),
                   round(conf5t$overall["Accuracy"], 3))

accyFreqBW <- data.frame(models, accuracy_Train, accuracy_Test)


########  EACH TIER #################
DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER == 1)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_modelt1 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_modelt1t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

confTier1 <- confusionMatrix(predict(aveGPANB_modelt1), aveGPAIndtTrainingSet$aveGPAInd) 
confTier1t <- confusionMatrix(predict(aveGPANB_modelt1t), aveGPAIndTestingSet$aveGPAInd) 


DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER == 2)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_modelt2 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_modelt2t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

confTier2 <- confusionMatrix(predict(aveGPANB_modelt2), aveGPAIndtTrainingSet$aveGPAInd) 
confTier2t <- confusionMatrix(predict(aveGPANB_modelt2t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER == 3)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_modelt3 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_modelt3t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

confTier3 <- confusionMatrix(predict(aveGPANB_modelt3), aveGPAIndtTrainingSet$aveGPAInd) 
confTier3t <- confusionMatrix(predict(aveGPANB_modelt3t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER == 4)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_modelt4 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_modelt4t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

confTier4 <- confusionMatrix(predict(aveGPANB_modelt4), aveGPAIndtTrainingSet$aveGPAInd) 
confTier4t <- confusionMatrix(predict(aveGPANB_modelt4t), aveGPAIndTestingSet$aveGPAInd) 

DF <- aveGPANB %>% 
  filter(ENTRY_COLLEGE == "HMCSE") %>% 
  filter(APPLICANT_TIER == 5)  %>% 
  select(-ENTRY_COLLEGE) %>% 
  select(-ENTRY_DEPARTMENT) %>% 
  na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### modelinh
library(naivebayes)
aveGPANB_modelt5 <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_modelt5t <- naive_bayes(aveGPAInd ~ ., data = aveGPAIndTestingSet, usekernel = TRUE)

confTier5 <- confusionMatrix(predict(aveGPANB_modelt5), aveGPAIndtTrainingSet$aveGPAInd) 
confTier5t <- confusionMatrix(predict(aveGPANB_modelt5t), aveGPAIndTestingSet$aveGPAInd) 

models <- c("Tier1","Tier2","Tier3","Tier4","Tier5")
models
accuracy_Train <- c(round(confTier1$overall["Accuracy"], 3),
                    round(confTier2$overall["Accuracy"], 3),
                    round(confTier3$overall["Accuracy"], 3),
                    round(confTier4$overall["Accuracy"], 3),
                    round(confTier5$overall["Accuracy"], 3))
accuracy_Test <- c(round(confTier1t$overall["Accuracy"], 3),
                   round(confTier2t$overall["Accuracy"], 3),
                   round(confTier3t$overall["Accuracy"], 3),
                   round(confTier4t$overall["Accuracy"], 3),
                   round(confTier5t$overall["Accuracy"], 3))

accyFreqEACH <- data.frame(models, accuracy_Train, accuracy_Test)























####################################
### repeatable grouping #############
####################################
#install.packages("plyr");library(plyr) #plyr operate lists.data.frame,arrarys
pMISS <- function(x){sum(is.na(x))/length(x)*100}
get.n.Tiers <- function(x) length(unique(x$APPLICANT_TIER))
get.n.Tiers(aveGPANB) # give you 5
n.Tiers <- c(1:5)
daply(aveGPANB, .(ETHNICITY), function(x) length(unique(x$ENTRY_PROGRAM)))

## repeated modeling 
model <- function(x){
  naive_bayes(aveGPAInd~., data=aveGPANB)
}
fitted.na.model <- dlply(aveGPAIndtTrainingSet, .(APPLICANT_TIER), model) # all tiers and colleges

all.prd <- predict(fitted.na.model[[1]], aveGPAIndtTrainingSet, type="prob")

tables(aveGPAIndtTrainingSet$aveGPAInd, all.prd$class)
#repeated prediction


tables(fitted.na.model[[1]])

# summary for all fitted model
ldply(fitted.na.model, function(x) summary(x)$)

