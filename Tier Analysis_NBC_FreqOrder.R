### Import data

library(readr)
V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier <- 
  read.csv("~/Data_IR/V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier.csv", stringsAsFactors=TRUE)
glimpse(V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier)
 

### select columns
aveGPANB<- V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier %>% 
  select("UNIV_ROW_ID","COHORT_YEAR","APPLICANT_TIER", "GPA_HIGHSCHOOL","ENTRY_COLLEGE", "ENTRY_DEPARTMENT",
         "AP_CREDITS","COUNTY","GENDER", "ENTRY_PROGRAM" ,"HIGH_SCHOOL_NAME","ETHNICITY",
         "CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
         "HOURS_BROUGHT_TO_UNIVERSITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT","AGE_AT_ENTRY",
         79:84) %>% 
  filter(COHORT_YEAR < 20202021 & ENTRY_COLLEGE=="HMCSE")  


### NEW DATA ###
aveGPANB2020 <- V1FreqOrderwith9crs_MASTER_CRSCSE_allcourse_tier %>% 
  select("UNIV_ROW_ID","COHORT_YEAR","APPLICANT_TIER", "GPA_HIGHSCHOOL", "ENTRY_COLLEGE", "ENTRY_DEPARTMENT",
         "AP_CREDITS","COUNTY","GENDER", "ENTRY_PROGRAM" ,"HIGH_SCHOOL_NAME","ETHNICITY",
         "CRSNAME1",  "CRSNAME2",  "CRSNAME3",  "CRSNAME4", 
         "HOURS_BROUGHT_TO_UNIVERSITY","FIRST_FALL_PELL_AMOUNT","FIRST_FALL_BRIGHT_FUTURES_AMOUNT","AGE_AT_ENTRY",
         79:84) %>% 
  filter(COHORT_YEAR == 20202021 & ENTRY_COLLEGE=="HMCSE") 

## Data Partitions ############################
library(rsample)  # data splitting 
library(dplyr)    # data transformation
library(ggplot2)  # data visualization
library(caret)    # implementing with caret
library(h2o)      # implementing with h2o
library(naivebayes)

# Performs stratified random split of the data set

#####################################
########  EACH TIER #################
#####################################


# TIER 1
DF <- aveGPANB %>% 
     filter(APPLICANT_TIER == 1) %>% 
     na.omit()

set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### models
#TRAIN SET
aveGPANB_modelt1 <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                data = aveGPAIndtTrainingSet, usekernel = TRUE)
#TEST SET
aveGPANB_modelt1t <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                   HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                   FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                   ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                   CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4,  
                                 data = aveGPAIndTestingSet, usekernel = TRUE)

confTier1 <- confusionMatrix(predict(aveGPANB_modelt1), aveGPAIndtTrainingSet$aveGPAInd) 
confTier1t <- confusionMatrix(predict(aveGPANB_modelt1t), aveGPAIndTestingSet$aveGPAInd) 
### export the results
p1 <-  predict(aveGPANB_modelt1, aveGPAIndtTrainingSet, type="prob")
results1 <- cbind(aveGPAIndtTrainingSet,p1)

### NEW data2020
DF2020 <- aveGPANB2020 %>% 
  filter(APPLICANT_TIER == 1) 

p12020 <-  predict(aveGPANB_modelt1, DF2020, type="prob")
results12020 <- cbind(DF2020,p12020)


# TIER 2
DF <- aveGPANB %>% 
  filter(APPLICANT_TIER == 2) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### models
#TRAIN SET
aveGPANB_modelt2 <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                data = aveGPAIndtTrainingSet, usekernel = TRUE)
#TEST SET
aveGPANB_modelt2t <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                   HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                   FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                   ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                   CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                 data = aveGPAIndTestingSet, usekernel = TRUE)


confTier2 <- confusionMatrix(predict(aveGPANB_modelt2), aveGPAIndtTrainingSet$aveGPAInd) 
confTier2t <- confusionMatrix(predict(aveGPANB_modelt2t), aveGPAIndTestingSet$aveGPAInd)

p2 <-  predict(aveGPANB_modelt2, aveGPAIndtTrainingSet, type="prob")
results2 <- cbind(aveGPAIndtTrainingSet,p2)

### NEW data2020
DF2020 <- aveGPANB2020 %>% 
  filter(APPLICANT_TIER == 2)

p22020 <-  predict(aveGPANB_modelt2, DF2020, type="prob")
results22020 <- cbind(DF2020,p22020)

# TIER 3
DF <- aveGPANB %>% 
  filter(APPLICANT_TIER == 3) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### models
library(naivebayes)
#TRAIN SET
aveGPANB_modelt3 <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                data = aveGPAIndtTrainingSet, usekernel = TRUE)
#TEST SET
aveGPANB_modelt3t <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                   HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                   FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                   ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                   CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                 data = aveGPAIndTestingSet, usekernel = TRUE)

confTier3 <- confusionMatrix(predict(aveGPANB_modelt3), aveGPAIndtTrainingSet$aveGPAInd) 
confTier3t <- confusionMatrix(predict(aveGPANB_modelt3t), aveGPAIndTestingSet$aveGPAInd) 
p3 <-  predict(aveGPANB_modelt3, aveGPAIndtTrainingSet, type="prob")
results3 <- cbind(aveGPAIndtTrainingSet,p3)

### NEW data2020
DF2020 <- aveGPANB2020 %>% 
  filter(APPLICANT_TIER == 3) 

p32020 <-  predict(aveGPANB_modelt3, DF2020, type="prob")
results32020 <- cbind(DF2020,p32020)

# TIER 4
DF <- aveGPANB %>% 
  filter(APPLICANT_TIER == 4) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### models
library(naivebayes)
#TRAIN SET
aveGPANB_modelt4 <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                data = aveGPAIndtTrainingSet, usekernel = TRUE)
#TEST SET
aveGPANB_modelt4t <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                   HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                   FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                   ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                   CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                 data = aveGPAIndTestingSet, usekernel = TRUE)


confTier4 <- confusionMatrix(predict(aveGPANB_modelt4), aveGPAIndtTrainingSet$aveGPAInd) 
confTier4t <- confusionMatrix(predict(aveGPANB_modelt4t), aveGPAIndTestingSet$aveGPAInd) 
p4 <-  predict(aveGPANB_modelt4, aveGPAIndtTrainingSet, type="prob")
results4 <- cbind(aveGPAIndtTrainingSet,p4)

### NEW data2020
DF2020 <- aveGPANB2020 %>% 
  filter(APPLICANT_TIER == 4) 

p42020 <-  predict(aveGPANB_modelt4, DF2020, type="prob")
results42020 <- cbind(DF2020,p42020)

# TIER 5
DF <- aveGPANB %>% 
  filter(APPLICANT_TIER == 5) %>% 
  na.omit()


set.seed(1234)
TrainingIndex <- createDataPartition(DF$aveGPAInd, p=0.7, list = FALSE)
aveGPAIndtTrainingSet <- DF[TrainingIndex,] # Training Set
aveGPAIndTestingSet <- DF[-TrainingIndex,] # Test Set
class(aveGPAIndTestingSet)
#prop.table for GPA
table(aveGPAIndtTrainingSet$aveGPAInd) %>% prop.table()
table(aveGPAIndTestingSet$aveGPAInd) %>% prop.table()

### models
library(naivebayes)
#TRAIN SET
aveGPANB_modelt5 <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                data = aveGPAIndtTrainingSet, usekernel = TRUE)
#TEST SET
aveGPANB_modelt5t <- naive_bayes(aveGPAInd ~ AP_CREDITS+GENDER++ETHNICITY+
                                   HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                   FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY+
                                   ENTRY_PROGRAM+HIGH_SCHOOL_NAME+COUNTY+
                                   CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4, 
                                 data = aveGPAIndTestingSet, usekernel = TRUE)


confTier5 <- confusionMatrix(predict(aveGPANB_modelt5), aveGPAIndtTrainingSet$aveGPAInd) 
confTier5t <- confusionMatrix(predict(aveGPANB_modelt5t), aveGPAIndTestingSet$aveGPAInd) 
p5 <-  predict(aveGPANB_modelt5, aveGPAIndtTrainingSet, type="prob")
results5 <- cbind(aveGPAIndtTrainingSet,p5)

### NEW data2020
DF2020 <- aveGPANB2020 %>% 
  filter(APPLICANT_TIER == 5)

p52020 <-  predict(aveGPANB_modelt5, DF2020, type="prob")
results52020 <- cbind(DF2020,p52020)

#Accuracy tables
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
accuracy_CI <- c(round(confTier1$overall["AccuracyPValue"], 3),
                    round(confTier2$overall["AccuracyPValueI"], 3),
                    round(confTier3$overall["AccuracyPValue"], 3),
                    round(confTier4$overall["AccuracyPValue"], 3),
                    round(confTier5$overall["AccuracyPValue"], 3))

accyFreqEACH <- data.frame(models, accuracy_Train, accuracy_Test,accuracy_CI)


##export results

rowresultsV3 <- rbind.data.frame(results1, results2, results3, results4, results5,results12020,results22020,
                               results32020,results42020,results52020)
write.csv(rowresultsV3, "NBC_By_Tier_resultsV3.csv")









#############################################################
#################### Forward ###############################
###########################################################
# selecting group
DF <- aveGPANB %>% 
  filter(APPLICANT_TIER <= 5) %>% 
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
aveGPANB_model1 <- naive_bayes(aveGPAInd ~ APPLICANT_TIER+AP_CREDITS+COUNTY+GENDER+
                                            ENTRY_PROGRAM+HIGH_SCHOOL_NAME+ETHNICITY+
                                            CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+ 
                                            HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                            FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY, 
                                            data = aveGPAIndtTrainingSet, usekernel = TRUE)
aveGPANB_model1t <- naive_bayes(aveGPAInd ~ aveGPAInd ~ APPLICANT_TIER+AP_CREDITS+COUNTY+GENDER+
                                  ENTRY_PROGRAM+HIGH_SCHOOL_NAME+ETHNICITY+
                                  CRSNAME1+CRSNAME2+CRSNAME3+CRSNAME4+ 
                                  HOURS_BROUGHT_TO_UNIVERSITY+FIRST_FALL_PELL_AMOUNT+
                                  FIRST_FALL_BRIGHT_FUTURES_AMOUNT+AGE_AT_ENTRY, 
                                data = aveGPAIndTestingSet, usekernel = TRUE)

conf1 <- confusionMatrix(predict(aveGPANB_model1), aveGPAIndtTrainingSet$aveGPAInd) 
conf1t <- confusionMatrix(predict(aveGPANB_model1t), aveGPAIndTestingSet$aveGPAInd) 


DF <- aveGPANB %>% 
  filter(APPLICANT_TIER <= 4) %>% 
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
  filter(APPLICANT_TIER <= 3) %>% 
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
  filter(APPLICANT_TIER <= 2) %>% 
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
  filter(APPLICANT_TIER <= 1) %>% 
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
library(e1071)
varImp(aveGPANB_model1)
varImp(aveGPANB_model2)
varImp(aveGPANB_model3)
varImp(aveGPANB_model4)
varImp(aveGPANB_model5)

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


############# descriptive STST)
qplot(AP_CREDITS, data=myGPAdata,geom = "histogram", fill=aveGPAInd, bins=10)
qplot(HOURS_BROUGHT_TO_UNIVERSITY, data=myGPAdata,geom = "histogram", fill=aveGPAInd, bins=10)

