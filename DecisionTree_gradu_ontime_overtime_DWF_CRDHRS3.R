##################################
### Decision Tree ################
##################################

#need packages party, rpart, rpart.plot
library(party)
library(rpart)
library(rpart.plot)
library(dplyr)


#import data set
library(readr)
Rtreedataset_M <- read_csv("C:/Users/jsparks3/Downloads/Rtreedataset_M.csv", 
                           col_types = cols(X1 = col_skip(), Stu_ProgramCIPCodeChange = col_factor(levels = c()), 
                                            Stu_CollegeChange = col_factor(levels = c()), 
                                            Stu_DepartmentChange = col_factor(levels = c()), 
                                            Stu_Department3 = col_factor(levels = c()), 
                                            Stu_Gender = col_factor(levels = c()), 
                                            Deg_CollegeCode = col_factor(levels = c()), 
                                            Deg_Depar.Code = col_factor(levels = c()), 
                                            Gradu_Code = col_factor(levels = c())))
# characters are not supported, double or factor
Rtreedataset_M$Stu_Ethnicity <- as.factor(Rtreedataset_M$Stu_Ethnicity)


#Data partition
library(caret)
# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(Rtreedataset_M$Gradu_Code, p=0.8, list = FALSE)
TrainingSet <- Rtreedataset_M[TrainingIndex,] # Training Set 677
TestingSet <- Rtreedataset_M[-TrainingIndex,] # Test Set 166

#model 01 include Year 4 factors
treeGradu <- ctree(Gradu_Code ~ ., data=TrainingSet, controls = ctree_control(mincriterion = 0.9,minsplit = 10))
treeGradu
plot(treeGradu)



#confusion matrix
library(caret)
library(e1071)
confusionMatrix(predict(treeGradu), TrainingSet$Gradu_Code)

#export the results


# method is "class" since y is factor

tree1 <- rpart(Gradu_Code ~ Stu_ProgramCIPCodeChange+Stu_CollegeChange+Stu_TotalUniversityHoursBegin+Stu_DepartmentChange
               +Stu_TotalUniversityHours3+Stu_TotalInstHours3+Stu_Ethnicity+HoursDWFCount3+Stu_Gender+Deg_Depar.Code
               , data=TrainingSet, control = rpart.control(minsplit=20,cp=0.01))
rpart.plot(tree1, extra = 1)
rpart.plot(tree1, extra = 2)
rpart.plot(tree1, extra = 3)
rpart.plot(tree1, extra = 4)
tree1$variable.importance
varImp(tree1)



