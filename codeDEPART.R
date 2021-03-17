mydepart <- myFTIC %>% select(contains("College"), contains("Department"))
DEPRT <- mydepart %>% select(3,7,8)
DEPRT[!duplicated(DEPRT),]
DEPART1 <- DEPRT[!duplicated(DEPRT),]
myDEPARTcode <- DEPART1 %>% select(codeCollege="Stu_CollegeCode.y",codeDEPRT="Stu_DepartmentCode.y",DEPART="Stu_Department.y")
glimpse(DEPART1)

