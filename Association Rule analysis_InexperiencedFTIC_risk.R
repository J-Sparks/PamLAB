#################################################
#### Association Rule for prior hours == 0 ######
#################################################


cleanFTIC_all_falls_BdegDF <- read.csv("~/lowerGPA_all_falls/cleanFTIC_all_falls_BdegDF.csv", stringsAsFactors=TRUE)

xtabs(~cleanFTIC_all_falls_BdegDF$Cohort+cleanFTIC_all_falls_BdegDF$Termindex)


FTIC_GPA_com <- cleanFTIC_all_falls_BdegDF %>% 
  group_by(Termindex,PriorUniAPR,APRGPA1stFall) %>% 
  summarise(meanGPA = mean(GPAEachFalls),meanTermHrs=mean(Stu_GPATermHours),meanPriorHrs=mean(Stu_TotalUniversityHours),CountFTIC=n())  #(meanGPA = mean(GPA1stFall),
FTIC_GPA_com


summary(cleanFTIC_all_falls_BdegDF)


############################################
#### select variables  #####################
###########################################



outDEGGPAFTIC_1st_term_association <- cleanFTIC_all_falls_BdegDF %>% 
  filter( PriorUniAPR =="NoPriorHrs" & Termindex== 1) %>% # for the 1st fall
  filter(GPAEachFalls >  0.28626667 , na.rm=TRUE) %>%  # outliers <=  0.2866667
  select(Gender="Stu_Gender",County="Stu_County",Ethnicity="Stu_Ethnicity",Entcollege="Stu_College",
         Stu_Department,
         ResidenceHall="Stu_ResidenceHall",
         #Stu_GPATermHours,
        #rankGPAEachFall,
        APRGPA1stFall,
        #Deg_TermGranted
        #,DegCollege="Deg_College"
        ) #1490/1617


summary(outDEGGPAFTIC_1st_term_association)

#check NA
p <- function(x){sum(is.na(x))/length(x)*100}
apply(outDEGGPAFTIC_1st_term_association, 2, p)




# need libraries
library(arules)
library(arulesViz)


#summary
DEGGPAFTIC_1st_term_associationDF <- as(outDEGGPAFTIC_1st_term_association,"transactions") #transaction are analysed to identify rules of assocation
class(DEGGPAFTIC_1st_term_associationDF)
#inspect(head(newGPAassociationDF)) # too long
itemFrequencyPlot(DEGGPAFTIC_1st_term_associationDF, topN=10)
itemFrequencyPlot(DEGGPAFTIC_1st_term_associationDF, support=0.1)

# results for above2.7
rules_GPAabove <- apriori(DEGGPAFTIC_1st_term_associationDF,parameter = list(minlen=1, maxlen=6, conf=.1, support=0.05), # to see whether there is sufficient evidence to suggest
                    appearance = list(rhs=c("APRGPA1stFall=Above2.77"), default = "lhs"))
rules_GPAabovee
resultsabove <- inspectDT(head(sort(rules_GPAabove, by="lift"),30)) # three metrics (most commonly)
#support:%of transactions that contain all of the items in an itemsets, the higher values the more frequently the itemset occurs
# rules with high support are preferred since more likely to be applicable to a large number of future transactions
#confidence:probability that lhs of the rule also contain rhs
#lift: strengh of association
#coverage: lhs support
resultsabove

#save as html
htmlwidgets::saveWidget(resultsabove, "arulesabove.html", selfcontained = FALSE)
browseURL("arules.html")

rules_GPAbelow <- apriori(DEGGPAFTIC_1st_term_associationDF,parameter = list(minlen=1, maxlen=6, conf=.1, support=0.05), # to see whether there is sufficient evidence to suggest
                     appearance = list(rhs=c("APRGPA1stFall=Below2.77"), default = "lhs"))

resultsGPAbelow <- inspectDT(head(sort(rules_GPAbelow, by="lift"),29))
resultsGPAbelow

#save as html
htmlwidgets::saveWidget(resultsGPAbelow, "arulesbelow.html", selfcontained = FALSE)
browseURL("arulesbelow.html")

###plots
plot(rules_GPAbelow, method="graph", engine = "interactive",shading = "lift") #grouped



## find reduncdant
sum(is.redundant(rules_02))
inspect(rules_01[is.redundant(rules_02)])
newM <- DATAFRAME(rules_02)
newM[newM$RHS == "{averageGPA1stFall=Below2.7}" & grepl(x=newM$LHS, pattern = "\\{Non-Florida\\}|\\{White}"),]#??

rerules_02 <- rules_02[!is.redundant(rules_02)]


##plots
require(arulesViz)
plot(x=rules_GPAbelow,
     measure = c("confidence","lift"), shading = "support")

plot(x=rules_GPAabove,
     method = "scatterplot", main="Scatter plot for above GPA Rules")

plot(x=rules_GPAabove,
     method = "two-key plot") # higher order give higher confidence but support

plot(x=rules_GPAbelow,
     method = "matrix")

plot(x=rerules_02,
     method = "matrix3D")

plot(x=rules_GPAbelow,
     method = "graph",
     engine = "htmlwidget")

#crossTable
ct <- crossTable(DEGGPAFTIC_1st_term_associationDF,  sort=TRUE)
ct
sp <- crossTable(DEGGPAFTIC_1st_term_associationDF,measure="support", sort=TRUE)
sp
lift <- crossTable(DEGGPAFTIC_1st_term_associationDF, measure="lift", sort=TRUE)
lift[1:5,1:5]
chi2 <- crossTable(DEGGPAFTIC_1st_term_associationDF, measure="chiSquared", sort=TRUE)
round(chi2[1:5,1:5], digits = 5)


#################################################################################
#### Association Rule for Inexperienced FTIC using CSEdata ( for APR GPA)  ######
#################################################################################




library(readr)
CSEdata <- read_csv("~/Data_IR/CSEdata.csv", 
                    col_types = cols(UNIV_ROW_ID = col_character()))
View(CSEdata)
glimpse(CSEdata)
apply(CSEdata, 2, p)

outliersAPR <- CSEdata$GPA_ENTERING_SECOND_FALL
outL <- boxplot.stats(CSEdata$GPA_ENTERING_SECOND_FALL)$out 
outL# 0.990000 2.525735 3.106896 3.555719 4.000000
min(outL, na.rm=TRUE) #0
max(outL, na.rm = TRUE) #0.98
glimpse(CSEdata)
### Filter not outliers

CSEdataV1 <- CSEdata %>% 
  filter(GPA_ENTERING_SECOND_FALL >= 0.99, na.rm=TRUE )  %>% # remove outliers
  mutate(AverageGPAindex = ifelse(GPA_ENTERING_SECOND_FALL< 3.106896, "Below3.10","Above3.10")) %>% 
  mutate(ExpFTIC=ifelse(HOURS_BROUGHT_TO_UNIVERSITY<= 0,"InexpFTIC","ExpFTIC"))
apply(CSEdataV1, 2, p)   # no NA for GPA

FTIC_GPADEG_com <- CSEdataV1 %>% 
  group_by(COHORT_YEAR,ExpFTIC,AverageGPAindex) %>% 
  summarise(meanPriorHrs=mean(HOURS_BROUGHT_TO_UNIVERSITY),CountFTIC=n())  #(meanGPA = mean(GPA1stFall),
FTIC_GPADEG_com



#################################
######## select variables #######
#################################

CSEdataV2 <-  CSEdataV1 %>% 
  #filter(ExpFTIC=="InexpFTIC") %>% 
  select(ENTRY_COLLEGE,
         ENTRY_DEPARTMENT,
         GENDER,
         ETHNICITY,
         COUNTY,
         ENTRY_PROGRAM,
         #COUNTY,
         FIRST_GENERATION_STUDENT,
         #ATHLETE,
         APPLICANT_TIER,
         HIGH_SCHOOL_NAME,
         GPA_HIGHSCHOOL,
         HOURS_BROUGHT_TO_UNIVERSITY,
         AP_CREDITS,
         ACT_PROPORTION,
         SAT_PROPORTION,
         #FIRST_FALL_PELL,FIRST_FALL_BRIGHT_FUTURES,FIRST_FALL_ANY_LOANS,
         #FIRST_FALL_NEED_BASED_LOANS,
         AverageGPAindex)

summary(CSEdataV2)
# need libraries
library(arules)
library(arulesViz)


#summary
CSEdataV3 <- as(CSEdataV2,"transactions") #transaction are analysed to identify rules of assocation
class(CSEdataV3)

summary(CSEdataV3)
#inspect(head(newGPAassociationDF)) # too long
itemFrequencyPlot(CSEdataV3, topN=10)
itemFrequencyPlot(CSEdataV3, support=0.05)

# results for above3.10
rules_above <- apriori(CSEdataV3,parameter = list(minlen=1, maxlen=6, conf=.5, support=0.1), # to see whether there is sufficient evidence to suggest
                       appearance = list(rhs=c("AverageGPAindex=Above3.10"), default = "lhs"))
results_above <- inspect(head(sort(rules_above, by="lift"),10))
results_above


# results for below3.10
rules_below <- apriori(CSEdataV3,parameter = list(minlen=1, maxlen=6, conf=.5, support=0.2), # to see whether there is sufficient evidence to suggest
                       appearance = list(rhs=c("AverageGPAindex=Below3.10"), default = "lhs"))
results_below <- inspect(head(sort(rules_below, by="lift"),10))
results_below


#Plots
library("RColorBrewer")
plot(rules_below, control = list(brewer.pal(10,"Spectral")), main="Rules for Below average")# showing high lift(support/confidence) for rules

require(arulesViz)
plot(x=rules_below,
     measure = c("confidence","lift"), shading = "support")

