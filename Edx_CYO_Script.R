##########################################################################################################################
# HarvardX PH125.9x Data Science Capstone Movielens Project
#
# Student: Sharmin Shabnam
#
##########################################################################################################################

##########################################################################################################################
# The following packages are required. Please set your default repository to CRAN prior to installing.
##########################################################################################################################
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dslabs)) install.packages("dslabs")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven")
if(!require(pROC)) install.packages("haven")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(knitr)) install.packages("knitr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(e1071)) install.packages('e1071', dependencies=TRUE)
if(!require(randomForest)) install.packages('randomForest', dependencies=TRUE)
if(!require(summarytools)) install.packages('summarytools', dependencies=TRUE)
if(!require(Hmisc)) install.packages('Hmisc', dependencies=TRUE)


library(tidyverse)
library(caret)
library(ggplot2)
library(gridExtra)
library(dslabs)
library(haven)
library(pROC)
library(rpart.plot)
library(ROCR)
library(knitr)
library(kableExtra)
library(e1071)
library(randomForest)
library(summarytools)
library(Hmisc)

##########################################################################################################################
# Download data set, create descriptive statistics
##########################################################################################################################


#Load data file from github repository
namcs2014 <- read.csv("https://raw.githubusercontent.com/
shabnam-shbd/Data_Science_Capstone_Project_CYO/
master/namcs2014-stata.csv", header=TRUE, sep=",")


#Select variables from the data set and sort according 
#to their types (factors and numeric variables)
selected_vars <- c("AGE", "AGER", "SEX", "RACER","PAYTYPER", "BMI", "USETOBAC",
                   "SUBSTAB", "ETOHAB","ALZHD", "ARTHRTIS","ASTHMA","DEPRN",
                   "CANCER", "CEBVD", "CKD","COPD","CHF","CAD","DIABTYP1",
                   "DIABTYP2","ESRD","HPE","HIV", "HYPLIPID","HTN","OSA",
                   "OSTPRSIS", "NUMMED", "REGIONOFF")

#Create dataframe "namcs_table1" for descriptive table only
namcs_table1 <- namcs2014 %>%
  select(selected_vars) %>% 
  #Convert BMI to numeric variable 
  mutate(BMI = as.numeric(BMI)) %>%
  #Transform BMI from numeric to categorical variable
  mutate(BMI=cut(BMI, breaks=c(-10,0,18.5,25,81.2),
                 labels=c("Unknown or Missing","Underweight",
                          "Normal weight","Overweight or Obese"),
                 right=FALSE)) %>% 
  #Transform PAYTYPER into a categorical variable
  mutate(PAYTYPER=cut(PAYTYPER, breaks=c(-10,1,2,3,4,8),
                      labels=c("Unknown or missing",
                               "Private insurance",
                               "Medicare","Medicaid",
                               "Other"),
                      right=FALSE)) %>%
  #Transform USETOBAC into a categorical variable
  mutate(USETOBAC=cut(USETOBAC, breaks=c(-10,1,2,3,4),
                      labels=c("Unknown or missing",
                               "Never","Former","Current"),
                      right=FALSE))
#Sort variables according to their types (factor,
#categorical and numeric variables)
factor_vars <- c("AGER","SEX", "RACER","PAYTYPER", "BMI", "USETOBAC",
                 "SUBSTAB", "ETOHAB","ALZHD","ARTHRTIS","ASTHMA",
                 "DEPRN","CANCER", "CEBVD", "CKD","COPD","CHF","CAD",
                 "DIABTYP1","DIABTYP2","ESRD","HPE","HIV", "HYPLIPID",
                 "HTN","OSA","OSTPRSIS", "REGIONOFF")

namcs_table1[factor_vars] <- lapply(namcs_table1[factor_vars], as.factor)

yesno_vars <- c("ETOHAB","ALZHD","ARTHRTIS","ASTHMA","DEPRN","CANCER",
                "CEBVD", "CKD","COPD","CHF","CAD","DIABTYP1","DIABTYP2",
                "ESRD","HPE","HIV","HYPLIPID","HTN","OSA","OSTPRSIS","SUBSTAB") 

namcs_table1[yesno_vars] <- lapply(namcs_table1[yesno_vars],factor, 
                                   levels = c(1, 0), 
                                   labels = c("Yes", "No"))

numeric_vars <- c("AGE","NUMMED")

namcs_table1[numeric_vars] <- sapply(namcs_table1[numeric_vars],as.numeric)

levels(namcs_table1$SEX) <- list("Female"="1", "Male"="2")

levels(namcs_table1$REGIONOFF) <- list("Northeast"="1", "Midwest"="2",
                                       "South"="3", "West"="4")

levels(namcs_table1$AGER) <- list("<15 years"="1", "15-24 years"="2", 
                                  "25-44 years"="3","45-64 years"="4",
                                  "65-74 years"="5","75 years and above"="6")

levels(namcs_table1$RACER) <- list("White"="1", "Black"="2", "Other"="3")

var_labels <- c(AGE = "Patient age (years)",
                AGER = "Patient age categories",
                SEX = "Sex",
                RACER = "Race",
                PAYTYPER = "Insurance type",
                BMI = "Body-mass index category",
                USETOBAC = "Tobacco use",
                SUBSTAB = "Substance abuse",
                ETOHAB = "Alcohol misuse, abuse or dependence",
                ALZHD = "Alzheimer\'s disease",
                ARTHRTIS = "Arthritis",
                ASTHMA = "Asthma",
                DEPRN = "Depression",
                CANCER = "Cancer",
                CEBVD = "Cerebrovascular disease",
                CKD = "Chronic kidney disease",
                COPD = "Chronic obstructive pulmonary disease",
                CHF = "Congestive heart failure",
                CAD = "Coronary artery disease",
                DIABTYP1 = "Diabetes mellitus Type 1",
                DIABTYP2 = "Diabetes mellitus Type 2",
                ESRD = "End-stage renal disease",
                HPE = "Pulmonary embolism or deep vein thrombosis",
                HIV = "HIV infection",
                HYPLIPID = "Hyperlipidemia",
                HTN = "Hypertension",
                OSA = "Obstructive sleep apnea",
                OSTPRSIS = "Osteoporosis",
                NUMMED = "Number of medications",
                REGIONOFF = "Geographic region in USA")

#Assign variable labels to each variable
label(namcs_table1) = lapply(names(namcs_table1),
                             function(x) var_labels[match(x, names(var_labels))])

#Table 1: Descriptive Statistics of Selected Variables in NAMCS 2014 Data Set. 
# To print nicely formatted table of descriptive statistics
print(dfSummary(namcs_table1, plain.ascii = F, style = "grid",
                subtitle.emphasis = T, varnumbers = F, labels.col = T,
                graph.col = F, headings = F, display.labels = F,
                valid.col = F, na.col = F, tmp.img.dir = "/tmp"))



##########################################################################################################################
# Data preparation for machine learning models
##########################################################################################################################

#Suppress warnings and set total digits to 5
options(warn=-1, digits=5)

#"namcs_clean" dataset is for ML models
#The following graphs show sex and race distribution of the depression status.
#Select and transform categorical variables from the data set 
namcs_clean <- namcs2014 %>%
  mutate(BMI = as.numeric(BMI)) %>%
  select(selected_vars) %>% 
  mutate(SEX = factor(ifelse(SEX == 1,1,0))) %>% 
  mutate(BMI=cut(BMI, breaks=c(-10,0,18.5,25,81.2), labels=c("1","2","3","4"), right=FALSE)) %>% 
  mutate(PAYTYPER=cut(PAYTYPER, breaks=c(-10,1,2,3,4,8), labels=c("1", "2", "3","4","5"), right=FALSE)) %>%
  mutate(USETOBAC=cut(USETOBAC, breaks=c(-10,1,2,3,4), labels=c("1", "2","3","4"), right=FALSE))

#Sex distribution of Depression
namcs_clean %>%
  mutate(SEX = factor(ifelse(SEX == 1,1,0), labels = c("Female", "Male"))) %>% 
  ggplot(aes(x = as.factor(DEPRN), fill= SEX)) + theme_light() +
  geom_bar() + 
  labs(title="Sex distribution of Depression", x="Depression", y="Count") +
  scale_x_discrete( breaks = c("0","1"),labels= c("Yes","No"))

#Race distribution of Depression
namcs_clean %>%
  mutate(RACER=cut(as.numeric(RACER), breaks=c(1,2,3,4),
                   labels=c("White", "Black","Other"), right=FALSE)) %>%
  ggplot(aes(x = as.factor(DEPRN), fill= RACER)) + theme_light() +
  geom_bar() + 
  labs(title="Race distribution of Depression", x="Depression", y="Count") +
  scale_x_discrete( breaks = c("0","1"),labels= c("Yes","No"))  

#Factorize variables 
factor_vars <- c( 'REGIONOFF',"AGER", 'RACER','PAYTYPER',"USETOBAC",'BMI',
                  "ETOHAB","ALZHD","ARTHRTIS","ASTHMA","DEPRN","CANCER",
                  "CEBVD", "CKD","COPD","CHF","CAD","DIABTYP1","DIABTYP2",
                  "ESRD","HPE","HIV","HYPLIPID","HTN","OSA","OSTPRSIS","SUBSTAB")
namcs_clean[factor_vars] <- lapply(namcs_clean[factor_vars], as.factor)


#Clean up memory
rm(namcs2014,namcs_table1,factor_vars,selected_vars)
invisible(gc())


##########################################################################################################################
# The following section is to create data partitions
##########################################################################################################################
#Create Data partition into test set and training set 80% and 20%
set.seed(1000, sample.kind="Rounding") #if using R 3.6 or later
test_index <- createDataPartition(namcs_clean$DEPRN,
                                  times = 1, p = 0.2, list = FALSE)

#Test and Training set for final analysis
test_set <- namcs_clean[test_index, ] %>%  select(-AGE)
train_set <- namcs_clean[-test_index, ] %>% select(-AGE)
dim(test_set)
dim(train_set)
table(test_set$DEPRN)
table(train_set$DEPRN)

#Clean up memory
rm(namcs,factor_vars,selected_vars)
invisible(gc())


##########################################################################################################################
# The following section fits Logistic Regression Model
##########################################################################################################################
#Training a logistic regression model with the caret glm method 
set.seed(1000, sample.kind = "Rounding")
fit_logistic_reg <- glm(DEPRN ~ .,
                        family=binomial(link="logit"), 
                        data=train_set)  

#Make  prediction in the test data
pred_logistic <- predict(fit_logistic_reg,
                         newdata = test_set, type = 'response')

#ROC graph and area under the curve
roc_lr <- roc(response = test_set$DEPRN, predictor = pred_logistic)
plot(roc_lr, print.auc=TRUE,
     lwd=2, xlim=c(1,0),
     col="steelblue",
     main="ROC Curve for Logistic Regression",)

#Extract AUC stats
auc_lr <- auc(roc_lr)
auc_lr

#Threshold using youden method
threshold_youden_lr <- coords(roc_lr, "best", 
                           ret = c("threshold", "sensitivity", "specificity"),
                           best.method = "youden", transpose = F)

#Selecting the threshold value as probability cut off
threshold_youden_lr$threshold
cutoff_lr <- threshold_youden_lr$threshold

#Confusion Matrix using threshold cut off value
pred_logistic <- ifelse(predict(fit_logistic_reg, 
                                newdata = test_set,
                                type = "response") > cutoff_lr, 1, 0)

confusionMatrix_lr <- confusionMatrix(data = as.factor(pred_logistic),
                                      reference = as.factor(test_set$DEPRN),
                                      positive = "1")
confusionMatrix_lr

#Save results in a dataframe
compare_results <-data.frame(ML_model = "Logistic Regression",
                      Accuracy = confusionMatrix_lr$overall['Accuracy'],
                      Sensitivity = confusionMatrix_lr$byClass['Sensitivity'],
                      Specificity = confusionMatrix_lr$byClass['Specificity'],
                      Balanced_accuracy = confusionMatrix_lr$byClass['Balanced Accuracy'],
                      AUC = auc_lr)


#Clean up memory
rm(fit_logistic_reg,pred_logistic,threshold_youden_lr,
   roc_lr,confusionMatrix_lr,cutoff_lr, auc_lr)
invisible(gc())

##########################################################################################################################
# The following section fits CART
##########################################################################################################################

#Classification tree model
#Tuned the complexity parameter with cp = seq(0, 0.05, 0.002).
set.seed(1000, sample.kind = 'Rounding')
fit_rpart <- rpart(as.factor(DEPRN) ~ .,data = train_set,
                   method = "class",
                   control = rpart.control(minsplit = 20,
                                           cp = 0.00001, 
                                          xval = 5,
                                          maxdepth = 30),
                   parms = list(split = "information"))

#ROC graph and area under the curve
y_hat_rpart <- predict(fit_rpart, newdata = test_set, type="prob")[,"1"]
head(y_hat_rpart)
roc_rpart <- roc(response = as.factor(test_set$DEPRN), predictor = y_hat_rpart)
plot(roc_rpart, print.auc=TRUE,
     lwd=2, xlim=c(1,0),
     col="steelblue",
     main="ROC Curve for CART")

#Extract AUC stats
auc_rpart <- auc(roc_rpart)
auc_rpart

# Threshold using youden method
threshold_youden_rpart <- coords(roc_rpart, "best", 
                                 ret = c("threshold", "sensitivity", "specificity"),
                                 best.method = "youden", transpose = F)

#Selecting the threshold value as probability cut off
threshold_youden_rpart$threshold 
cutoff_rpart <- threshold_youden_rpart$threshold
cutoff_rpart
#Confusion Matrix using threshold cut off value
pred_rpart <- ifelse(predict(fit_rpart, 
                             newdata = test_set,
                             type = "prob") > cutoff_rpart, 1, 0)[,"1"]
confusionMatrix_rpart <- confusionMatrix(data = as.factor(pred_rpart),
                                         reference = as.factor(test_set$DEPRN),
                                         positive = "1")
confusionMatrix_rpart

#Save results in a dataframe
compare_results <- compare_results %>%
  add_row(ML_model = "CART",
          Accuracy = confusionMatrix_rpart$overall['Accuracy'],
          Sensitivity = confusionMatrix_rpart$byClass['Sensitivity'],
          Specificity = confusionMatrix_rpart$byClass['Specificity'],
          Balanced_accuracy = confusionMatrix_rpart$byClass['Balanced Accuracy'],
          AUC = auc_rpart)

#Clean up memory
rm(fit_rpart,y_hat_rpart,pred_rpart,threshold_youden_rpart,
   roc_rpart,confusionMatrix_rpart,cutoff_rpart, auc_rpart)
invisible(gc())

##########################################################################################################################
# The following section fits Random Forest
##########################################################################################################################

#Random Forest model
set.seed(1000, sample.kind = 'Rounding')
fit_rf <- randomForest(as.factor(DEPRN) ~ .,
                       data = train_set,
                       #More trees such as >500 would likely result in 
                       #better prediction but would require more powerful
                       #computational resources
                       ntree = 500, mtry = 5, importance = TRUE)

str(train_set)
#Make prediciton
y_hat_rf <- predict(fit_rf, test_set, type='prob')[,"1"]
head(y_hat_rf)

#ROC graph and area under the curve
roc_rf <- roc(response = as.factor(test_set$DEPRN), predictor = y_hat_rf) 
plot(roc_rf, print.auc=TRUE,
     lwd=2, xlim=c(1,0),
     col="steelblue",
     main="ROC Curve for Random Forest")

#Extract AUC stats
auc_rf <- auc(roc_rf)
auc_rf

# Threshold using youden method
threshold_youden_rf <- coords(roc_rf, "best", 
                                 ret = c("threshold", "sensitivity", "specificity"),
                                 best.method = "youden", transpose = F)

#Selecting the threshold value as probability cut off
threshold_youden_rf$threshold
cutoff_rf <- threshold_youden_rf$threshold

#Confusion Matrix using predictions
pred_rf <- ifelse(predict(fit_rf, 
                             newdata = test_set,
                             type = "prob") > cutoff_rf, 1, 0)[,"1"]
confusionMatrix_rf <- confusionMatrix(data = as.factor(pred_rf),
                                      reference = as.factor(test_set$DEPRN),
                                      positive = "1")
confusionMatrix_rf

#Save results in a dataframe
compare_results <- compare_results %>% add_row(ML_model = "Random Forest",
                      Accuracy = confusionMatrix_rf$overall['Accuracy'],
                      Sensitivity = confusionMatrix_rf$byClass['Sensitivity'],
                      Specificity = confusionMatrix_rf$byClass['Specificity'],
                      Balanced_accuracy = confusionMatrix_rf$byClass['Balanced Accuracy'],
                      AUC = auc_rf)

#Clean up memory
rm(fit_rf,y_hat_rf,pred_rf,threshold_youden_rf,
   roc_rf,confusionMatrix_rf,cutoff_rf, auc_rf)
invisible(gc())




##########################################################################################################################
# The following section compares different ML models
##########################################################################################################################

compare_results
