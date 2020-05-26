## Project :
## To predict the whether the customer will fall under default or not.

getwd()
setwd("E:/ExcelR/project file/Team 1")

# install.packages("readr")
library(readr)

# Importing the training dataset

loan <- read.csv("E:/ExcelR/project file/Team 1/bank_final.csv",na.strings = c("", "NA")) # Choose the bank_final dataset

View(loan)
names(loan)

summary(loan)
str(loan)

sum(is.na(loan))
sum(is.na(loan))/prod(dim(loan)) * 100 # Finding NA's Percentage in whole data frame  


# Barplots

barplot(table(loan$State),main="State",col="Red")

barplot(table(loan$BankState),main="Bank State",col="Red")

barplot(table(loan$City),main="City",col="Red")

barplot(table(loan$ApprovalFY),main="Fiscal Year of Commitment",col="Red")

barplot(table(loan$Bank),main="Bank Name",col="Red")

barplot(table(loan$Term),main="loan Data",col="Red")

barplot(table(loan$NoEmp),main="NoEmp",col="Red")

barplot(table(loan$NewExist),main="New Exist Business",col="Red")

barplot(table(loan$CreateJob),main="CreateJob",col="Red")

barplot(table(loan$RetainedJob),main="RetainedJob",col="Red")

barplot(table(loan$FranchiseCode),main="FranchiseCode",col="Red")

barplot(table(loan$UrbanRural),main="Urban & Rural",col="Red")

barplot(table(loan$RevLineCr),main=" Revolving Line of Credit",col="Red")

barplot(table(loan$LowDoc),main="Low Document",col="Red")

barplot(table(loan$DisbursementGross),main="loan Data",col="Red")

barplot(table(loan$BalanceGross),main="loan Data",col="Red")

barplot(table(loan$MIS_Status),main="MIS_Status",col="Red")

barplot(table(loan$ChgOffPrinGr),main="loan Data",col="Red")

barplot(table(loan$GrAppv),main="loan Data",col="Red")

barplot(table(loan$SBA_Appv),main="loan Data",col="Red")


loan[,'UrbanRural']<-factor(loan[,'UrbanRural'])
prop.table(table(loan$UrbanRural))* 100


#install.packages("psych")
library(psych)
describe(loan)

loan$NewExist[loan$NewExist==0] <- 1
loan[,'NewExist']<-factor(loan[,'NewExist'])
prop.table(table(loan$NewExist))* 100

#install.packages("plyr")
library(plyr)

loan$FranchiseCode[loan$FranchiseCode >1] <- 2
loan[,'FranchiseCode']<-factor(loan[,'FranchiseCode'])
loan$FranchiseCode <- as.factor(revalue(loan$FranchiseCode,c("0"=1,"1"=0,"2"=0))) # Franchise code = 0 and No Franchise = 1
summary(loan$FranchiseCode)
prop.table(table(loan$FranchiseCode))* 100


loan$LowDoc <- as.factor(revalue(loan$LowDoc,c("1"=0, "C"=0,"N"=0,"Y"=1))) # LowDoc = 1 and No lowDoc = 0
summary(loan$LowDoc)
prop.table(table(loan$LowDoc))



loan$RevLineCr <- as.factor(revalue(loan$RevLineCr,c(","=0, "`"=0,"1"=0,"N"=0,"T"=0,"Y"=1))) # RevLineCr = yes = 1 and RevLineCr = No = 0
loan$RevLineCr[is.na(loan$RevLineCr)] <- 0
summary(loan$RevLineCr)
prop.table(table(loan$RevLineCr))*100


loan$MIS_Status <- as.factor(revalue(loan$MIS_Status,c("P I F"=0, "CHGOFF"=1))) # P I F = 0 and CHGOFF = 1
loan$MIS_Status[is.na(loan$MIS_Status)] <- 0
summary(loan$MIS_Status)
prop.table(table(loan$MIS_Status))*100
plot(MIS_Status)

str(loan)
# Converting data into Numeric

# install.packages("tidyr")
library(tidyr)

loan$GrAppv<-extract_numeric(loan$GrAppv)
loan$DisbursementGross <- extract_numeric(loan$DisbursementGross)
loan$BalanceGross <- extract_numeric(loan$BalanceGross)
loan$ChgOffPrinGr <- extract_numeric(loan$ChgOffPrinGr)
loan$SBA_Appv <- extract_numeric(loan$SBA_Appv)
loan$Term <- extract_numeric(loan$Term)
loan$NoEmp <- extract_numeric(loan$NoEmp)
loan$NewExist <- extract_numeric(loan$NewExist)
loan$CreateJob <- extract_numeric(loan$CreateJob)
loan$RetainedJob <- extract_numeric(loan$RetainedJob)
loan$FranchiseCode <- extract_numeric(loan$FranchiseCode)
loan$UrbanRural <- extract_numeric(loan$UrbanRural)
loan$RevLineCr <- extract_numeric(loan$RevLineCr)
loan$LowDoc <- extract_numeric(loan$LowDoc)
loan$ApprovalFY <- extract_numeric(loan$ApprovalFY)

# Weight of Evidence and Information Value

# install.packages("Information")
library(Information)

loan$MIS_Status<-extract_numeric(loan$MIS_Status)

IV <- create_infotables(data=loan, y="MIS_Status", bins=10, parallel=FALSE)

?create_infotables

IV_Value = data.frame(IV$Summary)

print(IV$Tables, row.names=FALSE)

plot_infotables(IV, "SBA_Appv")

plot_infotables(IV, IV$Summary$Variable[1:20], same_scale=FALSE)



# Information Value	   Variable Predictiveness
# Less than 0.02	     Not useful for prediction
# 0.02 to 0.1	         Weak predictive Power
# 0.1 to 0.3 	         Medium predictive Power
# 0.3 to 0.5	         Strong predictive Power
# >0.5	               Suspicious Predictive Power



# Again MIS Status changing numeric into factor for prediction

loan[,'MIS_Status']<-factor(loan[,'MIS_Status'])


names(loan)

# Removing columns on training dataset

loan <- subset(loan, select=-c(ï..Name,City,State,Bank,BankState,Zip,CCSC,ApprovalDate,ApprovalFY,ChgOffDate,DisbursementDate,BalanceGross,ChgOffPrinGr,Default))

View(loan)

summary(loan)

str(loan)
names(loan)
dim(loan)
sum(is.na(loan))

attach(loan)

# Boxplots 

boxplot(loan$Term,horizontal = TRUE)
boxplot(loan$NoEmp,horizontal = TRUE)
boxplot(loan$CreateJob,horizontal = TRUE)
boxplot(loan$RetainedJob,horizontal = TRUE)
boxplot(loan$DisbursementGross,horizontal = TRUE)
boxplot(loan$GrAppv,horizontal = TRUE)
boxplot(loan$SBA_Appv,horizontal = TRUE)

# Histograms 

hist(Term, 
     main="Loan term in months", 
     xlab="Term", 
     col="red")

hist(NoEmp, 
     main="Number of Business Employees", 
     xlab="NoEmp",
     col="blue")


hist(CreateJob, 
     main="Number of jobs created", 
     xlab="CreateJob",
     col="Red")

hist(RetainedJob, 
     main="Number of jobs retained", 
     xlab="RetainedJob",
     col="Red")

hist(DisbursementGross, 
     main="DisbursementGross($)", 
     xlab="DisbursementGross",
     col="Red")


hist(GrAppv,
     main=" Gross Amount of Loan Approved by Bank ($)", 
     xlab="Gross Approved",
     col="Red")

hist(SBA_Appv, 
     main=" Small Business Administration's Guaranteed Amount of Approved Loan ($)", 
     xlab="SBA_Approved",
     col="Red")

# Barplots

barplot(table(loan$Term),main="loan Data",col="Red")

barplot(table(loan$NoEmp),main="loan Data",col="Red")

barplot(table(loan$NewExist),main="loan Data",col="Red")

barplot(table(loan$CreateJob),main="loan Data",col="Red")

barplot(table(loan$RetainedJob),main="loan Data",col="Red")

barplot(table(loan$FranchiseCode),main="loan Data",col="Red")

barplot(table(loan$UrbanRural),main="loan Data",col="Red")

barplot(table(loan$RevLineCr),main="loan Data",col="Red")

barplot(table(loan$LowDoc),main="loan Data",col="Red")

barplot(table(loan$DisbursementGross),main="loan Data",col="Red")

#barplot(table(loan$BalanceGross),main="loan Data",col="Red")

barplot(table(loan$MIS_Status),main="loan Data",col="Red")

#barplot(table(loan$ChgOffPrinGr),main="loan Data",col="Red")

barplot(table(loan$GrAppv),main="loan Data",col="Red")

barplot(table(loan$SBA_Appv),main="loan Data",col="Red")

# Checking the Dataset distribution 

qqnorm(loan$Term)
qqline(loan$Term)

qqnorm(loan$NoEmp)
qqline(loan$NoEmp)

qqnorm(loan$CreateJob)
qqline(loan$CreateJob)

qqnorm(loan$RetainedJob)
qqline(loan$RetainedJob)

qqnorm(loan$DisbursementGross)
qqline(loan$DisbursementGross)


qqnorm(loan$GrAppv)
qqline(loan$GrAppv)

qqnorm(loan$SBA_Appv)
qqline(loan$SBA_Appv)


library(caTools)
# create normalization function
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}
# normalize the data to get rid of outliers if present in the data set
#loan <- as.data.frame(lapply(loan, normalize))



#summary(loan)

# Spliting Dataset into train and test  

set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(loan,SplitRatio = 0.70) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train = subset(loan,sample == TRUE) # dividing data set into 70 % to creates a training dataset named train with rows which are marked as TRUE
test = subset(loan, sample== FALSE) # dividing data set into 30 %. 



# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(train$MIS_Status~.,data=train,family = "binomial"(link="logit"))

summary(model)


# To calculate the odds ratio manually we are going to take exp of coef(model)
exp(coef(model))
# predicting with test data
# Confusion matrix table 
prob <- predict(model,test,type="response")
prob


library(InformationValue)
optCutOff <- optimalCutoff(test$MIS_Status, prob)[1] 
# We are going to use NULL and Residual Deviance to compare the between different models

# Misclassification Error
misClassError(test$MIS_Status, prob, threshold = optCutOff)

# ROC
plotROC(test$MIS_Status, prob)

Concordance(test$MIS_Status, prob)

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,test$MIS_Status)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 83.13 %

plotROC(test$MIS_Status, prob)

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
pred_MIS_Status <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
pred_MIS_Status <- ifelse(prob>=0.5,"CHGOFF","P I F")

# Creating new column to store the above values
test[,"prob"] <- prob
test[,"pred_values"] <- pred_values
test[,"pred_MIS_Status"] <- pred_MIS_Status


str(test)
# Confusion Matrix 
library(caret)
confusionMatrix(factor(test$pred_values),factor(test$MIS_Status))

# Confusion Matrix and Statistics
# 
#             Reference
# Prediction     0     1
#           0 31554  5145
#           1  2637  6818
# 
# Accuracy : 0.8314          
# 95% CI : (0.8279, 0.8348)
# No Information Rate : 0.7408          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.5288          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.9229          
#             Specificity : 0.5699          
#          Pos Pred Value : 0.8598          
#          Neg Pred Value : 0.7211          
#              Prevalence : 0.7408          
#          Detection Rate : 0.6837          
#    Detection Prevalence : 0.7951          
#       Balanced Accuracy : 0.7464          
#                                           
#        'Positive' Class : 0               
                            
# Variable importance
varImp(model)



table(test$MIS_Status,test$pred_values)

View(test)
prop.table(table(test$pred_MIS_Status))* 100

write.csv(test,'test_final.csv')

# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
#install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,test$MIS_Status)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)


# Predicting with train data
# Confusion matrix table 
prob <- predict(model,train,type="response")
prob
library(InformationValue)
optCutOff <- optimalCutoff(train$MIS_Status, prob)[1] 
# We are going to use NULL and Residual Deviance to compare the between different models

# Misclassification Error
misClassError(train$MIS_Status, prob, threshold = optCutOff)

# ROC
plotROC(train$MIS_Status, prob)

Concordance(train$MIS_Status, prob)

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,train$MIS_Status)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 83.32 %

plotROC(train$MIS_Status, prob)

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
pred_MIS_Status <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
pred_MIS_Status <- ifelse(prob>=0.5,"CHGOFF","P I F")

# Creating new column to store the above values
train[,"prob"] <- prob
train[,"pred_values"] <- pred_values
train[,"pred_MIS_Status"] <- pred_MIS_Status

# Confusion Matrix

confusionMatrix(factor(train$pred_values),factor(train$MIS_Status))

View(train)



loan <- rbind(test, train) # Combining training dataset and testing dataset

barplot(table(loan$pred_MIS_Status),main="Predicted MIS_Status",col="Red")
summary(loan$pred_MIS_Status)
prop.table(table(loan$pred_MIS_Status))*100 


View(loan)


names(loan)


write.csv(test,'test_final.csv')
write.csv(loan,'loan_final.csv')

save(test , file = 'test_final.rda')


