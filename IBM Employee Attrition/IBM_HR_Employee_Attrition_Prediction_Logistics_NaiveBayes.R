library(stats)
library(caTools)
library(Amelia)
library(dplyr)

setwd("~/National College of Ireland/Machine Learning Project/Final Datasets/DMML_CA1/IBM Churn Analysis")

IBMDF <- read.csv(file="IBM_HR_Data_new.csv")

head(IBMDF)
str(IBMDF)
summary(IBMDF)

#Print the structure of the dataframe
print(str(IBMDF))



#Checking if there are any null values
sapply(IBMDF,function(x) sum(is.na(x)))


IBMDF_new <- na.omit(IBMDF)


sapply(IBMDF_new,function(x) sum(is.na(x)))

summary(IBMDF_new) #No NULL values here!

#Missing Values vs Observed Values
missmap(IBMDF_new)

# Check for any correlated variables
library(corrplot)

cor <- cor(IBMDF_new[sapply(IBMDF_new,is.numeric)])
corrplot(cor)
cor(cor)

IBMDF_new$DailyRate <- NULL  
IBMDF_new$Department <- NULL
IBMDF_new$Education <- NULL
IBMDF_new$EducationField <- NULL
IBMDF_new$EducationCount <- NULL
IBMDF_new$Department <- NULL
IBMDF_new$EmployeeCount <- NULL
IBMDF_new$EmployeeNumber <- NULL
IBMDF_new$StandardHours <- NULL
IBMDF_new$Over18 <- NULL
IBMDF_new$Application.ID <- NULL
IBMDF_new$Employee.Source <- NULL
IBMDF_new$NumCompaniesWorked <- NULL
IBMDF_new$RelationshipSatisfaction <- NULL
IBMDF_new$TrainingTimesLastYear <- NULL
IBMDF_new$PerformanceRating <- NULL
IBMDF_new$PerformanceHike <- NULL
IBMDF_new$JobRole <- NULL
IBMDF_new$JobSatisfaction <- NULL
IBMDF_new$MaritalStatus <- NULL
IBMDF_new$EnvironmentSatisfaction <- NULL

IBMDF_new$YearsWithCurrManager  <- NULL
IBMDF_new$Employee.Source <- NULL

str(IBMDF_new)

summary(IBMDF_new)

install.packages("ggplot2")

library(ggplot2)
#Business Travel against Attrition
ggplot(IBMDF_new, 
       aes(x= BusinessTravel,  group=Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill="Business Travel") +
  facet_grid(~Attrition) +
  scale_y_continuous() + 
  scale_fill_manual(values = c("purple", "blue","red","purple")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition") +
  labs(x = "Business Travel")

unique(IBMDF_new$OverTime )

#OverTime Against Attrition

ggplot(IBMDF_new, 
       aes(x = OverTime, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill= "OverTime") +
  facet_grid(~Attrition) +
  scale_fill_manual(values = c("Blue","red","orange","blue")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Attrition")

par(mfrow=c(1,3))
hist(IBMDF_new$Age, main = "Age", xlab = "Age", ylab = "Count", col = "black", )
hist(IBMDF_new$TotalWorkingYears, main = "Total Working Years", xlab = "Total Working Years", ylab = "Count", col = "black")
hist(IBMDF_new$YearsAtCompany, main = "Years AT Company", xlab = "Years At Company", ylab = "Count", col = "black")



#Need to remove Outliers
par(mfrow=c(3,3))

boxplot(IBMDF_new$Age,xlab = "Age")
boxplot(IBMDF_new$JobInvolvement,xlab = "Job Involvement")
#boxplot(IBMDF_new$JobLevel,ylab = "Job Level")
boxplot(IBMDF_new$StockOptionLevel, xlab = "StockOptionLevel")
boxplot(IBMDF_new$TotalWorkingYears, xlab = "Total Working Years")

boxplot(IBMDF_new$YearsAtCompany, xlab = "Years At Company")

boxplot(IBMDF_new$Gender, xlab = "Gender")

boxplot(IBMDF_new$JobInvolvement, xlab = "Job Involvement")
boxplot(IBMDF_new$JobLevel, xlab = "Job Level")
boxplot(IBMDF_new$WorkLifeBalance, xlab = "WorkLifeBalance")
boxplot(IBMDF_new$YearsAtCompany, xlab = "Years At Company")


#Apply COnditions to Remove Outliers
library(dplyr)
IBMDF_new = subset(IBMDF_new, Age<60 & YearsAtCompany<19 & JobInvolvement<50 & StockOptionLevel<2.5 & TotalWorkingYears<18 & YearsAtCompany<14 & JobLevel<3)
str(IBMDF_new)


#Check to confirm if outliers are removed
boxplot(IBMDF_new$Age,ylab = "Age")
boxplot(IBMDF_new$JobInvolvement,ylab = "Job Involvement")
boxplot(IBMDF_new$JobLevel,ylab = "Job Level")
boxplot(IBMDF_new$StockOptionLevel, ylab = "StockOptionLevel")
boxplot(IBMDF_new$TotalWorkingYears, ylab = "Total Working Years")
boxplot(IBMDF_new$Gender, ylab = "Gender")

boxplot(IBMDF_new$WorkLifeBalance, ylab = "WorkLifeBalance")
boxplot(IBMDF_new$YearsAtCompany, ylab = "Years At Company")


#Creating Dummy Variable for Attrition Column
IBMDF_new$Attrition <- ifelse(IBMDF_new$Attrition == "Voluntary Resignation",1,0)

head(IBMDF_new)

#Transformation of Attrition
IBMDF_new$Attrition <- as.factor(IBMDF_new$Attrition)

library(caTools)

set.seed(1000)
split = sample.split(IBMDF_new$Attrition, SplitRatio = 0.8)
training_set = subset(IBMDF_new, split == TRUE)
test_set = subset(IBMDF_new, split == FALSE)
View(training_set)

View(test_set)

#Final Model - Logistic Regression

#mod<-glm(Attrition ~.,data = training_set,family = "binomial")
#summary(mod)

str(IBMDF_new)
mod1<-glm(Attrition ~ Age + Gender + JobInvolvement + HourlyRate + JobLevel + StockOptionLevel + TotalWorkingYears + YearsAtCompany + BusinessTravel + WorkLifeBalance,data = training_set,family = "binomial")
summary(mod1)


data_pred <- predict(mod1, type="response",newdata = test_set)
summary(data_pred)

#Plotting ROC Curve
install.packages(("ROCR"))
library(ROCR)
predt <- prediction(data_pred, test_set$Attrition)
ROCperflr <- performance(predt,"tpr","fpr")
plot(ROCperflr, colorsize = TRUE, print.cutoffs.at=seq(0,1,by=0.1))



data_pred <- ifelse(data_pred>=0.5,"1","0")
test_set$Attrition <- as.factor(test_set$Attrition)
install.packages("caret")
library(caret)

#Confusion Matrix
conf_mat <- confusionMatrix(as.factor(data_pred), test_set$Attrition, positive = "1")
conf_mat

#Getting Accuracy

accuracy <- conf_mat$overall[1]
model <- "Logistic Regression"

Accuracy_table <- data.frame(model,accuracy)
Accuracy_table


#------------------------------------------------------------------------------

#Naive Bayes Model

library(caret)
library(irr)
library(rpart)
library(tidyverse)
library(e1071)

#setwd("~/National College of Ireland/Machine Learning Project/Final Datasets/Jupyter FIles/IBM Churn Analysis")

IBMDF1 <- read.csv(file="IBM_HR_Data_new.csv")

head(IBMDF1)

#Print the structure of the dataframe
print(str(IBMDF1))

#Checking if there are any null values
sapply(IBMDF1,function(x) sum(is.na(x)))

IBMDF1_new <- na.omit(IBMDF1)

sapply(IBMDF1_new,function(x) sum(is.na(x)))

summary(IBMDF1_new)


IBMDF1_new$DailyRate <- NULL  
IBMDF1_new$Department <- NULL
IBMDF1_new$Education <- NULL
IBMDF1_new$EducationField <- NULL
IBMDF1_new$EducationCount <- NULL
IBMDF1_new$Department <- NULL
IBMDF1_new$EmployeeCount <- NULL
IBMDF1_new$EmployeeNumber <- NULL
IBMDF1_new$StandardHours <- NULL
IBMDF1_new$Over18 <- NULL
IBMDF1_new$Application.ID <- NULL
IBMDF1_new$Employee.Source <- NULL
IBMDF1_new$NumCompaniesWorked <- NULL
IBMDF1_new$RelationshipSatisfaction <- NULL
IBMDF1_new$TrainingTimesLastYear <- NULL
IBMDF1_new$PerformanceRating <- NULL
IBMDF1_new$PerformanceHike <- NULL
IBMDF1_new$JobRole <- NULL
IBMDF1_new$JobSatisfaction <- NULL
IBMDF1_new$MaritalStatus <- NULL
IBMDF1_new$EnvironmentSatisfaction <- NULL

IBMDF1_new$YearsWithCurrManager  <- NULL
IBMDF1_new$Employee.Source <- NULL

summary(IBMDF1_new)


#Need to remove Outliers

boxplot(IBMDF1_new$Age,ylab = "Age")
boxplot(IBMDF1_new$JobInvolvement,ylab = "Job Involvement")
boxplot(IBMDF1_new$JobLevel,ylab = "Job Level")
boxplot(IBMDF1_new$StockOptionLevel, ylab = "StockOptionLevel")
boxplot(IBMDF1_new$TotalWorkingYears, ylab = "Total Working Years")

boxplot(IBMDF1_new$YearsAtCompany, ylab = "Years At Company")

boxplot(IBMDF1_new$Gender, ylab = "Gender")

boxplot(IBMDF1_new$JobInvolvement, ylab = "Job Involvement")
boxplot(IBMDF1_new$JobLevel, ylab = "Job Level")
boxplot(IBMDF1_new$WorkLifeBalance, ylab = "WorkLifeBalance")
boxplot(IBMDF1_new$YearsAtCompany, ylab = "Years At Company")


#Apply COnditions to Remove Outliers
library(dplyr)
IBMDF1_new = subset(IBMDF1_new, as.numeric(Age)<60 & as.numeric(YearsAtCompany)<19 & as.numeric(JobInvolvement)<50 & as.numeric(StockOptionLevel)<2.5 & as.numeric(TotalWorkingYears)<18 & as.numeric(YearsAtCompany)<14 & as.numeric(JobLevel)<3)
str(IBMDF1_new)


#Check to confirm if outliers are removed
boxplot(IBMDF1_new$Age,ylab = "Age")
boxplot(IBMDF1_new$JobInvolvement,ylab = "Job Involvement")
boxplot(IBMDF1_new$JobLevel,ylab = "Job Level")
boxplot(IBMDF1_new$StockOptionLevel, ylab = "StockOptionLevel")
boxplot(IBMDF1_new$TotalWorkingYears, ylab = "Total Working Years")
boxplot(IBMDF1_new$Gender, ylab = "Gender")
boxplot(IBMDF1_new$WorkLifeBalance, ylab = "WorkLifeBalance")
boxplot(IBMDF1_new$YearsAtCompany, ylab = "Years At Company")

IBMDF1_new$Age <- as.factor(IBMDF1_new$Age)
IBMDF1_new$BusinessTravel <- as.factor(IBMDF1_new$BusinessTravel)

IBMDF1_new$Gender<- as.factor(IBMDF1_new$Gender)

IBMDF1_new$HourlyRate <- as.factor(IBMDF1_new$HourlyRate)

IBMDF1_new$JobLevel <- as.factor(IBMDF1_new$JobLevel)

IBMDF1_new$StockOptionLevel <- as.factor(IBMDF1_new$StockOptionLevel)
IBMDF1_new$TotalWorkingYears <- as.factor(IBMDF1_new$TotalWorkingYears)

IBMDF1_new$WorkLifeBalance <- as.factor(IBMDF1_new$WorkLifeBalance)
IBMDF1_new$YearsAtCompany <- as.factor(IBMDF1_new$YearsAtCompany)


missmap(IBMDF_new)

IBMDF1_new$Attrition <- ifelse(IBMDF1_new$Attrition == "Voluntary Resignation",1,0)


IBMDF1_new$Attrition <- as.factor(IBMDF1_new$Attrition)

str(IBMDF1_new)

# Partition of Data

library(caTools)

set.seed(1000)
split = sample.split(IBMDF1_new$Attrition, SplitRatio = 0.8)
training_set = subset(IBMDF1_new, split == TRUE)
test_set = subset(IBMDF1_new, split == FALSE)
View(training_set)
View(test_set)



nbmodel<-naiveBayes(Attrition~ Age + Gender + HourlyRate + MonthlyIncome + JobLevel + JobInvolvement + StockOptionLevel + TotalWorkingYears + YearsAtCompany + BusinessTravel + WorkLifeBalance,data = training_set)
nbmodel

#Prediction of Output Variable
pred_nb <- as.numeric(pred_nb)
pred_nb <- ifelse(pred_nb>=0.5,"1","0")
pred_nb <- as.numeric(pred_nb)
pred_nb <- predict(nbmodel, newdata = test_set)
class(pred_nb)


test_set$Attrition

#Plotting ROC Curve
#install.packages(("ROCR"))
library(ROCR)
pred_nb <- as.numeric(pred_nb)
predt <- prediction(pred_nb,test_set$Attrition)
ROCperflr <- performance(predt,measure="tpr",x.measure = "fpr")
plot(ROCperflr, colorsize = TRUE, print.cutoffs.at=seq(0,1,by=0.2),text.adj=c(-0.2,1.7))

table(test_set$Attrition,pred_nb)

#Confusion Matrix
library(caret)
pred_nb <- as.factor(pred_nb)
levels(pred_nb) <- list("0" = "1", "1" = "2")
str(pred_nb)
conf_mat <- confusionMatrix(as.factor(pred_nb),as.factor(test_set$Attrition))
conf_mat
model <- "Naive Bayes Model"
accuracy <- conf_mat$overall[1]

accuracy_table <- data.frame(model,accuracy)

accuracy_table

