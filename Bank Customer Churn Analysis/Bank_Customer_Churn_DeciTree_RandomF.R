library(stats)
#install.packages('caTools')
library(caTools)
library(Amelia)
library(dplyr)

#Setting working directory in R

setwd("~/National College of Ireland/Machine Learning Project/Final Datasets/DMML_CA1/Bank Customer Churn Analysis")

#Read the Telecom dataset input file

BCustDF <- read.csv(file="Bank_CUstomer_Churn_Data.csv")

head(BCustDF)

#Print the structure of the dataframe
print(str(BCustDF))

table(BCustDF$Exited)

#Checking if there are any null values
sapply(BCustDF,function(x) sum(is.na(x))) 

summary(BCustDF) #No NULL values here!

#install.packages("visdat")
library(visdat)
#Check Missing Values
vis_miss(BCustDF)

#Dropping unwanted columns
BCustDF$RowNumber <- NULL
BCustDF$Surname <- NULL
summary(BCustDF)
head(BCustDF)

#Need to remove Outliers

boxplot(BCustDF$CreditScore,ylab = "Credit Score")

boxplot(BCustDF$Age, ylab = "Age")
boxplot(BCustDF$Tenure, ylab = "Tenure")

boxplot(BCustDF$Balance, ylab = "Balance")

boxplot(BCustDF$NumOfProducts, ylab = "Number of Products")

boxplot(BCustDF$HasCrCard, ylab = "Has Credit Card")
boxplot(BCustDF$IsActiveMember, ylab = "Ia Active Member")
boxplot(BCustDF$EstimatedSalary, ylab = "Estimated Salary")


#Apply COnditions to Remove Outliers
library(dplyr)
BCustDF = subset(BCustDF, as.numeric(Age)<43 & as.numeric(NumOfProducts)<3.9)
str(BCustDF)


#Check to confirm if outliers are removed

boxplot(BCustDF$CreditScore,ylab = "Credit Score")

boxplot(BCustDF$Age, ylab = "Age")
boxplot(BCustDF$Tenure, ylab = "Tenure")

boxplot(BCustDF$Balance, ylab = "Balance")

boxplot(BCustDF$NumOfProducts, ylab = "Number of Products")

boxplot(BCustDF$HasCrCard, ylab = "Has Credit Card")
boxplot(BCustDF$IsActiveMember, ylab = "Ia Active Member")
boxplot(BCustDF$EstimatedSalary, ylab = "Estimated Salary")



#Converting features into factors
BCustDF$CustomerId <- as.numeric(BCustDF$CustomerId)

BCustDF$CreditScore <- as.numeric(BCustDF$CreditScore)
BCustDF$Location <- as.factor(BCustDF$Location)
BCustDF$Gender <- as.factor(BCustDF$Gender)
BCustDF$Age <- as.numeric(BCustDF$Age)
BCustDF$Tenure <- as.numeric(BCustDF$Tenure)
BCustDF$Balance <- as.numeric(BCustDF$Balance)
BCustDF$NumOfProducts <- as.numeric(BCustDF$NumOfProducts)
BCustDF$HasCrCard <- as.numeric(BCustDF$HasCrCard)
BCustDF$IsActiveMember<- as.numeric(BCustDF$IsActiveMember)

BCustDF$EstimatedSalary <- as.numeric(BCustDF$EstimatedSalary)
BCustDF$Exited <- as.factor(BCustDF$Exited)

#Checking dataframe structure again after conversion
str(BCustDF)

library(caTools)

#Splitting data into Training and Test
set.seed(1000)
split = sample.split(BCustDF$Exited, SplitRatio = 0.8)
training_set = subset(BCustDF, split == TRUE)
test_set = subset(BCustDF, split == FALSE)
View(training_set)
View(test_set)

#Applying Decision Tree Model
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
decision_tree_model<-rpart(Exited~CreditScore + Location + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,data=training_set,method="class",cp=0.0001,minbucket = 25)
rpart.plot(decision_tree_model,type=5)
printcp(decision_tree_model)
plotcp(decision_tree_model)
prp(decision_tree_model)

dtree.prune<- prune(decision_tree_model, cp=0.0004)
dtree.prune
rpart.plot(dtree.prune,type = 5,extra=101)
plotcp(dtree.prune)

dt_predic_class = predict(dtree.prune,test_set,type="class")

#Plotting ROC Curve
library(ROCR)
library(caTools)
install.packages("rsq")
library(rsq)
dt_predic_class <- as.numeric(dt_predic_class)
predt <- prediction(dt_predic_class,test_set$Exited)
ROCperflr <- performance(predt,measure="tpr",x.measure = "fpr")
plot(ROCperflr, colorsize = TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj=c(-0.2,1.7))
library(caret)

#data.frame(R2 = R2(as.numeric(dt_predic_class),as.numeric(test_set$Exited)), RMSE = RMSE(as.numeric(dt_predic_class),as.numeric(test_set$Exited)), MAE = MAE(as.numeric(dt_predic_class),as.numeric(test_set$Exited)))

class(dt_predic_class)
dt_predic_class <- as.factor(dt_predic_class)
levels(dt_predic_class) <- list("0" = "1", "1" = "2")
conf_mat <- confusionMatrix(as.factor(dt_predic_class),as.factor(test_set$Exited))
conf_mat
model <- "Decision Tree Model"
accuracy <- conf_mat$overall[1]
accuracy_table <- data.frame(model,accuracy)

accuracy_table

#--------------------------------------------------------------------------------------------------------

#Random Forest

BCustDF1 <- read.csv(file="Bank_CUstomer_Churn_Data.csv")

head(BCustDF1)
summary(BCustDF1)
str(BCustDF1)

sapply(BCustDF1,function(x) sum(is.na(x))) 

BCustDF1$RowNumber <- NULL
BCustDF1$Surname <- NULL
summary(BCustDF1)
head(BCustDF1)

#Converting into Factors
BCustDF1$Exited <- as.factor(BCustDF1$Exited)
#BCustDF1$CustomerId <- as.factor(BCustDF1$CustomerId)

BCustDF1$CreditScore <- as.numeric(BCustDF1$CreditScore)
BCustDF1$Location <- as.factor(BCustDF1$Location)
BCustDF1$Gender <- as.factor(BCustDF1$Gender)
BCustDF1$Age <- as.numeric(BCustDF1$Age)
BCustDF1$Tenure <- as.numeric(BCustDF1$Tenure)
BCustDF1$Balance <- as.numeric(BCustDF1$Balance)
BCustDF1$NumOfProducts <- as.numeric(BCustDF1$NumOfProducts)
BCustDF1$HasCrCard <- as.factor(BCustDF1$HasCrCard)
BCustDF1$IsActiveMember<- as.factor(BCustDF1$IsActiveMember)

#BCustDF1$EstimatedSalary <- as.factor(BCustDF1$EstimatedSalary)

library(randomForest)
#drops <- c("reviews_per_month")
#df = listings[ , !(names(listings) %in% drops)]
library(caTools)


set.seed(1000)
split = sample.split(BCustDF1$Exited, SplitRatio = 0.8)
training_set = subset(BCustDF1, split == TRUE)
test_set = subset(BCustDF1, split == FALSE)
View(training_set)
View(test_set)


#RandomForest Model

rf <- randomForest(formula = Exited ~ .,data = training_set,mtry=4, ntree=500, importance=TRUE, do.trace=100)
rf
print(rf)
importance(rf)
plot(rf)

dt_predic = predict(rf,test_set,type="class")

# Model Performance Matrix
data.frame(R2 = R2(as.numeric(dt_predic),as.numeric(test_set$Exited)), RMSE = RMSE(as.numeric(dt_predic),as.numeric(test_set$Exited)), MAE = MAE(as.numeric(dt_predic),as.numeric(test_set$Exited)))

class(dt_predic)
class(test_set$Exited)

#Plotting ROC Curve
#install.packages(("ROCR"))
library(ROCR)
dt_predic <- as.numeric(dt_predic)
predt <- prediction(dt_predic,test_set$Exited)
ROCperflr <- performance(predt,measure="tpr",x.measure = "fpr")
plot(ROCperflr, colorsize = TRUE, print.cutoffs.at=seq(0,1,by=0.2),text.adj=c(-0.2,1.7))

library(caret)
dt_predic<- as.factor(dt_predic)
levels(dt_predic) <- list("0" = "1", "1" = "2")
conf_mat <- confusionMatrix(as.factor(dt_predic),as.factor(test_set$Exited))
conf_mat
model <- "Random Forest Model"
accuracy <- conf_mat$overall[1]

accuracy_table <- data.frame(model,accuracy)

accuracy_table


  
