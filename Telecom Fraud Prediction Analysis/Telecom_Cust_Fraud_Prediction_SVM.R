#Apply SVM

library(caret)
library(irr)
library(rpart)
library(tidyverse)
library(e1071)

setwd("~/National College of Ireland/Machine Learning Project/Final Datasets/DMML_CA1/Telecom Customer Churn Analysis")

TelCust <- read.csv(file="Churn_100k.csv")

head(TelCust)

#Print the structure of the dataframe
print(str(TelCust))

#Checking if there are any null values
sapply(TelCust,function(x) sum(is.na(x)))

TelCust_new <- na.omit(TelCust)

sapply(TelCust_new,function(x) sum(is.na(x)))

summary(TelCust_new)

#Need to remove Outliers
par(mfrow=c(2,3))
boxplot(TelCust_new$monthly_minutes,ylab = "Monthly Minutes")
boxplot(TelCust_new$PrevBalance,ylab = "Previous Balance")
boxplot(TelCust_new$latePayments,ylab = "Late Payments")
boxplot(TelCust_new$TotalBilled,ylab = "Total billed")



#Apply COnditions to Remove Outliers
library(dplyr)
TelCust_new = subset(TelCust_new, PrevBalance<70 & PrevBalance>10 & latePayments>2 & TotalBilled<210 & TotalBilled>20)
str(TelCust_new)


#Check to confirm if outliers are removed
par(mfrow=c(2,3))
boxplot(TelCust_new$monthly_minutes,xlab = "Monthly Minutes")
boxplot(TelCust_new$PrevBalance,xlab = "Previous Balance")
boxplot(TelCust_new$latePayments,xlab = "Late Payments")
boxplot(TelCust_new$TotalBilled,xlab = "Total billed")
boxplot(TelCust_new$customerServiceCalls,xlab = "Customer Service Calls")
boxplot(TelCust_new$phone_area_code,xlab = "Phone Area Code")


library(ggplot2)
#Gender against Event_LABEL Outcome
ggplot(TelCust_new, 
       aes(x= gender,  group=EVENT_LABEL)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
           stat="count", 
           alpha = 0.7) +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop.. ), 
            stat= "count", 
            vjust = -.5) +
  labs(y = "Percentage", fill="Gender") +
  facet_grid(~EVENT_LABEL) +
  scale_y_continuous() + 
  scale_fill_manual(values = c("blue","purple")) + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Event Label - Legit/Fraud") +
  labs(x = "Gender")


TelCust_new$EmployeeCount <- NULL
TelCust_new$EmployeeNumber <- NULL
TelCust_new$StandardHours <- NULL
TelCust_new$Over18 <- NULL
TelCust_new$Application.ID <- NULL
TelCust_new$Employee.Source <- NULL
TelCust_new$email_domain <- NULL
TelCust_new$phoneModel <-NULL
TelCust_new$billing_postal <- NULL
TelCust_new$billing_state <- NULL
TelCust_new$partner <- NULL
TelCust_new$PhoneService <- NULL
TelCust_new$MultipleLines <- NULL
TelCust_new$contract_code <- NULL
TelCust_new$currency_code <- NULL
TelCust_new$mobileHotspot <- NULL
TelCust_new$device_protection <- NULL
TelCust_new$paymentMethod <- NULL
TelCust_new$wifiCallingText <- NULL
TelCust_new$OnlineBackup <- NULL
TelCust_new$customer_reg_date <- NULL
TelCust_new$billing_city <- NULL
TelCust_new$networkSpeed <- NULL
TelCust_new$paperlessBilling <- NULL
TelCust_new$mailingcode <- NULL
TelCust_new$EVENT_TIMESTAMP <- NULL
TelCust_new$billing_address <- NULL



#Removing Columns

#TelCust_new$latePayments<- NULL
str(TelCust_new)
summary(TelCust_new)

TelCust_new$EVENT_LABEL <- ifelse(TelCust_new$EVENT_LABEL == "fraud",1,0)

# Check for any correlated variables
library(corrplot)
cor <- cor(TelCust_new[sapply(TelCust_new,is.numeric)])
corrplot(cor)
cor(cor)



TelCust_new$gender <- as.factor(TelCust_new$gender)
TelCust_new$EVENT_LABEL <- as.factor(TelCust_new$EVENT_LABEL)
TelCust_new$monthly_minutes <- as.factor(TelCust_new$monthly_minutes)
TelCust_new$customerServiceCalls <- as.numeric(TelCust_new$customerServiceCalls)
TelCust_new$streaming_minutes <- as.numeric(TelCust_new$streaming_minutes)
TelCust_new$TotalBilled <- as.numeric(TelCust_new$TotalBilled)
TelCust_new$PrevBalance <- as.numeric(TelCust_new$PrevBalance)
TelCust_new$latePayments <- as.factor(TelCust_new$latePayments)
TelCust_new$ip_address_asn <- as.factor(TelCust_new$ip_address_asn)
TelCust_new$phone_area_code <- as.factor(TelCust_new$phone_area_code)


str(TelCust_new)

#Kernel SVM Model

library(caTools)
y_pred_svm <- as.integer((y_pred_svm))
y_pred_svm<-ifelse(y_pred_svm> 0.5,1,0)

set.seed(1000)

split = sample.split(TelCust_new$EVENT_LABEL, SplitRatio = 0.7)
training_set = subset(TelCust_new, split == TRUE)
test_set = subset(TelCust_new, split == FALSE)
View(training_set)
View(test_set)



#install.packages('e1071')
library(e1071)
#install.packages("kernlab")
library(kernlab)
classifier_svm = svm(formula = EVENT_LABEL ~ gender + monthly_minutes + PrevBalance + latePayments + TotalBilled +  customerServiceCalls + phone_area_code,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'polynomial')



# Predicting the Test set results

y_pred_svm = predict(classifier_svm, newdata = test_set)
p_class_svm <- factor(y_pred_svm, levels = levels(test_set[["EVENT_LABEL"]]))
# Making the Confusion Matrix
conf_mat <- confusionMatrix(as.factor(y_pred_svm),as.factor(test_set$EVENT_LABEL))
conf_mat

model <- "SVM Model"
accuracy <- conf_mat$overall[1]

accuracy_table <- data.frame(model,accuracy)

accuracy_table



