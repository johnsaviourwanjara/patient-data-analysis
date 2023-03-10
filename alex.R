dt <- readRDS(file.choose())
dt <- subset(dt[-(1:2 && 21)])
colnames(dt)
#Test for coxph
library(survival)
library(stargazer)
library(ggplot2)
library(survminer)
res.cox <- coxph(Surv(time_to_treatmentsuccess_1, time_to_treatmentsuccess_2) ~., data =  dt)
stargazer(res.cox,type = 'text')

# this test is good however the distribution of the data affects its use as dataset comprises of zeros

#Normality test
res_aov <- aov(time_to_treatmentsuccess_1 ~ time_to_treatmentsuccess_2, data = dt)
summary(res_aov)
#check normality visually
par(mfrow=c(1,2))

#QQ_plot
library(car)
qqPlot(res_aov$residuals,id=FALSE)

#histogram
hist(res_aov$residuals)

#Both the qqplot and the histogram indicates a non-normal data distribution

#Test for Multicollinearity
library(tidyverse)
library(caret)

# Split the data into training and test set
set.seed(123)
training.samples <- dt$time_to_treatmentsuccess_1 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dt[training.samples, ]
test.data <- dt[-training.samples, ]
train.data<-subset(train.data[-1])
test.data<-subset(test.data[-1])
#Creating a regression model
model_1 <- lm(time_to_treatmentsuccess_1 ~., data = train.data)

#predictions
predictions <- model_1 %>% predict((test.data),na.omit=TRUE)

# Performance of the model
per_mod <- data.frame(
  RMSE = RMSE(predictions, test.data$time_to_treatmentsuccess_1),
  R2 = R2(predictions, test.data$time_to_treatmentsuccess_1)
)

#Checking for multicollinearity
car::vif(model_1)#Returning aliased coeficients error implying existence of multicollinearity 

#test for AFT
library(dplyr)
dt %>% 
  sapply(levels)
AFT <- survreg(Surv(time_to_treatmentsuccess_1,en_group2) ~., data =  dt)
# Accelerated failure rate models can not be used since;
##there are no column with valid survival times for the distribution
##No column with 2 or more levels, all are NULL

#test for clustered standard error
library(miceadds)

m2 <- lm.cluster(time_to_treatmentsuccess_1~.,
                 cluster = 'patient_uuid',
                 data = dt)
m2coeffs <- data.frame(summary(m2))
m2coeffs[!startsWith(row.names(m2coeffs), 'patient_uuid'),]

#test for omitted variable bias
#this test can only be conducted if we are omitting certain variables within the dataset for a counter check,however the distribution of the dataset is an impediment to the test.
