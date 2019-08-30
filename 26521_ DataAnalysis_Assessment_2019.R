#------------------------------------------------------------
# Name: Bijalben Prajapati
# Matriculation number: 26521
# Course name: Data Analysis and Statistics 
# Date: 31/08/2019
#------------------------------------------------------------
rm(list=ls())
install.packages('ISLR')
while(!is.null(dev.list()))
{
  dev.off()
}

require(ISLR)
library(ISLR)
attach(Wage)
library(caret)
library(ISLR)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(olsrr)
library(randomForest)
library(sjPlot)
library(Boruta)


assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
colnames(assessment_dataframe)
Sel_data <- assessment_dataframe[,c(1,2,5,11)]
colnames(Sel_data)

# understand data description  
summary(Sel_data)

# check if data contaion any null value or not
is.null(Sel_data$age)
is.null(Sel_data$wage)
is.null(Sel_data$education)
is.null(Sel_data$year)

# handly categorical varaible
Dummy_results <- fastDummies::dummy_cols(Sel_data)
colnames(Dummy_results)
Sel_Data_Dummy<-Dummy_results[,c(1,2,4:9)]
colnames(Sel_Data_Dummy)

# give proper name
names(Sel_Data_Dummy)[names(Sel_Data_Dummy) == "education_5. Advanced Degree"] <- "Advanced_Degree"
names(Sel_Data_Dummy)[names(Sel_Data_Dummy) == "education_4. College Grad"] <- "College_Grad"
names(Sel_Data_Dummy)[names(Sel_Data_Dummy) == "education_1. < HS Grad"] <- "less_HS_Grad"
names(Sel_Data_Dummy)[names(Sel_Data_Dummy) == "education_3. Some College"] <- "Some_College"
names(Sel_Data_Dummy)[names(Sel_Data_Dummy) == "education_2. HS Grad"] <- "HS_Grad"
colnames(Sel_Data_Dummy)

summary(Sel_Data_Dummy)
head(Sel_Data_Dummy)
# check normality of wage
shapiro.test(Sel_Data_Dummy$wage)

# check how much data is fit to linear line  
#pairs (Sel_data, upper.panel = NULL)
pairs (Sel_data, upper.panel = NULL, panel = panel.smooth)

# Hypothesis Test for finding relationship 
cor.test(Sel_Data_Dummy$year, Sel_Data_Dummy$wage, method = "spearman", exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$age, Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$College_Grad, Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$Some_College, Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$less_HS_Grad,  Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$HS_Grad, Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)
cor.test(Sel_Data_Dummy$Advanced_Degree, Sel_Data_Dummy$wage, method = "spearman" , exact = F, conf.level = 0.95)

# Find Stringth and Type of relationship 
Correlation <- cor(Sel_Data_Dummy,method = "spearman") 
round(Correlation, 2)

# Feature Selection for traning a model
Feature_Selection_model <- lm(wage ~ ., data = Sel_data) 
ols_step_all_possible(Feature_Selection_model)

# handle dummy variable trap problem
Sel_Data_Model_Dummy <- Sel_Data_Dummy[,!(names(Sel_Data_Dummy) %in% c("Some_College"))]
colnames(Sel_Data_Model_Dummy)

# train model and predict wage with multiple regression 
set.seed(222)
ind <- sample(2, nrow(Sel_Data_Model_Dummy), replace = T, prob = c(0.8, 0.2))
train <- Sel_Data_Model_Dummy[ind==1,]
test <- Sel_Data_Model_Dummy[ind==2,]
Wage_Relationship_by_error_model <- lm(wage ~ ., data = train)
WRE_Prediction_value <- predict(Wage_Relationship_by_error_model, test)
postResample(pred = WRE_Prediction_value, obs = test$wage)

# predicted result Vs. actual result 
plot(test$wage,col='red',main='a',pch=18,cex=0.7)
points(WRE_Prediction_value,col='blue',pch=18,cex=0.7)
legend('bottomright',legend=c('Actual','Predicted'),pch=18,col=c('red','blue'))

# find interaction between variables 
interaction_YE <- lm(wage ~ year:education, data = Sel_data)
plot_model(interaction_YE, type = "int")

interaction_AE <- lm(wage ~ age:education, data = Sel_data)
plot_model(interaction_AE, type = "int")

interaction_AY <- lm(wage ~ age:year, data = Sel_data)
plot_model(interaction_AY, type = "int")

#train model and predict wage with multiple regression by use interection variable in this  
Improve_Interaction_model <- lm(wage ~ year+age+College_Grad+Advanced_Degree+HS_Grad+less_HS_Grad+
                                  age:(College_Grad+Advanced_Degree+HS_Grad+less_HS_Grad), data = train)
II_Prediction_value <- predict(Improve_Interaction_model, test)
postResample(pred = II_Prediction_value, obs = test$wage)

#train model and predict wage with multiple regression by normalizing age
Improve_Log_model <- lm(wage ~ year+log(age)+College_Grad+Advanced_Degree+HS_Grad+less_HS_Grad+log(age):College_Grad+Advanced_Degree+HS_Grad+less_HS_Grad, data = train)
IL_Prediction_value <- predict(Improve_Log_model, test)
postResample(pred = IL_Prediction_value, obs = test$wage)

rf <- randomForest(wage ~ log(age)+(age):College_Grad+Advanced_Degree+HS_Grad+less_HS_Grad, data = train)
rf_Prediction_value <- predict(rf, test)
postResample(pred = rf_Prediction_value, obs = test$wage)