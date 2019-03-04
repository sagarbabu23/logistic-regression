install.packages("xlsx")
install.packages("MASS")
install.packages("car")
install.packages("Hmisc")
install.packages("pastecs")
install.packages("psych")
library(xlsx)
library(MASS)
library(car)
library(Hmisc)
library(pastecs)
library(psych)
stepAIC(y_model)

bank_data <- read.csv(file.choose(), sep = ";")
View(bank_data)
summary(bank_data)
colnames(bank_data)
attach(bank_data)
y_model <- glm(y ~ age +factor(job)+ factor(marital)+ factor(education)+balance + duration + campaign + pdays + previous + factor(default) + factor(housing) + factor(loan)+ factor(poutcome),family = binomial , data = bank_data)
summary(y_model)

exp(coef(y_model))

# Confusion matrix table 
prob <- predict(y_model,bank_data,type="response")
summary(y_model)
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank_data$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 89.9

##### for prob>0.4#########
confusion<-table(prob>0.4,bank_data$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 89.9



###########affarirs data set###########

install.packages("data.table")
library(data.table)

install.packages('AER')
data(Affairs,package="AER")
summary(Affairs)
head(Affairs)
View((Affairs))
str(Affairs)
gender_F <- ifelse(Affairs$gender=="male",1,0)
Children_F <- ifelse(Affairs$children=="yes",1,0)

Affairs <- cbind(Affairs,gender_F,Children_F)
attach(Affairs)
View(Affairs)
y_model <- glm(affairs ~ age + yearsmarried + religiousness + education +
                 occupation + rating + gender_F + Children_F, data= Affairs)
summary(y_model)
Y_model <- exp(coef(y_model))


library(MASS)
library(car)

stepAIC(y_model)

prob_y <- as.data.frame(predict(y_model, type = c("response"), Affairs))

final_y <- cbind(Affairs, prob_y)

confusion_y <- table(prob_y>0.4, Affairs$affairs)

table(prob_y>0.4)

confusion_y
accuracy_y <- (347+24+17+18 +40 +37)/sum(confusion_y)

accuracy_y

