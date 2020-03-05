bank = read.csv(file.choose())
View(bank)

library(stats)
library(dplyr)
library(readr)
library(Amelia)

##### EDA --------------------------
data(bank)
summary(bank)
data1 = bank
table(bank$default)
str(bank)
plot(bank$default)
#----------------------------------------------------------------------------------------
bank$default <- ifelse(bank$default == 'no',0,1)
head(bank$default)

set.seed(123)
indx = sample(1:nrow(bank), as.integer(0.9*nrow(bank)))

bank_train = bank[indx,]
bank_test =bank[-indx,]

prop.table(table(bank_train$default))*100

prop.table(table(bank_test$default))*100

bank_train_labels = bank[indx,17]
bank_test_labels = bank[-indx,17]

library(Amelia)

missmap(bank, main = "Missing values vs observed")

model <- glm(default ~.,family=binomial(link='logit'),data=bank_train)
model

anova(model, test="Chisq")

fitted.results <- predict(model,newdata=bank_test,type='response')
head(fitted.results)


fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)

head(bank_test$default)

misClasificError <- mean(fitted.results != bank_test$default)
misClasificError

print(paste('Accuracy',1-misClasificError))

library(ROCR)

p <- predict(model, newdata=bank_test, type="response")
pr <- prediction(p, bank_test$default)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0,1,lwd = 2, lty = 2)
#area under curve
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#logistic Regression model using Predict

model2 <- glm(default ~ balance + loan + 
              duration+ campaign + poutcome +
                y + housing,
              family=binomial(link='logit'),data=bank_train)
model2


anova(model2, test="Chisq")

fitted.results <- predict(model2,newdata=bank_test,type='response')
head(fitted.results)


fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)

head(bank_test$default)

misClasificError <- mean(fitted.results != bank_test$default)
misClasificError

print(paste('Accuracy',1-misClasificError))

library(ROCR)
p <- predict(model2, newdata=bank_test, type="response")
pr <- prediction(p, bank_test$default)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
abline(0,1,lwd = 2, lty = 2)


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

