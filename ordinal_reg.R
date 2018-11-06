train <- train_data
test <- test_data

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
install.packages("caret")
library(caret)


#A
head(train)
colnames(train)
str(train)
train$homeowner = factor(train$homeowner)
train$car_value = factor(train$car_value)
train$married_couple = factor(train$married_couple)
train$B = factor(train$B)
train$C = factor(train$C)
train$D = factor(train$D)
train$E = factor(train$E)
train$F = factor(train$F)
train$G = factor(train$G)

model <- polr(A ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)
summary(model)
ctable <- coef(summary(model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(model))
confint.default(model)
exp(coef(model))
exp(cbind(OR = coef(model), ci))
# 
# sf <- function(y) {
#   c('Y>=1' = qlogis(mean(y >= 1)),
#     'Y>=2' = qlogis(mean(y >= 2)),
#     'Y>=3' = qlogis(mean(y >= 3)))
# }
# 
# s <- with(train, summary(as.numeric(A) ~ group_size+ homeowner + car_age + car_value + risk_factor + 
#                           married_couple + C_previous + duration_previous + cost, fun=sf))
# 


test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$A = factor(test$A)
#newdat <- cbind(test, predict(model, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model, type = "class", newdata = test), test$A, dnn = c("Predictions", "Actual Values"))

#B


model1 <- glm(B ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                married_couple + C_previous + duration_previous + cost,family=binomial(link='logit'),data=train)
summary(model1)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$B = factor(test$B)
#newdat <- cbind(test, predict(model, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
pred = predict(model1, newdata=test)
accuracy <- table(pred, test$B)
1- sum(diag(accuracy))/sum(accuracy)
#C



model2 <- polr(C ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)

summary(model2)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$C = factor(test$C)
#newdat <- cbind(test, predict(model, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model2, newdata = test), test$C, dnn = c("Predictions", "Actual Values"))

#D



model3 <- polr(D ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)

summary(model3)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$D = factor(test$D)
#newdat <- cbind(test, predict(model, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model3, newdata = test), test$D, dnn = c("Predictions", "Actual Values"))

#E

# model4 <- polr(E ~ group_size+ homeowner + car_age + car_value + risk_factor + 
#                  married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)
# 
# summary(model2)
# str(train)
# test$homeowner = factor(test$homeowner)
# test$car_value = factor(test$car_value)
# test$married_couple = factor(test$married_couple)
# test$E = factor(test$E)
# #newdat <- cbind(test, predict(model, test, type = "probs"))
# newdat <- cbind(test, predict(model, test))
# confusionMatrix(predict(model4, newdata = test), test$E, dnn = c("Predictions", "Actual Values"))

model4 <- glm(E ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                married_couple + C_previous + duration_previous + cost,family=binomial(link='logit'),data=train)
summary(model4)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$E = factor(test$E)
#newdat <- cbind(test, predict(model, test, type = "probs"))
newdat <- cbind(test, predict(model, test))
pred = predict(model4, newdata=test)
accuracy <- table(pred, test$E)
1- sum(diag(accuracy))/sum(accuracy)
#F

model5 <- polr(F ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)

summary(model5)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$F = factor(test$F)
#newdat <- cbind(test, predict(model, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model5, newdata = test), test$F, dnn = c("Predictions", "Actual Values"))

#G
model6 <- polr(G ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                 married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)
str(train)
summary(model2)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$G = factor(test$G)
newdat <- cbind(test, predict(model6, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model6, newdata = test), test$G, dnn = c("Predictions", "Actual Values"))



for (i in 1:nrow(train)){
if (train$G[i] == 2){
  train$G_new[i] = 1
} else {train$G_new[i] = train$G[i]}
}
train$G_new = factor(train$G_new)
summary(train$G_new)

for (i in 1:nrow(test)){
  if (test$G[i] == 2){
    test$G_new[i] = 1
  } else {test$G_new[i] = test$G[i]}
}
train$G_new = factor(train$G_new)
summary(train$G_new)

model6_new <- polr(G_new ~ group_size+ homeowner + car_age + car_value + risk_factor + 
                     married_couple + C_previous + duration_previous + cost, data = train, Hess=TRUE)
str(train)
summary(model6_new)
str(train)
test$homeowner = factor(test$homeowner)
test$car_value = factor(test$car_value)
test$married_couple = factor(test$married_couple)
test$G_new = factor(test$G_new)
newdat <- cbind(test, predict(model6, test, type = "probs"))
#newdat <- cbind(test, predict(model, test))
confusionMatrix(predict(model6_new, newdata = test), test$G_new, dnn = c("Predictions", "Actual Values"))
summary(train$G)



library(arules)
A_check <- NULL
A_check <- train_data[c(2:3,5:6,10:16)]
str(A_check)
A_check$group_size <- factor(A_check$group_size)
A_check$homeowner <- factor(A_check$homeowner)
A_check$car_value <- factor(A_check$car_value)
A_check$risk_factor <- factor(A_check$risk_factor)
rules <- apriori(A_check, parameter = list(supp = 0.35, conf = 0.7,minlen=2, target = "rules"))
summary(rules)
inspect(head(rules, by = "lift"))
