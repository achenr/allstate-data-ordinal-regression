my_data = read.csv("C:/Users/Akshaya/Dropbox/Adv Stats/Project/train.csv",na.strings = c(""," ","NA"))
View(my_data)

plcy_data <- my_data[my_data$record_type==1,]
plcy_data <- plcy_data[,-c(1:7)]
str(plcy_data)

#converting independent variables to appropriate datatype
col_names <- colnames(plcy_data[,c(2,8)])
for(i in col_names){
  plcy_data[[i]] <- as.factor(plcy_data[[i]])
}
plcy_data$C_previous <- ordered(plcy_data$C_previous)

#converting dependent variables to appropriate datatype
col_names <- colnames(plcy_data[,c(11:17)])
for(i in col_names){
  plcy_data[[i]] <- ordered(plcy_data[[i]])
}

#checking for missing values
sum(is.na(plcy_data$group_size))
sum(is.na(plcy_data$homeowner))
sum(is.na(plcy_data$car_age))
sum(is.na(plcy_data$car_value))
sum(is.na(plcy_data$risk_factor)) #34346 missing values
sum(is.na(plcy_data$age_oldest))
sum(is.na(plcy_data$age_youngest))
sum(is.na(plcy_data$married_couple))
sum(is.na(plcy_data$C_previous)) #836 missing values
sum(is.na(plcy_data$duration_previous)) #836 missing values
sum(is.na(plcy_data$cost))

#handling missing values
library(mice)
set.seed(1000)
plcy_data_sample = plcy_data[sample(nrow(plcy_data),20000),]
plcy_data_mice = mice(plcy_data_sample,maxit = 3,m=3, method = 'pmm')
final_data = data.frame(complete(plcy_data_mice,1))
str(final_data)

write.csv(final_data,"C:/Users/Akshaya/Dropbox/Adv Stats/Project/final_data.csv")

final_data$age_diff <- final_data$age_oldest-final_data$age_youngest

#Removing Age_old & young
final_data <- final_data[,-c(6:7)]

#Grouping car value b,c & i to a and h
final_data$car_value[final_data$car_value=='b' | final_data$car_value=='c'] = 'a' 
final_data$car_value[final_data$car_value=='i'] = 'h' 
final_data$car_value <- factor(final_data$car_value,levels = c('a','d','e','f','g','h'))
table(final_data$car_value)

#checking for outliers
bp = boxplot(final_data$car_age)
summary(bp)
length(boxplot.stats(final_data$car_age)$out) #203 outliers
hist(final_data$car_age)
sort(bp$out,decreasing = TRUE)

#correlation 
library(corrplot)
corr = cor(final_data[,c(1,3,8,17)])
corr_m = cor.mtest(final_data[,c(1,3,8,17)],conf.level=0.99)
corrplot(corr, type='upper',method='number',p.mat = corr_m$p,sig.level = 0.01)
View(corr)


#LINEAR MODEL to predict cost
index = sample(2,nrow(final_data),replace=TRUE,prob = c(0.7,0.3))
train_data = final_data[index==1,]
test_data = final_data[index==2,]

lm_model = lm(cost~.,data=train_data)
no_model = lm(cost~1,data=train_data)
step(lm_model,scope = list(lower = no_model,upper = lm_model),direction = "backward")

lm_model_new = lm(cost ~ homeowner + car_age + car_value + risk_factor + 
                    married_couple + C_previous + duration_previous + A + 
                    C + E + F + G + age_diff, data = train_data)

pred_lm = predict(lm_model_new,test_data)
head(pred_lm)
head(test_data$cost)
mean((pred_lm-test_data$cost)^2)


#RANDOM FOREST
library(randomForest)
rf_model = randomForest(cost ~ homeowner + car_age + car_value + risk_factor + 
                          married_couple + C_previous + duration_previous + A + 
                          C + E + F + G + age_diff, data = train_data, mtry = 10, ntree=100)
rf_pred = predict(rf_model,test_data)
mean((rf_pred-test_data$cost)^2)

