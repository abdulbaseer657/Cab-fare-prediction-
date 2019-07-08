
#-----------------setting up working directory ----------------------------------------------

rm(list=ls())
#setting working directory 
setwd("C:\\Users\\baseer\\Desktop\\project")

#-----------------------------Loading test and train datasets---------------------------------

#loading train dataset 
train=read.csv("train_cab.csv",header = TRUE,na.strings = c(""," ","0","NA","na"))
#loading test dataset
test=read.csv("test.csv",header = TRUE,na.strings = c(""," ","0","NA","na"))


#-----------------------------missing value analysis-------------------------------------------

##---------analysing the missing values in the train dataset---------------------

missing_val_train=data.frame(apply(train,2,function(x)(sum(is.na(x)))))
missing_val_train$columns=row.names(missing_val_train)
row.names(missing_val_train)=NULL
colnames(missing_val_train)[1]="missing_percentage"
missing_val_train$missing_percentage=missing_val_train$missing_percentage/16067*100
missing_val_train$missing_percentage=sort(missing_val_train$missing_percentage,TRUE)


##------------analysing the missing values in the test dataset-----------------------

missing_val_test=data.frame(apply(test,2,function(x)(sum(is.na(x)))))
missing_val_test$columns=row.names(missing_val_test)
row.names(missing_val_test)=NULL
colnames(missing_val_test)[1]="missing_percentage"
missing_val_test$missing_percentage=missing_val_test$missing_percentage/nrow(test)*100
missing_val_test$missing_percentage=sort(missing_val_test$missing_percentage,TRUE)

# NO missing value found in test dataset 

#----------------------------imputation of missing values ----------------------------------

## ----------------------imputatiion results for train dataset ------------------------------
train$fare_amount=as.numeric(as.character(train$fare_amount))
#actual value at [5,1]= 5.3
#removing a value at index[5,1] after recording it 
train[5,1]="NA"
#converting a datatype after imputing a value as NA 
train$fare_amount=as.numeric(train$fare_amount)
#mean value result=15.01
#median value result=8.50
#Knn imputation value result =6.03

#mean imputation
#train = transform(train,fare_amount=ifelse(is.na(fare_amount), mean(fare_amount,na.rm = TRUE) ,fare_amount))
#median 
#train = transform(train,fare_amount=ifelse(is.na(fare_amount), median(fare_amount,na.rm = TRUE) ,fare_amount))
#knn imputation
#load "DMwR" library for KNN imputation
library(DMwR)
train=knnImputation(train,k=3)

#hence we are freezing KNN imputation as it gives us the best results as recorded above 


#----------------------------imputing missing values in test dataset-------------------------

# NO missing values found in test dataset


#---------------------feature engineering of date_time and Distance---------------------------------------


##---------------------date_time feature engneering on train dataset --------------------------------
library("lubridate")
library(dplyr)
train = train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         month = month(pickup_datetime),
         year = year(pickup_datetime),
         day = day(pickup_datetime),
         dayOfWeek = wday(pickup_datetime),
         hour = hour(pickup_datetime)
  )

train=na.omit(train)
##------------------date_time feature engineering on test dataset-----------------------------------
library("lubridate")
test = test %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         month = month(pickup_datetime),
         year = year(pickup_datetime),
         day = day(pickup_datetime),
         dayOfWeek = wday(pickup_datetime),
         hour = hour(pickup_datetime)
  )
test=na.omit(test)
###--------feature engineering to extract the distance from given latitudes & logitudes-----

##------------------obtaining distance from lat and log in train dataset --------------------

#install/activate "geosphere" library to extact distance from lat and log 
library(geosphere)

#train<-na.omit(train)
#as longitudes cant be greater then 180 and less the -180 , we remove them
train=subset(train,(pickup_longitude<180&pickup_longitude>-180))
train=subset(train,(dropoff_longitude<180&dropoff_longitude>-180))

#as latitudes cant be greater then 90 and less the -90 , we remove them
train=subset(train,(pickup_latitude<90&pickup_latitude>-90))
train=subset(train,(pickup_latitude<90&pickup_latitude>-90))

#now using distHaversine formula from geosphere library we obtain the distance 

train = train %>% 
  mutate(distance = by(train, 1:nrow(train), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))

summary(train$distance)
#we observe min distance as 0 
#distance cant be 0 hence remove absorvations of 0

train$distance=as.numeric(train$distance)
train = train [-which(train$distance == 0), ]

##------------------obtaining distance from lat and log in test dataset --------------------

#install/activate "geosphere" library to extact distance from lat and log 
library(geosphere)

#test<-na.omit(test)
#as longitudes cant be greater then 180 and less the -180 , we remove them
test=subset(test,(pickup_longitude<180&pickup_longitude>-180))
test=subset(test,(dropoff_longitude<180&dropoff_longitude>-180))

#as latitudes cant be greater then 90 and less the -90 , we remove them
test=subset(test,(pickup_latitude<90&pickup_latitude>-90))
test=subset(test,(pickup_latitude<90&pickup_latitude>-90))

#now using distHaversine formula from geosphere library we obtain the distance 

test = test %>% 
  mutate(distance = by(test, 1:nrow(test), function(row) { 
    distHaversine(c(row$pickup_longitude, row$pickup_latitude), c(row$dropoff_longitude,row$dropoff_latitude))/1000}))

summary(test$distance)

#distance cant be 0 hence remove absorvations of 0

test$distance=as.numeric(test$distance)
test = test [-which(test$distance == 0), ]

#--------------------------Outlaier analysis ------------------------------------

##-------------- BoxPlots - Distribution and Outlier Check--------------------------
library("ggplot2")
numeric_index = sapply(train,is.numeric) #selecting only numeric variables

numeric_data = train[,numeric_index]

cnames = colnames(numeric_data)
# 
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "fare_amount"), data = subset(train))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="fare_amount")+
           ggtitle(paste("Box plot of fare_amount for",cnames[i])))
}

# ## Plotting box plots 
gridExtra::grid.arrange(gn1,ncol=1)
gridExtra::grid.arrange(gn2,ncol=1)
gridExtra::grid.arrange(gn3,ncol=1)
gridExtra::grid.arrange(gn4,ncol=1)
gridExtra::grid.arrange(gn5,ncol=1)
gridExtra::grid.arrange(gn6,ncol=1)
gridExtra::grid.arrange(gn7,ncol=1)
gridExtra::grid.arrange(gn8,ncol=1)
gridExtra::grid.arrange(gn9,ncol=1)
gridExtra::grid.arrange(gn10,ncol=1)
gridExtra::grid.arrange(gn11,ncol=1)
gridExtra::grid.arrange(gn12,ncol=1)

# # #loop to remove outlaiers from all variables
for(i in cnames){
  print(i)
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  #print(length(val))
  train = train[which(!train[,i] %in% val),]
}
summary(train)
rownames(train)=NULL

#---------outlier analysis in test dataset------------------------------

numeric_index_test = sapply(test,is.numeric) #selecting only numeric variables

numeric_data_test = test[,numeric_index_test]

cnames_test = colnames(numeric_data_test)
# 
for (i in 1:length(cnames_test))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames_test[i]), x = "fare_amount"), data = subset(test))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="fare_amount")+
           ggtitle(paste("Box plot of fare_amount for",cnames_test[i])))
}

# # #loop to remove outlaiers from all variables
for(i in cnames_test){
  print(i)
  val = test[,i][test[,i] %in% boxplot.stats(test[,i])$out]
  #print(length(val))
  test = test[which(!test[,i] %in% val),]
}
summary(test)
rownames(test)=NULL


#-------------------------------vizualization------------------------------------------------

library("ggplot2")
library("scales")
library("psych")
library("gplots")

#-----------vizualization the dependencies of each variable on the target variable----------

#passenger_count 
ggplot()+
  geom_point(aes(x=train$passenger_count,y=train$fare_amount),colour="red")


#month 
ggplot()+
  geom_point(aes(x=train$month,y=train$fare_amount),colour="red")


#year 
ggplot()+
  geom_point(aes(x=train$year,y=train$fare_amount),colour="red")


#day 
ggplot()+
  geom_point(aes(x=train$day,y=train$fare_amount),colour="red")


#dayOfWeek 
ggplot()+
  geom_point(aes(x=train$dayOfWeek,y=train$fare_amount),colour="red")


#hour 
ggplot()+
  geom_point(aes(x=train$hour,y=train$fare_amount),colour="red")


#distance 
ggplot()+
  geom_point(aes(x=train$distance,y=train$fare_amount),colour="red")


#---------------------------------feature selection---------------------------------

## Correlation Plot 
library("corrgram")
corrgram(train[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#we find negetive correlation in pickup_latitude,pickup_longitude ,drpoff_latitude,dropoff_latitude
# we also fing high correlation in target variable and ditance but we do not remove it 
#as we have a null hypothesis that distance is prapotional to fare_amount 
#we remove rest of the correlated variables by dimention reduction

## Chi-squared Test of Independence
factor_index = sapply(train,is.factor)
factor_data = train[,factor_index]

for (i in 1:10)
{
  print(names(train)[i])
  print(chisq.test(table(train$fare_amount,train[,i])))

}

#removing variable day and passenger_count as their p value is greater than 0.05 
##------------------------- Dimension Reduction in train---------------------------------
train = subset(train, 
                         select = -c(pickup_latitude,pickup_longitude ,dropoff_longitude,dropoff_latitude,day,pickup_datetime))


##------------------------- Dimension Reduction in test---------------------------------
test = subset(test, 
               select = -c(pickup_latitude,pickup_longitude ,dropoff_longitude,dropoff_latitude,day,pickup_datetime))

#------------------------feature scaling ---------------------------------------------
#Normality check
qqnorm(train$pickup_datetime)
hist(train$month)
qqnorm(train$month)
qqnorm(train$fare_amount)
hist(train$fare_amount)
qqnorm(train$year)
hist(train$year)
qqnorm(train$dayOfWeek)
hist(train$dayOfWeek)
qqnorm(train$hour)
hist(train$hour)
#after visualization of the data we understand that data is not distributed normally 

cnames=colnames(train[-1])
#Normalisation
for(i in cnames){
  print(i)
 train[,i] = (train[,i] - min(train[,i]))/
    (max(train[,i] - min(train[,i])))
}
#--------------------------processed datasets on test and train-----------------------------

train=na.omit(train)
View(train)

test=na.omit(test)
View(test)

#Hence we are done with data pre processing 
#-----------------------------model development---------------------------------

#removing all datasets except test and train data sets using setdiff()from dplyr
rm(list=setdiff(ls(),c("test","train")))

#set.seed(123)
train_index = sample(1:nrow(train), 0.8 * nrow(train))
train1 = train[train_index,]
test1 = train[-train_index,]


#Load Libraries
library(rpart)
library(MASS)



# ##rpart for regression
fit = rpart(fare_amount ~ ., data = train1, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test1[,-1])

summary(fit)

 #MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test1[,1], predictions_DT)

#Error Rate: 10.33
#Accuracy: 89.67


#run regression model
lm_model = lm(fare_amount ~., data = train1)
summary(lm_model)
#Predict

predictions_LR = predict(lm_model, test1[-1])

#Calculate MAPE
MAPE(test1[,1], predictions_LR)



library("randomForest")

RF_model = randomForest(fare_amount ~ ., train1, importance = TRUE, ntree = 500,method = "anova")
summary(RF_model)
predictions_rf = predict(RF_model, test1[-1])
MAPE(test1[,1], predictions_rf)
###########Model selection #########

MAPE(test1[,1], predictions_rf)
MAPE(test1[,1], predictions_LR)
MAPE(test1[,1], predictions_DT)

#random forest = 18.5%
#linear Regression = 18.6%
#dicision tree = 20.4% 
# hence we select a model with minimum MAPE 


#Hence random forest is beeing selected for the predictions

result = predict(RF_model, test)
result# is the predicted result for the test dataset 
###########hence the required regression is done ##########










