#Removed all the existing objects
rm(list = ls())
# set working directory 
setwd("C:/Users/HP/Desktop/Bikerent")
getwd()
#Load data
bikedata = read.csv("bike.csv",header=TRUE)

####################### Understand data (Data anlysis) ########################

##to understand values of col
str(bikedata)


class(bikedata)


## to understand summary
summary(bikedata)

## to understand dimension 
dim(bikedata)

## from above understanding of data it is clear that some data type not correct 
##data type change 

bikedata$dteday = as.Date(bikedata$dteday,format="%Y-%m-%d")
bikedata$season=as.factor(bikedata$season)
bikedata$yr=as.factor(bikedata$yr)
bikedata$mnth=as.factor(bikedata$mnth)
bikedata$holiday=as.factor(bikedata$holiday)
bikedata$weekday=as.factor(bikedata$weekday)
bikedata$workingday=as.factor(bikedata$workingday)
bikedata$weathersit=as.factor(bikedata$weathersit)

str(bikedata)

## now we find dependent and indepndent variables
## after understanding variables as 'dteday','instant''casual','registered'does not
## removal of variable which are not required for further process


#Extracting the day values from the date and storing into a new column - 'day'
bikedata$day=format(bikedata$dteday,"%d")
unique(bikedata$day)

#Using plot() function to visualize the relationship between the data column 'day' and dependent variable 'cnt'

plot(bikedata$day,bikedata$cnt)

library(ggplot2)          
ggplot(bikedata, aes(instant, cnt)) + geom_point() + scale_x_continuous("Instant")+ scale_y_continuous("Count")

bikedata=subset(bikedata,select = -c(instant,dteday,casual,registered))
str(bikedata)
dim(bikedata)
########################## Missing value anlaysis ##############################

sum(is.na(bikedata))
summary(is.na(bikedata))
###From this it is clear that data has no missing value

## there is no missing value in dataset

############################ Outlier Anlaysis ################################
numeric_col = c('temp','atemp','hum','windspeed')
categorical_col = c("season","yr","mnth","holiday","weekday","workingday","weathersit")

###  to detect outliers in continous variables 
boxplot(bikedata[,c('temp','atemp','hum','windspeed')])

### With help of box plot we are able to understand that there are outliers in data
#### values above and below quartile are outliers now replace it with NULL

for (x in c('hum','windspeed'))
{
  value = bikedata[,x][bikedata[,x] %in% boxplot.stats(bikedata[,x])$out]
  bikedata[,x][bikedata[,x] %in% value] = NA
} 

####Checking whether the outliers in the above defined columns are replaced by NULL or not
##
sum(is.na(bikedata$hum))
sum(is.na(bikedata$windspeed))
as.data.frame(colSums(is.na(bikedata)))
#Removing the null values
library(tidyr)
bikedata = drop_na(bikedata)
as.data.frame(colSums(is.na(bikedata)))

######################## feature selection  #######################

### Numeric/Continuous data variables of the dataset
print(numeric_col)

library(corrgram)
corrgram(bikedata[,numeric_col],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis Plot")
##### From above it is clear that temp and atemp are highly corealted 
bikedata = subset(bikedata,select = -c(atemp))
str(bikedata)

#### Categorical variables of dataset.

print(categorical_col)
for(x in categorical_col)
{
  print(x)
  anova_test = aov(cnt ~ bikedata[,x],bikedata)
  print(summary(anova_test))
}

######From the ANOVA Test analysis, it is clear that the variables 
######['workingday'and 'weekday'] have p-values > 0.05. 
######Thus, we drop these data variables.
bikedata = subset(bikedata, select=-c(weekday,workingday))
str(bikedata)


####################### Feature Scaling ##############################
#### Before performing data scaling we need to check data distribution
#### If data is normally distributed then no need to apply scaling technique.
#### If data is skewed then there is need to normalization of data by using scaling technique.
### QQCure,Histogram, skewness test on continous variables

qqnorm(bikedata$temp)
qqnorm(bikedata$hum)
qqnorm(bikedata$windspeed)

hist(bikedata$temp)
hist(bikedata$hum)
hist(bikedata$windspeed)

library(e1071)
num_col = c('temp','hum','windspeed')
for(x in num_col)
{
  print(x)
  skewtest = skewness(bikedata[,x])
  print(skewtest)
}

##### From above it si clear that data is normally distributed 
##### There are total 9 variables 1 is dependent and other are independent variables
##### So, after pre-processing of data we get to know that problem statemnt is of predictive,
##### that is Regression type of business problem statement.

############################## Sampling of data ##############################


##### sampling of data into training and testing 

categorical_col_updated = c('season','yr','mnth','weathersit','holiday')
library(dummies)
bike = bikedata
bike = dummy.data.frame(bike,categorical_col_updated)
dim(bike)
#Separating the depenedent and independent data variables into two dataframes.
library(caret)
set.seed(101)
split_val = createDataPartition(bike$cnt, p = 0.80, list = FALSE) 
train_data = bike[split_val,]
test_data = bike[-split_val,]


############################## Modeling of data ##############################


#1. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}

#2. R SQUARE error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}


##MODEL 1: LINEAR REGRESSION
linear_model = lm(cnt~., train_data) #Building the Linear Regression Model on our dataset
summary(linear_model)
linear_predict=predict(linear_model,test_data[-27]) #Predictions on Testing data
LR_MAPE = MAPE(test_data[,27],linear_predict) # Using MAPE error metrics to check for the error rate and accuracy level
LR_R = RSQUARE(test_data[,27],linear_predict) # Using R-SQUARE error metrics to check for the error rate and accuracy level
Accuracy_Linear = 100 - LR_MAPE
print("MAPE: ")
print(LR_MAPE)
print("R-Square: ")
print(LR_R)
print('Accuracy of Linear Regression: ')
print(Accuracy_Linear)


##MODEL 2: DECISION TREES
library(rpart)
DT_model =rpart(cnt~., train_data, method = "anova" , minsplit=5)
DT_predict = predict(DT_model,test_data[-27])
DT_MAPE = MAPE(test_data[,27],DT_predict)
DT_R = RSQUARE(test_data[,27],DT_predict)
Accuracy_DT = 100 - DT_MAPE
print("MAPE: ")
print(DT_MAPE)
print("R-Square: ")
print(DT_R)
print('Accuracy of Decision Tree: ')
print(Accuracy_DT)

##MODEL 3: RANDOM FOREST
library(randomForest)
set.seed(123)
RF_model = randomForest(cnt~., train_data, ntree = 300, importance = TRUE)
RF_predict=predict(RF_model,test_data[-27])
RF_MAPE = MAPE(test_data[,27],RF_predict)
RF_R = RSQUARE(test_data[,27],RF_predict)
Accuracy_RF = 100 - RF_MAPE
print("MAPE: ")
print(RF_MAPE)
print("R-Square: ")
print(RF_R)
print('Accuracy of Random Forest: ')
print(Accuracy_RF)

##MODEL 4: KNN 
library('FNN')
set.seed(123)
KNN_model = FNN::knn.reg(train = train_data[,], test = test_data[,], y = train_data[,27], k = 3)$pred
KNN_predict=ceiling(KNN_model$pred[1:27]) #Predicted values
KNN_MAPE = MAPE(test_data[,27],KNN_predict)
Accuracy_KNN = 100 - KNN_MAPE
print("MAPE: ")
print(KNN_MAPE)
print('Accuracy of KNN: ')
print(Accuracy_KNN)


Bike_res = data.frame('Actual_count' = test_data[,27], 'Predicted_count' = RF_predict )
write.csv(Bike_res,"BIKE_RESULT_R.csv",row.names=FALSE)


