#Clear Environment-
rm(list=ls())
#Set working directory-
setwd("D:/R-programming/2.Project- Bike Rental-R_File")
#Check working directory-
getwd()

#load data-
data= read.csv("day.csv")

#------------------------------Exploratory Data Analysis-------------------------------------------#
class(data)
dim(data)
head(data)
names(data)
str(data)
summary(data)

#Remove the instant variable, as it is index in dataset.
data= subset(data,select=-(instant))
#Remove date variable as we have to predict count on seasonal basis not date basis-
data= subset(data,select=-(dteday))
#Remove casual and registered variable as count is sum of these two variables-
data= subset(data,select=-c(casual,registered))

#check the remaining variables-
names(data)

#Rename the variables-
names(data)[2]="year"
names(data)[3]="month"
names(data)[7]="weather"
names(data)[8]="temprature"
names(data)[10]="humidity"
names(data)[12]="count"

#Seperate categorical and numeric variables-
names(data)

#numeric variables-
cnames= c("temprature","atemp","humidity","windspeed","count")

#categorical varibles-
cat_cnames= c("season","year","month","holiday","weekday","workingday","weather")

#=================================Data Pre-processing==========================================#

#--------------------------------Missing Vlaue Analysis----------------------------------------#
#Check missing values in dataset-
sum(is.na(data))
#Missing value= 0
#No Missing values in data.

#-----------------------------------Outlier Analysis----------------------------------------------#
df=data
data=df

#create Box-Plot for outlier analysis-
library(ggplot2)    #Library for visulization-
for(i in 1:length(cnames)){
  assign(paste0("AB",i),ggplot(aes_string(x="count",y=(cnames[i])),d=subset(data))+
           geom_boxplot(outlier.color = "Red",outlier.shape = 18,outlier.size = 2,
                        fill="Purple")+theme_get()+
           stat_boxplot(geom = "errorbar",width=0.5)+
           labs(x="Count of Bike",y=cnames[i])+
           ggtitle("Boxplot of count of bikes with",cnames[i]))
}

gridExtra::grid.arrange(AB1,AB2,AB3,ncol=3)
gridExtra::grid.arrange(AB4,AB5,ncol=2)

#Replace outliers with NA-
for(i in cnames){
  print(i)
  outlier= data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(outlier))
  data[,i][data[,i] %in% outlier]=NA
}

sum(is.na(data))

#Impute outliers by median method-
data$humidity[is.na(data$humidity)]=median(data$humidity,na.rm=TRUE)
data$windspeed[is.na(data$windspeed)]=median(data$windspeed,na.rm=TRUE)

sum(is.na(data))

#---------------------------------Data Understanding----------------------------------------------#

#Barplot of bike rented with respect to working days-
ggplot(data, aes(x = reorder(weekday,-count), y = count))+
  geom_bar(stat = "identity",fill = "aquamarine3")+
  labs(title = "Number of bikes rented with respect to days", x = "Days of the week")+ 
  theme(panel.background = element_rect("antiquewhite"))+
  theme(plot.title = element_text(face = "bold"))

#->from bar plot we can see maximum bikes rented on day 5 least bikes on day 0.

#Bikes rented with respect to temp and humidity-
ggplot(data,aes(temprature,count)) + 
  geom_point(aes(color=humidity),alpha=0.5) +
  labs(title = "Bikes rented with respect to variation in temperature and hunidity", x = "Normalized temperature")+
  scale_color_gradientn(colors=c(dark blue,blue,light blue,light green,yellow,orange,red)) +
  theme_bw()

#->maximum bike rented between temp 0.50 to 0.75 and humidity 0.50 to 0.75

#Bikes rented with respect to temp and windspeed-
ggplot(data, aes(x = temprature, y = count))+
  geom_point(aes(color=weather))+
  labs(title = "Bikes rented with respect to temperature and weathersite", x = "Normalized temperature")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()

#->maximum bike rented with windspeed and normalized temp between 0.50 to 0.75

#Bikes rented with respect to temp and season-
ggplot(data, aes(x = temprature, y = count))+
  geom_point(aes(color=season))+
  labs(title = "Bikes rented with respect to temperature and season", x = "Normalized temperature")+
  #  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_bw()
#->maximum bike rented in season4

#---------------------------------Feature Selection----------------------------------------------#
df=data
data=df

#correlation analysis for numeric variables-
library(corrgram)
corrgram(data[,cnames],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation plot for numeric variables")

#correlated variable= temprature & atemp

#Anova analysis for categorical variable with target numeric variable-
for(i in cat_cnames){
  print(i)
  Anova_result= summary(aov(formula = count~data[,i],data))
  print(Anova_result)
}

#Dimension Reduction-
data = subset(data,select=-c(atemp,holiday,weekday,workingday))

#---------------------------------Feature Scaling--------------------------------------------#
df=data
data=df

#update numeric variables after dimension reduction-
cnames= c("temprature","humidity","windspeed","count")

#skewness test for continuous variables-
library(propagate)
for(i in cnames){
  print(i)
  skew= skewness(data[,i])
  print(skew)
}
#No skewness in dataset.

#Normality check using histogram plot-
hist(data$temprature,col="Green",xlab="Temprature",ylab="Frequency",
     main="Histogram of Temprature")
hist(data$humidity,col="Red",xlab="Humidity",ylab="Frequency",
     main="Histogram of Humidity")
hist(data$windspeed,col="Purple",xlab="Windspeed",ylab="Frequency",
     main="Histogram of Windspeed")

#check summary of continuous variable to check the scaling- 
for(i in cnames){
  print(summary(data[,i]))
}
#as from summary, the data is already normalized, so no need for scaling.

#save the pre-processed data in drive-
write.csv(data,"Bike_Rental_count.csv",row.names=FALSE)

#====================================Model Devlopment==========================================#

#Clean the Environment-
library(DataCombine)
rmExcept("data")

#Data Copy for refrance-
df=data
data=df

#Function for Error metrics to calculate the performance of model-
mape= function(y,y1){
  mean(abs((y-y1)/y))*100
}

#Function for r2 to calculate the goodness of fit of model-
rsquare=function(y,y1){
  cor(y,y1)^2
}

#convert categorical variables into dummy variable-
#Recall categorical variables-
cat_cnames= c("season","year","month","weather")

library(dummies)
data= dummy.data.frame(data,cat_cnames)

#divide the data into traina nd test-
set.seed(123)
train_index= sample(1:nrow(data),0.8*nrow(data))
train= data[train_index,]
test= data[-train_index,]

#------------------------------Decision Tree for Regression-----------------------------------#
#Model devlopment on train data-
library(rpart)

DT_model= rpart(count~.,train,method = "anova")
DT_model

#Prediction on train data-
DT_train= predict(DT_model,train[-25])

#Prediction on test data-
DT_test= predict(DT_model,test[-25])

#Mape calculation of train data-
DT_MAPE_Train = mape(train[,25],DT_train)
#mape= 56.30%

#Mape calculation of test data-
DT_MAPE_Test = mape(test[,25],DT_test)
#mape=23.70%

#r2 calculation for train data-
DT_r2_train= rsquare(train[,25],DT_train)
#r2_test= 0.79

#r2 calculation for test data-       
DT_r2_test=rsquare(test[,25],DT_test)
#r2_test= 0.75

##############Random Search CV in Decision Tree##############################################

#set parameters-
library(caret)

control = trainControl(method="repeatedcv", number=5, repeats=1,search=random)
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)

#model devlopment on train data-
RDT_model = caret::train(count~., data=train, method="rpart2",trControl=control,tuneGrid= tunegrid)

print(RDT_model)

#Best fit parameters
best_parameter = RDT_model$bestTune
print(best_parameter)

#build model based on best fit-
RDT_model = rpart(count ~ .,train, method = "anova", maxdepth =7)

#Prediction on train data-
RDT_train= predict(RDT_model,train[-25])

#Prediction on test data-
RDT_test= predict(RDT_model,test[-25])

#Mape calculation of train data-
RDT_MAPE_Train = mape(train[,25],RDT_train)
#mape= 56.30%

#Mape calculation of test data-
RDT_MAPE_Test = mape(test[,25],RDT_test)
#mape=23.70%

#r2 calculation for train data-
RDT_r2_train= rsquare(train[,25],RDT_train)
#r2_test= 0.79

#r2 calculation for test data-
RDT_r2_test=rsquare(test[,25],RDT_test)
#r2_test= 0.75

##############Grid Search CV in Decision Tree##############################################

control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(.maxdepth=c(6:18))

#model devlopment on train data-
GDT_model= caret::train(count~.,train, method="rpart2", tuneGrid=tunegrid, trControl=control)
print(GDT_model)

#Best fit parameters
best_parameter = GDT_model$bestTune
print(best_parameter)

#build model based on best fit-
GDT_model = rpart(count ~ .,train, method = "anova", maxdepth =7)

#Prediction on train data-
GDT_train= predict(GDT_model,train[-25])

#Prediction on test data-
GDT_test= predict(GDT_model,test[-25])

#Mape calculation of train data-
GDT_MAPE_Train = mape(train[,25],GDT_train)
#mape= 56.30%

#Mape calculation of test data-
GDT_MAPE_Test = mape(test[,25],GDT_test)
#mape=23.70%

#r2 calculation for train data-
GDT_r2_train= rsquare(train[,25],GDT_train)
#r2_test= 0.79

#r2 calculation for test data-
GDT_r2_test=rsquare(test[,25],GDT_test)
#r2_test= 0.75

#--------------------------------Random Forest for Regression------------------------------#
#Model devlopment on train data-
library(randomForest)
RF_model= randomForest(count~.,train,ntree=100,method="anova")

#Prediction on train data-
RF_train= predict(RF_model,train[-25])

#Prediction on test data-
RF_test= predict(RF_model,test[-25])

#Mape calculation of train data-
RF_MAPE_Train=mape(train[,25],RF_train)
#mape= 23.31%

#Mape calculation of test data-
RF_MAPE_Test=mape(test[,25],RF_test)
#mape= 17.41%

#r2 calculation for train data-
RF_r2_train=rsquare(train[,25],RF_train)
#r2_test= 0.96

#r2 calculation for test data-
RF_r2_test=rsquare(test[,25],RF_test)
#r2_test= 0.87

##############Random Search CV in Random Forest##############################################

control = trainControl(method="repeatedcv", number=5, repeats=1,search=random)
#maxdepth = c(1:30)
#tunegrid = expand.grid(.maxdepth=maxdepth)

#model devlopment on train data-
RGB_model = caret::train(count~., data=train, method="rf",trControl=control,tuneLength=10)

print(RGB_model)

#Best fit parameters
best_parameter = RGB_model$bestTune
print(best_parameter)

#build model based on best fit-
RGB_model = randomForest(count ~ .,train, method = "anova", mtry=8,importance=TRUE)

#Prediction on train data-
RGB_train= predict(RGB_model,train[-25])

#Prediction on test data-
RGB_test= predict(RGB_model,test[-25])

#Mape calculation of train data-
RGB_MAPE_Train = mape(train[,25],RGB_train)
#mape= 25.44%

#Mape calculation of test data-
RGB_MAPE_Test = mape(test[,25],RGB_test)
#mape=17.52%

#r2 calculation for train data-
RGB_r2_train= rsquare(train[,25],RGB_train)
#r2_test= 0.96

#r2 calculation for test data-
RGB_r2_test=rsquare(test[,25],RGB_test)
#r2_test= 0.86

##############Grid Search CV in Random Forest##############################################

control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(.mtry=c(6:18))

#model devlopment on train data-
GRF_model= caret::train(count~.,train, method="rf", tuneGrid=tunegrid, trControl=control)
print(GRF_model)

#Best fit parameters
best_parameter = GRF_model$bestTune
print(best_parameter)

#build model based on best fit-
GRF_model = randomForest(count ~ .,train, method = "anova", mtry=7)

#Prediction on train data-
GRF_train= predict(GRF_model,train[-25])

#Prediction on test data-
GRF_test= predict(GRF_model,test[-25])

#Mape calculation of train data-
GRF_MAPE_Train = mape(train[,25],GRF_train)
#mape= 24.88%

#Mape calculation of test data-
GRF_MAPE_Test = mape(test[,25],GRF_test)
#mape=17.61%

#r2 calculation for train data-
GRF_r2_train= rsquare(train[,25],GRF_train)
#r2_test= 0.96

#r2 calculation for test data-
GRF_r2_test=rsquare(test[,25],GRF_test)
#r2_test= 0.87

#---------------------------------Linear Regression--------------------------------------------------#

#Recall numeric variables to check the VIF for multicollinearity-
cnames= c("temprature","humidity","windspeed")
numeric_data= data[,cnames]

#VIF test-
library(usdm)
vifcor(numeric_data,th=0.7)

#Model devlopment on train data-
LR_model= lm(count~.,train)
summary(LR_model)

#prediction on train data-
LR_train= predict(LR_model,train[-25])

#prediction on test data-
LR_test= predict(LR_model,test[-25])

#Mape calculation of train data-
LR_MAPE_Train=mape(train[,25],LR_train)
#mape= 47.40%

#Mape calculation of test data-
LR_MAPE_Test=mape(test[,25],LR_test)
#mape= 16.87%

#r2 calculation for train data-
LR_r2_train=rsquare(train[,25],LR_train)
#r2_test= 0.84

#r2 calculation for test data-
LR_r2_test=rsquare(test[,25],LR_test)
#r2_test= 0.83

#--------------------------------Gradient Boosting----------------------------------------#
library(gbm)

#Develop Model
GB_model = gbm(count~., data = train, n.trees = 100, interaction.depth = 2)

#prediction on train data-
GB_train = predict(GB_model, train[-25], n.trees = 100)

#prediction on test data-
GB_test = predict(GB_model, test[-25], n.trees = 100)

#Mape calculation of train data-
GB_MAPE_Train=mape(train[,25],GB_train)
#mape= 37.02%

#Mape calculation of test data-
GB_MAPE_Test=mape(test[,25],GB_test)
#mape= 17.24%

#r2 calculation for train data-
GB_r2_train=rsquare(train[,25],GB_train)
#r2_test= 0.90

#r2 calculation for test data-
GB_r2_test=rsquare(test[,25],GB_test)
#r2_test= 0.85

##############Random Search CV in Gradient Boosting##############################################

control = trainControl(method="repeatedcv", number=5, repeats=1,search=random)
#maxdepth = c(1:30)
#tunegrid = expand.grid(.maxdepth=maxdepth)

#model devlopment on train data-
RGB_model = caret::train(count~., data=train, method="gbm",trControl=control,tuneLength=10)

print(RGB_model)

#Best fit parameters
best_parameter = RGB_model$bestTune
print(best_parameter)

#build model based on best fit-
RGB_model = randomForest(count ~ .,train, method = "anova", n.trees=15,
                         interaction.depth=4,shrinkage=0.433567,n.minobsinnode=20)

#Prediction on train data-
RGB_train= predict(RGB_model,train[-25])

#Prediction on test data-
RGB_test= predict(RGB_model,test[-25])

#Mape calculation of train data-
RGB_MAPE_Train = mape(train[,25],RGB_train)
#mape= 25.00%

#Mape calculation of test data-
RGB_MAPE_Test = mape(test[,25],RGB_test)
#mape=17.60%

#r2 calculation for train data-
RGB_r2_train= rsquare(train[,25],RGB_train)
#r2_test= 0.97

#r2 calculation for test data-
RGB_r2_test=rsquare(test[,25],RGB_test)
#r2_test= 0.86

##############Grid Search CV in Gradient Boosting##############################################

control = trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
tunegrid = expand.grid(n.trees = seq(2565,2575, by = 2),
                        interaction.depth = c(2:4), 
                        shrinkage = c(0.01,0.02),
                        n.minobsinnode = seq(18,22, by = 2))

#model devlopment on train data-
GGB_model= caret::train(count~.,train, method="gbm", tuneGrid=tunegrid, trControl=control)
print(GGB_model)

#Best fit parameters
best_parameter = GGB_model$bestTune
print(best_parameter)

#build model based on best fit-
GGB_model = randomForest(count ~ .,train, method = "anova", n.trees = 2569,
                         interaction.depth = 4,shrinkage = 0.01,n.minobsinnode = 20)

#Prediction on train data-
GGB_train= predict(GGB_model,train[-25])

#Prediction on test data-
GGB_test= predict(GGB_model,test[-25])

#Mape calculation of train data-
GGB_MAPE_Train = mape(train[,25],GGB_train)
#mape= 25,64%

#Mape calculation of test data-
GGB_MAPE_Test = mape(test[,25],GGB_test)
#mape=17.40%

#r2 calculation for train data-
GGB_r2_train= rsquare(train[,25],GGB_train)
#r2_test= 0.97

#r2 calculation for test data-
GGB_r2_test=rsquare(test[,25],GGB_test)
#r2_test= 0.86

#===================================Result====================================================#

Result= data.frame(Model=c('Decision Tree for Regression', 
                            'Random Search in Decision Tree','Gird Search in Decision Tree',
                            'Random Forest','Random Search in Random Forest','Grid Search in Random Forest',
                            'Linear Regression','Gradient Boosting','Random Search in Gradient Boosting',
                            'Grid Search in Gradient Boosting'),'MAPE_Train'=c(DT_MAPE_Train,
                            RDT_MAPE_Train,GDT_MAPE_Train,RF_MAPE_Train,RRF_MAPE_Train,
                            GRF_MAPE_Train,LR_MAPE_Train,GB_MAPE_Train,RGB_MAPE_Train,GGB_MAPE_Train),
                  'MAPE_Test'=c(DT_MAPE_Test,RDT_MAPE_Test,GDT_MAPE_Test,RF_MAPE_Test,RRF_MAPE_Test,
                                GRF_MAPE_Test,LR_MAPE_Test,GB_MAPE_Test,RGB_MAPE_Test,GGB_MAPE_Test),
                  'R-Squared_Train'=c(DT_r2_train,RDT_r2_train,GDT_r2_train,RF_r2_train,RRF_r2_train,
                                      GRF_r2_train,LR_r2_train,GB_r2_train,RGB_r2_train,GRF_r2_train),
                  'R-Squared_Test'=c(DT_r2_test,RDT_r2_test,GDT_r2_test,RF_r2_test,RRF_r2_test,
                                      GRF_r2_test,LR_r2_test,GB_r2_test,RGB_r2_test,GRF_r2_test))


#####################################Thank You##################################################



