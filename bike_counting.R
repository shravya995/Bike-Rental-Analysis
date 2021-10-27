#remove all the objects stored
rm(list=ls())
#set current working directory
setwd("C:/Users/SHRAVYA/Desktop/edwisor/project 3")
#install packages
install.packages(c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE", "sampling", "DataCombine", "inTrees","dplyr","usdm"))
## Read the data
bike_counting = read.csv("day.csv", header = T, na.strings = c(" ", "", "NA"))
#...........................exploratory data analysis................................................................................................ 
#exploratory data analysis
str(bike_counting)
bike_counting$dteday  = as.numeric(bike_counting$dteday )
bike_counting$season  = as.factor(bike_counting$season )
bike_counting$yr  = as.factor(bike_counting$yr )
bike_counting$mnth  = as.factor(bike_counting$mnth )
bike_counting$holiday  = as.factor(bike_counting$holiday )
bike_counting$weekday  = as.factor(bike_counting$weekday )
bike_counting$workingday  = as.factor(bike_counting$workingday )
bike_counting$weathersit  = as.factor(bike_counting$weathersit )
#.................................missing value analysis................................................

missing_val = data.frame(apply(bike_counting,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(bike_counting)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
#no missing values found
#ggplot analysis
ggplot(data = missing_val[1:3,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()
library(ggplot2)
numeric_index = sapply(bike_counting,is.numeric) #selecting only numeric

numeric_data = bike_counting[,numeric_index]


cnames = colnames(numeric_data)
# ............................... Outlier analysis .......................................#

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var), eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
  boxplot(var_name, main = "With outliers")
  hist(var_name,
       main = "With outliers",
       xlab = NA,
       ylab = NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main = "Without outliers")
  hist(var_name,
       main = "Without outliers",
       xlab = NA,
       ylab = NA)
  title("Outlier Check", outer = TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name)) *
                                            100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  
}


outlierKD(bike_counting, temp) #no outliers
outlierKD(bike_counting, atemp) #no outliers
outlierKD(bike_counting, hum) # no extreme outlier detected
outlierKD(bike_counting, windspeed) #some extreme values are present but canot be considered as outlier
outlierKD(bike_counting, casual) # no logical outliers
outlierKD(bike_counting, registered)# no ouliers
outlierKD(bike_counting, cnt)# no ouliers
#...............................Univariate Analysis ...........................................................
# 1. Continous predictors
univariate_continuous <- function(dataset, variable, variableName) {
  var_name = eval(substitute(variable), eval(dataset))
  print(summary(var_name))
  ggplot(data = dataset, aes(var_name)) +
    geom_histogram(aes(binwidth = .5, colour = "black")) +
    labs(x = variableName) +
    ggtitle(paste("count of", variableName))
}

univariate_continuous(bike_counting, cnt, "cnt")
univariate_continuous(bike_counting, temp, "temp")
univariate_continuous(bike_counting, atemp, "atemp")
univariate_continuous(bike_counting, hum, "hum") # skwed towards left
univariate_continuous(bike_counting, windspeed, "windspeed") #skewed towards right
univariate_continuous(bike_counting, casual, "casual") # skwed towards right
univariate_continuous(bike_counting, registered, "registered")
#2. categorical variables
univariate_categorical  <- function(dataset, variable, variableName) {
  variable <- enquo(variable)
  
  percentage <- dataset +
    dplyr::select(!!variable) +
    group_by(!!variable) +
    summarise(n = n()) +
    mutate(percantage = (n / sum(n)) * 100)
  print(percentage)
  
  dataset +
    count(!!variable) +
    ggplot(mapping = aes_(
      x = rlang::quo_expr(variable),
      y = quote(n),
      fill = rlang::quo_expr(variable)
    )) +
    geom_bar(stat = 'identity',
             colour = 'white') +
    labs(x = variableName, y = "count") +
    ggtitle(paste("count of ", variableName)) +
    theme(legend.position = "bottom") -> p
  plot(p)
}

univariate_categorical(bike_counting, season, "season")
univariate_categorical(bike_counting, yr, "yr")
univariate_categorical(bike_counting, mnth, "mnth")
univariate_categorical(bike_counting, holiday, "holiday")
univariate_categorical(bike_counting, weekday, "weekday")
univariate_categorical(bike_counting, workingday, "workingday")
univariate_categorical(bike_counting, weathersit, "weathersit")
#................................bivariate analysis.................................................

#.........# bivariate analysis for categorical variables
bivariate_categorical <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    
    ggplot(
      data = dataset,
      mapping = aes_(
        x = rlang::quo_expr(variable),
        y = rlang::quo_expr(targetVariable),
        fill = rlang::quo_expr(variable)
      )
    ) +
      geom_boxplot() +
      theme(legend.position = "bottom") -> p
    plot(p)
    
  }

bivariate_continous <-
  function(dataset, variable, targetVariable) {
    variable <- enquo(variable)
    targetVariable <- enquo(targetVariable)
    ggplot(data = dataset,
           mapping = aes_(
             x = rlang::quo_expr(variable),
             y = rlang::quo_expr(targetVariable)
           )) +
      geom_point() +
      geom_smooth() -> q
    plot(q)
    
  }

bivariate_categorical(bike_counting, season, cnt)
bivariate_categorical(bike_counting, yr, cnt)
bivariate_categorical(bike_counting, mnth, cnt)
bivariate_categorical(bike_counting, holiday, cnt)
bivariate_categorical(bike_counting, weekday, cnt)
bivariate_categorical(bike_counting, workingday, cnt)
bivariate_categorical(bike_counting, weathersit, cnt)

bivariate_continous(bike_counting, temp, cnt)
bivariate_continous(bike_counting, atemp, cnt)
bivariate_continous(bike_counting, hum, cnt)
bivariate_continous(bike_counting, windspeed, cnt)
bivariate_continous(bike_counting, casual, cnt)
bivariate_continous(bike_counting, registered, cnt)
#.....................feature selection...............................


library(corrgram)
## Correlation Plot 
corrgram(bike_counting[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#atemp and temp are highly correlated.
#...................................feature reduction.....................................

bike_counting = subset(bike_counting, select = -c(atemp,dteday))

#Feature Scaling
#Normality check
qqnorm(bike_counting$cnt )
hist(bike_counting$cnt )
str(bike_counting)
#the variables casual,registered,and count are not much skwed but the other variables are highly skwed, hence we apply normalization.
#Normalisation
cnames = c("temp","hum","windspeed","casual","registered","cnt")

for(i in cnames){
  print(i)
  bike_counting[,i] = (bike_counting[,i] - min(bike_counting[,i]))/
    (max(bike_counting[,i] - min(bike_counting[,i])))
}
#.......................................Sampling............................................................................
##Systematic sampling
#Function to generate Kth index
sys.sample = function(N,n)
{
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp = seq(r, r + k*(n-1), k)
}
lis = sys.sample(731, 300) #select the repective rows
# #Create index variable in the data
bike_counting$index = 1:731
# #Extract subset from whole data
systematic_data = bike_counting[which(bike_counting$index %in% lis),]
#................................Model Development............................................................#

#Clean the environment
library(DataCombine)
rmExcept("bike_counting")
#Divide data into train and test using stratified sampling method
set.seed(1234)
bike_counting$description = NULL
library(caret)
train.index = createDataPartition(bike_counting$cnt, p = .80, list = FALSE)
train = bike_counting[ train.index,]
test  = bike_counting[-train.index,]
#load libraries
#rpart for regression
library(rpart)
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-14])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,15], predictions_DT)

#Error Rate: 1.35
#Accuracy: 99.65
#rmse calculation
install.packages("Metrics")
library(Metrics)
rmse(test$bike_counting, predictions_DT)
#rmse value is 0.021
###Random Forest
library(randomForest)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 1000)
#Extract rules fromn random forest
#transform rf object to an inTrees' format
library(RRF)
library(inTrees)
treeList <- RF2List(RF_model)
#Extract rules
exec = extractRules(treeList, train[,-14])  # R-executable conditions
ruleExec <- extractRules(treeList,train[,-14],digits=4)

##Make rules more readable:
readableRules = presentRules(exec, colnames(train))
readableRules[1:2,]
##Get rule metrics
ruleMetric = getRuleMetric(exec, train[,-14], train$cnt)  # get rule metrics


#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-14])

MAPE(test[,15], RF_Predictions)

#Error Rate: 1.34
#Accuracy: 99.66
rmse(test$bike_counting, RF_Predictions)
#rmse value is 0.012
#linear regression model
#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(bike_counting), 0.8 * nrow(bike_counting))
train = bike_counting[train_index,]
test = bike_counting[-train_index,]
# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")
library(rpart)
#Predict for new test cases
predictions_DT = predict(fit, test[,-14])
#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,14], predictions_DT)

#Error Rate: 13.41
#Accuracy: 88.59
rmse(test$bike_counting, predictions_DT)
#rmse value is 10.33
