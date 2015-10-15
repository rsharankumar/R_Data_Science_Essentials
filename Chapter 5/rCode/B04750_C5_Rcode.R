#### Data Science Essentials
#### Chapter 5 - Regtression Models
#### Author: Sharan Kumar R and Raja B Koushik

# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 5")
getwd()

# Data for Classification
worlddatac <- read.csv("data/worlddata_ForClassification.csv")
summary(worlddatac)
head(worlddatac)
#Formating the dataset
wcdata <- na.omit(worlddatac)
summary(wcdata)
names(wcdata)
wcdata <-  wcdata[ , -which(names(wcdata) %in% c("country"))]
head(wcdata)


# Data for Regression
worlddata <- read.csv("data/worlddata.csv")
summary(worlddata)
head(worlddata)
#Formating the daaset
wdata <- na.omit(worlddata)
summary(wdata)
wdata <-  wdata[ , -which(names(wdata) %in% c("country"))]
head(wdata)

##Logistic Model

###########
# divide into sample
training_positions <- sample(nrow(wcdata), size=floor((nrow(wcdata)*0.7)))

# Split into train and test based on the sample size
traindata<-wcdata[training_positions,]
testdata<-wcdata[-training_positions,]

nrow(traindata)
nrow(testdata)
###Building logistic regession model
model <- glm(as.factor(life_expectancy_morethan_70)~., traindata, family=binomial(link = "logit"))
summary(model)

# Predicting the output
prediction <- predict(model, testdata, type="response")

result <- cbind(testdata$life_expectancy_morethan_70, prediction)
head(result, 50)
write.csv(result, "result.csv")
# Evaluation of the model
result <- as.data.frame(result)
colnames(result) <- c("Actual","Prediction")
result$Predicted[result[2] > 0.7] <- 1
result$Predicted[result[2] <= 0.7] <- 0
result <-  result[ , -which(names(result) %in% c("Prediction"))]
head(result, 20)

xtab <- table(result$Predicted, result$Actual)

install.packages("caret")
library(caret)
confusionMatrix(xtab)
confusionMatrix(result$Predicted, result$Actual)
confusionMatrix(xtab, prevalence = 0.25) 



###Regression

# divide into sample
training_positions <- sample(nrow(wdata), size=floor((nrow(wdata)*0.7)))

# Split into train and test based on the sample size
traindata<-wdata[training_positions,]
testdata<-wdata[-training_positions,]

nrow(traindata)
nrow(testdata)


names(wdata)
model <- lm(life_expectancy~., traindata)
summary(model)

# perdicting
prediction <- predict(model, testdata)

result <- cbind(testdata$life_expectancy, prediction)

write.csv(result, "result.csv")

result <- as.data.frame(result)
colnames(result) <- c("Actual","Prediction")
head(result, 50)

#evaluvation for the regression - mean squared error
sqerr <- (result$Actual-result$Prediction)^2
meansqerr <- sum(sqerr)/nrow(result)
meansqerr

############
##replace NA with mean and median

worldd <- read.csv("data/worlddata.csv")
worldd <-  worldd[ , -which(names(worldd) %in% c("country"))]
worldd <- as.matrix(worldd)
head(worldd)
worldd[is.na(worldd)] <- median(worldd, na.rm=TRUE)
head(worldd)
worldd[is.na(worldd)] <- mean(worldd, na.rm=TRUE)
head(worldd)

# Removing the Outliers
install.packages("outliers")
library(outliers)
outlier_tf = outlier(worldd,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
worldd = worldd[-find_outlier,]
nrow(worldd)

head(worldd)

# Remove high correlated columns
ncol(worldd)
library(caret)
tooHigh <- findCorrelation(cor(worldd[,1:16]), .9)
worldd<-worldd[,-tooHigh]
ncol(worldd)
