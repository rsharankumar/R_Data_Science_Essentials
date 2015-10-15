# Data Science Essential - Chapter 2
#### Author: Sharan Kumar R and Raja B Koushik
setwd("C:\\Users\\Sharan\\Desktop\\R Data Science Essentials\\Chapter 2\\Data")
getwd()


tdata <- read.csv("titanic.csv")
names(tdata)
head(tdata)


# Descriptive Statistics

summary(tdata)

install.packages("Hmisc")
library(Hmisc)
describe(tdata)


# stanard deviation
sd(tdata$Fare)

# percentile
quantile(tdata$Fare)
quantile(tdata$Fare, probs = seq(0, 1, 0.1))

# Box plot
bplot <- tdata[c("Pclass", "Age", "SibSp", "Parch", "Fare", "Cabin")]
boxplot(bplot, outline = FALSE, col = "blue")
?boxplot
dev.copy(png,filename="boxplot.png", width=600, height=600);
dev.off ();

# Inferential
install.packages("lsr")
library(lsr)
ciMean(tdata$Fare)

# bi variables
names(tdata)
aggregate( tdata$Fare ~ tdata$Sex, tdata, ciMean )


# t test
t.test( tdata$Fare ~ tdata$Sex, data = tdata )


# Corrrelation
cor(tdata$Fare, tdata$Age, use="complete")

cor.test(tdata$Fare, tdata$Age, use="complete")

## Uni variate analysis

## Histogram
age <- na.omit(tdata$Age)
range(age)
breaks = seq(0, 80, by=10)

ageBreak = cut(age, breaks, right=FALSE)
ageDistribution = table(ageBreak)
ageDistribution <- data.frame(ageDistribution)

# histogram on the frequency distribution
library(ggplot2)
names(tdata)
ageDistribution
colnames(ageDistribution) <- c("Range","No.Of.People")
q <- qplot(x=Range, y=No.Of.People, 
           data=ageDistribution, geom="bar", stat="identity",
           position="dodge")
q + geom_bar(stat="identity", fill="#0000FF", colour="black")
ggsave(file="Distribution.png", dpi=500)



## Pie chart
names(tdata)
pieChart <- ggplot(tdata, aes(x = factor(1), fill = factor(tdata$Sex))) + geom_bar(width = 1) 
pieChart + coord_polar(theta = "y") +
ggtitle("Male and female")


ggsave(file="pie-chart-gender.png", dpi=500)


# Scatter Plot

ggplot(tdata, aes(x=Fare, y=Age)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm)
ggsave(file="Age-Fare-Chart.png", dpi=500)

# correlation
cor(tdata$Age, tdata$Fare, use= "pairwise.complete.obs")


# Cross tabulation
names(tdata)

# Two variables
tab<-xtabs(~Survived+Sex, data=tdata)
ftable(tab)
result <- replace(tab, , sprintf("%.1f%%",prop.table(tab,2)*100))
result

## Multi- variate Analysis
# Three variables
tab<-xtabs(~Survived+Sex+SibSp, data=tdata)
ftable(tab)
result <- replace(tab, , sprintf("%.1f%%",prop.table(tab,3)*100))
data.frame(result)

names(tdata)
head(tdata)

# Trend Analysis

library(sqldf)

trendData1 <- sqldf("select Age, count(PassengerId) from tdata where Survived in ('1') group by Age")
colnames(trendData1) <- c("Age","Survived")
trendData2 <- sqldf("select Age, count(PassengerId) from tdata where Survived in ('0') group by Age")
colnames(trendData2) <- c("Age","NotSurvived")
trendData <- sqldf("select a.Age, Survived, NotSurvived from trendData1 a inner join trendData2 b on a.Age = b.Age")
head(trendData)
library(reshape)
combinedData <- melt(trendData, id = 'Age')

ggplot(meltedJoinsByWeek, aes(x = Age, y = value, colour = variable)) + 
  geom_line() + 
  ylab(label="Survival and Death") + 
  xlab("Age of the passenger") + 
  scale_colour_manual(values=c("green", "red"))

ggsave(file="Survival_on_Age.png", dpi=500)


#####################3