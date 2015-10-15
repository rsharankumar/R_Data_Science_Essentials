######## Data Science Essentials
# Chapter 3
#### Author: Sharan Kumar R and Raja B Koushik
library(arules)
library(arulesSequences)

# Dataset

data("AdultUCI")
head(AdultUCI)
class(AdultUCI)

# Remove the numerical attributes
AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL


AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))
AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]], c(0,25,40,60,168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic")) 
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capital-gain"]]>0]), Inf)), labels = c("None", "Low", "High")) 
AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]], c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capital-loss"]]>0]), Inf)), labels = c("None", "Low", "High")) 
head(AdultUCI)
## create transactions 
Adult <- as(AdultUCI, "transactions") 
class(Adult)
summary(Adult)
Adultdf <- as(Adult, "data.frame")
head(Adultdf, 3)

# transactional dataset
data("Adult")
head(Adult)
as(Adult, "data.frame")


#Other dataset
setwd("C:\\Users\\Sharan\\Desktop\\R Data Science Essentials\\Chapter 3\\Data")
getwd()

sampdata = read.transactions(file="following.csv", rm.duplicates= FALSE, format="single",sep=",",cols =c(1,2));
summary(sampdata)

## Apriori Analysis

rules1 <- apriori(Adult,parameter = list(sup = 0.5, conf = 0.9,target="rules"));
inspect(rules1);

rules2 <- apriori(sampdata,parameter = list(sup = 0.45, conf = 0.9, target="rules"));
inspect(rules2);

###filter rules

inspect(head(sort(rules2, by="lift"),10))

head(quality(rules1));
head(quality(rules2));


## plotting rules
install.packages("arulesViz")
library(arulesViz)
image(Adult)
image(sampdata)
dev.copy(png,filename="sampdata.png", width=400, height=575);
dev.off ();

plot(rules1)
dev.copy(png,filename="rules1-a.png", width=500, height=500);
dev.off ();

plot(rules1, measure=c("support","lift"), shading="confidence")
dev.copy(png,filename="rules1-b.png", width=500, height=500);
dev.off ();


# Data for sequence analysis
data(zaki)
summary(zaki)
as(zaki, "data.frame")

trans_data <- read_baskets(con = "sample.txt", info = c("sequenceID","eventID","SIZE"))
summary(zaki)
seq_rules <- cspade(zaki, parameter = list(support = 0.55), control   = list(verbose=TRUE))
#Summary information about the rules generated
summary(seq_rules)

#The rules generated with the support details
as(seq_rules, "data.frame")


