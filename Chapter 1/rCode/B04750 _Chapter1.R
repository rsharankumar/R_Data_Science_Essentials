# Chapter 1
#### Author: Sharan Kumar R and Raja B Koushik
setwd("C:\\Users\\sharan\\Desktop")


# Read data from R
data  <- read.csv("local-data.csv")

# Other methods
data  <- read.delim("local-data.txt", header=TRUE, sep="\t")
data  <- read.table("local-data.txt", header=TRUE, sep="\t")

# with few parameter
mydata <- read.table("c:/mydata.csv", header=TRUE, sep=",", row.names="id")

### read data from excel
install.packages("xlsx")
library(xlsx)
mydata <- read.xlsx("DTH AnalysisV1.xlsx", 1)
head(mydata)

# reading data from db
install.packages("RJDBC")
library(RJDBC)
install.packages("sqldf")
library(sqldf)

#### CONNECT TO DATABASE 
drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", "C:/Users/Downloads/Microsoft SQL Server JDBC Driver 3.0/sqljdbc_3.0/enu/sqljdbc4.jar")
conn <- dbConnect(drv, "jdbc:sqlserver://localhost;database=SAMPLE_DB", "admin", "test")

### Sample query
bin <- dbGetQuery(conn, "select count(*) from  sampletable")

# From Cassandra

library(RCassandra)
conn<-RC.connect(host = "localhost", port = 9160)
RC.login(conn, username = "user", password = "password123")
RC.describe.keyspaces(conn)
RC.use(conn, keyspace="sampleDB", cache.def = TRUE)
a<-RC.read.table(conn, c.family="Users", convert = TRUE, na.strings = "NA", as.is = FALSE, dec = ".")
RC.close(conn)

#### Data type

# basic Data type

a <- 10
class(a)

# Integer
a <- as.integer(a)
class(a)

# character

name <- "Sharan"
class(name)

# Logiccal
flag <- TRUE
class(flag)

# Vectors:

v1 <- c(12, 34, -21, 34.5, 100) # numeric vector
class(v1)
v2 <- c("sam","paul","steve", "mark") # character vector
class(v2)
v3 <- c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE) #logical vector
class(v3)


newV <- c(v1,v2)
class(newV)
newV


# matrix
rnames <- c("R1","R2","R3","R4","R5")
cnames <- c("C1","C2","C3","C4","C5")
matdata <-matrix(1:25, nrow=5,ncol=5, dimnames=list(rnames, cnames))
class(matdata)
typeof(matdata)
matdata

# list

l1 <- list(v1, v2, v3)
typeof(l1)


# data frame

data <- read.csv("IndiaData.csv", header =TRUE)
length(data)
nrow(data)
head(data)
#1. how many Null
sum(is.na(data))

# replacing with the mean value
for (i in which(sapply(data, is.numeric))) {
  data[is.na(data[, i]), i] <- mean(data[, i],  na.rm = TRUE)
}


newdata <- na.omit(data)
nrow(newdata)
head(newdata)

#2. Is there Outliers? Should i remove the outliers?
install.packages("outliers")
library(outliers)

outlier_tf = outlier(data$X2012,logical=TRUE)
sum(outlier_tf)
#What were the outliers
find_outlier = which(outlier_tf==TRUE,arr.ind=TRUE)
#Removing the outliers
newdata = data[-find_outlier,]
nrow(newdata)


## Arithmetic Operation

a1 <- c(1,2,3,4,5)
b1 <- c(6,7,8,9,10)
c1 <- a1+b1

c1 <- b1-a1

c1 <- b1*a1

c1 <- b1/a1

c1 <- b1 ^ a1

# modulus
c1 <- b1 %% a1 


# Logical operator

x <- c(1:10)
x[(x>=8) | (x<=5)]

# matrix multiplication

# matrix
rnames <- c("R1","R2","R3","R4","R5")
cnames <- c("C1","C2","C3","C4","C5")
matdata1 <-matrix(1:25, nrow=5,ncol=5, dimnames=list(rnames, cnames))
matdata2 <-matrix(1:25, nrow=5,ncol=5, dimnames=list(rnames, cnames))
newmat <- matdata1 * matdata2
newmat



## string operations

# substring
x <- " The Shawshank Redemption" 
substr(x, 6, 14)

# pattern matching
grep("Shawshank", c("The","Shawshank","Redemption"), fixed=TRUE)


# replace matching chracter

sub("\\s",",","Hello There")


# string split
strsplit("Redemption", "") 

# paste
paste("Today is", date())


toupper(x)
tolower(x)


# aggregation

# mean
data <- mtcars
head(data)
mean(data$mpg)

# median

med <- median(data$mpg)
paste("Median MPG:", med)

# sum

hp <- sum(data$hp)
paste("Total HP:", hp)

# maximum

max <- max(data$mpg)
min <- min(data$mpg)
paste("Maximum MPG:", max, "and Minimum MPG:", min)


# SD

sd <- sd(data$mpg)
paste("Std Deviation of MPG:", sd)


# Final preocessing
# column selection
head(data)
newdata <- data[c(1,5:10)]
head(newdata)


# excluding column 
newdata <- data[c(-3,-5)]


# selecting rows
newdata <- data[ which(data$mpg > 25), ]
head(newdata)
# random sample
sample <- data[sample(1:nrow(data), 10, replace=FALSE),]
nrow(sample)



# merging
sample1 <- data[sample(1:nrow(data), 10, replace=FALSE),]
sample2 <- data[sample(1:nrow(data), 5, replace=FALSE),]

newdata <- rbind(sample1, sample2)
head(newdata)

# cbind

newdata1 <- data[c(1,5:7)]
newdata2 <- data[c(8:11)]
newdata <- cbind(newdata1, newdata2)
