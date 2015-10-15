#### Data Science Essentials
#### Chapter 7 - Recommendation Engine
#### Author: Sharan Kumar R and Raja B Koushik

# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 7")
getwd()


# reading the dataset
rdata <- read.csv("Data/following.csv")
head(rdata, 10)



# Pivoting the data
library(data.table)
pivoting <- data.table(rdata)
pivotdata<-dcast.data.table(pivoting, Items ~ UserID, fun.aggregate=length, value.var="UserID")

head(pivotdata)
colnames(pivotdata)

write.csv(pivotdata, "Data/pivot-follows.csv")

# After deletion of the index column and the null user

# Read the pivoted data
ubs<-read.csv("Data/pivot-follows.csv")
head(ubs)
colnames(ubs)

# Function to calculate the cosine between two vectors
getCosine <- function(x,y) 
{
  dat <- cbind(x,y)
  #f <- as.matrix(dat)
  f <- as.data.frame(dat)
  # Remove the rows with zeros
  datn<- f[-which(rowSums(f==0)>0),]
  #colnames(datn)<-c("x","y")
  #dat <- as.data.frame(datn)
  if(nrow(datn) > 2)
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  }
  else
  {
    this.cosine <- 0
  }
  return(this.cosine)
}



# Create a placeholder dataframe listing item vs. item
ubs.score  <- matrix(NA, nrow=ncol(ubs),ncol=ncol(ubs),dimnames=list(colnames(ubs),colnames(ubs)))

############ Perform any 1 #####################################

# Method: 1
# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(ubs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(ubs)) {
    # Fill in placeholder with cosine similarities
    ubs.score[i,j] <- getCosine(data.matrix(ubs[i]),data.matrix(ubs[j]))
  }
  print(i)
}


# Back to dataframe - Similarity matrix
ubs.score <- as.data.frame(ubs.score)
head(ubs.score)


# Get the top 10 neighbours for each
user.neighbours <- matrix(NA, nrow=ncol(ubs.score),ncol=11,dimnames=list(colnames(ubs.score)))
for(i in 1:ncol(ubs)) 
{
  # Setting threshold for avoiding zeros
  n <- length(ubs.score[,i])
  thres <- sort(ubs.score[,i],partial=n-10)[n-10]
  if(thres > 0.10)
  {
    # Choosing the top 10 recommendation
    user.neighbours[i,] <- (t(head(n=11,rownames(ubs.score[order(ubs.score[,i],decreasing=TRUE),][i]))))
  }
  else
  {
    user.neighbours[i,] <- ""
  }
}
head(user.neighbours, 10)

# Writing the recommendation to a file
write.csv(user.neighbours, "Data/SimilarUsersresults.csv")


allrec <- ""
# getting the item to recommend
for(i in 1:nrow(user.neighbours)) 
{
  # Setting threshold for avoiding zeros
  for (j in 2:3)
  {
    n <- user.neighbours[i,j]
    new <- subset(rdata, UserID == n)
    usr <- paste0("user", i)
    rec <- cbind(usr,data.frame(new$Items))
    allrec <- rbind(allrec,rec)
  }
  print (i)
}
allrec <- allrec[complete.cases(allrec),]
colnames(allrec) <- c("UserID","Items")
head(allrec, 10)

# recommendation to the users 
install.packages("sqldf")
require(sqldf)
newItems <- sqldf('SELECT * FROM allrec EXCEPT SELECT * FROM rdata')
head(newItems, 10)

#### Item based collaborative Filtering method

library(data.table)
pivoting <- data.table(rdata)
pivotdataItem<-dcast.data.table(pivoting, UserID ~ Items, fun.aggregate=length, value.var="Items")
head(pivotdataItem)
colnames(pivotdataItem)
write.csv(pivotdataItem, "Data/pivot-followsItem.csv")


ibs<-read.csv("Data/pivot-followsItem.csv")
head(ibs)
colnames(ibs)

# Create a placeholder dataframe listing item vs. item
ibs.score  <- matrix(NA, nrow=ncol(ibs),ncol=ncol(ibs),dimnames=list(colnames(ibs),colnames(ibs)))

# Lets fill in those empty spaces with cosine similarities
# Loop through the columns
for(i in 1:ncol(ibs)) {
  # Loop through the columns for each column
  for(j in 1:ncol(ibs)) {
    # Fill in placeholder with cosine similarities
    ibs.score[i,j] <- getCosine(data.matrix(ibs[i]),data.matrix(ibs[j]))
  }
  print(i)
}


# Back to dataframe - Similarity matrix
ibs.score <- as.data.frame(ibs.score)
head(ibs.score)

# similar Items
item.neighbours <- matrix(NA, nrow=ncol(ibs.score),ncol=11,dimnames=list(colnames(ibs.score)))
for(i in 1:ncol(ubs)) 
{
  # Setting threshold for avoiding zeros
  n <- length(ibs.score[,i])
  thres <- sort(ibs.score[,i],partial=n-10)[n-10]
  if(thres > 0.10)
  {
    # Choosing the top 10 recommendation
    item.neighbours[i,] <- (t(head(n=11,rownames(ibs.score[order(ibs.score[,i],decreasing=TRUE),][i]))))
  }
  else
  {
    item.neighbours[i,] <- ""
  }
}
head(item.neighbours, 10)

