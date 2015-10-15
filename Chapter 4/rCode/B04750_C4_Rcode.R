#### Data Science Essentials
#### Chapter 4 - Segmentation Using Clustering
#### Author: Sharan Kumar R and Raja B Koushik

# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 4")
getwd()


worlddata <- read.csv("data/worlddata.csv")
summary(worlddata)
head(worlddata)

#Formating the dataset
wdata <- na.omit(worlddata)
summary(wdata)

# Removing the column country
wdata <-  wdata[ , -which(names(wdata) %in% c("country"))]


wdata <- data.matrix(wdata)
wdata <- scale(wdata)
head(wdata)

# Clustering
install.packages("fpc")
library(fpc)

##
wdata

# Ideal number of clusters
head(wdata)
clusters<- pamk(wdata)
n<-clusters$nc
n

####
install.packages("NbClust")
library(NbClust)
NbClust(data = wdata, distance = "euclidean", min.nc = 2, max.nc = 20,
        method = "average", index = "all", alphaBeale = 0.1)


# Determine number of clusters
#Code from Tal Galili's post based on Kabacoff's book - http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/ 
wss <- (nrow(wdata)-1)*sum(apply(wdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(wdata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

dev.copy(png,filename="IdealClusters.png", width=500, height=500);
dev.off ();

head(wdata)
## k means
# K-Means Cluster Analysis
fit <- kmeans(wdata, n)
#Number of elements in each clusters
table(fit$cluster)
# get cluster means 
aggregate(wdata,by=list(fit$cluster),FUN=mean)
# plotting cluster
?plotcluster
plotcluster(wdata, fit$cluster)
dev.copy(png,filename="Plot.png", width=500, height=500);
dev.off ();

# better plotting
library(cluster) 
clusplot(wdata, fit$cluster, color=TRUE, shade=TRUE, labels=1, lines=0)
?clusplot
dev.copy(png,filename="plotNew.png", width=500, height=500);
dev.off ();


##################Hierarchical clustering


distmeasure <- dist(wdata, method = "manhattan")

cluster <- hclust(distmeasure, method="ward.D2") 

plot(cluster, cex=0.5, cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1, which.plots=2) # display dendogram
dev.copy(png,filename="hclust.png", width=500, height=500);
dev.off ();

 
rect.hclust(cluster, k=5, border="red")
dev.copy(png,filename="hclustGroups.png", width=500, height=500);
dev.off ();

##############

# Model Based Clustering
library(mclust)
mclusters <- Mclust(wdata)
summary(mclusters)


#############
### Density based clustering
dbscluster <- dbscan(wdata, eps=3, MinPts=15)
dbscluster
plot(dbscluster, col=dbscluster$cluster+1L)
?dbscan
