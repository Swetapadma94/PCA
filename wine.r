install.packages("readr")
library(readr)
wine<-read.csv("E:\\Assignment\\pca\\wine.csv")
View(wine)
attach(wine)
cor(wine)
# Model Building#
pca<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL )
summary(pca)
str(pca)
plot(pca) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)
biplot(pca)
pca$loadings ## coefficient
pca$scores[,1:3]
# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
wine<-cbind(wine,pca$scores[,1:3])
View(wine)
# preparing data for clustering (considering only pca scores as they represent the entire data)
clust_data<-wine[,15:17]
# Normalizing the data
norm<-scale(clust_data) # Scale function is used to normalize data
dist<-dist(norm,method = "euclidean" )
dist
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit<-hclust(dist,method = "complete")
plot(fit) # Displaying Dendrogram
groups<-cutree(fit,3)
final<-cbind(wine,groups)
View(final)
View(aggregate(final[,-c(2,9:11)],by=list(groups),FUN=mean))
#  Inferences can be drawn from the aggregate of the universities data on groups
#####

km<-kmeans(wine,3)
str(km)
library(animation)
km=kmeans.ani(wine,3)
km$centers
km$cluster
#elbow curve & k ~ sqrt(n/2) to decide the k value
kms<-(nrow(norm)-1)*sum(apply(norm,2,var))
for (i in 2:17)  kms[i] = sum(kmeans(norm, centers=i)$withinss)
plot(1:17, kms, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Screew-Plot")
### 3 optimum clusters###
# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xds <- rbind(cbind(rnorm(5000, 0, 8), rnorm(5000, 0, 8)), cbind(rnorm(5000, 50, 8), rnorm(5000, 50, 8)))
xcl <- clara(xds, 3, sample = 100)
clusplot(xcl)
xcl <- clara(xds, 3, sample = 100)
clusplot(xcl)
### 3 optimum clusters#######
#Partitioning around medoids
xpm <- pam(xds, 3)
clusplot(xpm)
