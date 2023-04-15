#################### Kmeans:
  data(iris)
# Installing Packages
install.packages("ClusterR")
install.packages("cluster")
# Loading package
library(ClusterR)
library(cluster)
# Removing initial label of
# Species from original dataset
iris_1 <- iris[, -5]
# Fitting K-Means clustering Model
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re
# Cluster identification for
# each observation
kmeans.re$cluster
# Confusion Matrix
cm <- table(iris$Species, kmeans.re$cluster)
cm
# Model Evaluation and visualization
plot(iris_1[c("Sepal.Length", "Sepal.Width")])
plot(iris_1[c("Sepal.Length", "Sepal.Width")],
     col = kmeans.re$cluster)
plot(iris_1[c("Sepal.Length", "Sepal.Width")],
     col = kmeans.re$cluster,
     main = "K-means with 3 clusters")
## Plotiing cluster centers
kmeans.re$centers
kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")]
# cex is font size, pch is symbol
points(kmeans.re$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 3)
## Visualizing clusters
y_kmeans <- kmeans.re$cluster
clusplot(iris_1[, c("Sepal.Length", "Sepal.Width")],y_kmeans,lines = 0,shade = TRUE,color = TRUE,labels =
           2,plotchar = FALSE,span = TRUE,main = paste("Cluster iris"),xlab = 'Sepal.Length',ylab = 'Sepal.Width')


### Kmedoids
######################################### k-Medoids
library(cluster)
df <- USArrests
#remove rows with missing values
df <- na.omit(df)
df <- scale(df)
head(df)
#perform k-medoids clustering with k = 4 clusters
kmed <- pam(df, k = 4)
kmed
library("factoextra")
fviz_cluster(kmed, data = df)


### Hierarchieal
#################### Hierarchieal
library(dplyr)
# Finding distance matrix
distance_mat <- dist(iris, method = 'euclidean')
distance_mat
# Fitting Hierarchical clustering Model
set.seed(240) # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl
# Plotting dendrogram
plot(Hierar_cl)
# Choosing no. of clusters
abline(h = 110, col = "green")
# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )
fit
table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")

