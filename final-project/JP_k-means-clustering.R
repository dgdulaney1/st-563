# run k-means clustering with several different values of K
red.features.sc <- scale(subset(red, select=-quality))
set.seed(1)
km.2.out <- kmeans(red.features.sc, 2, nstart=50)
table(km.2.out$cluster, red$quality)

set.seed(1)
km.3.out <- kmeans(red.features.sc, 3, nstart=50)
table(km.3.out$cluster, red$quality)

set.seed(1)
km.4.out <- kmeans(red.features.sc, 4, nstart=50)
table(km.4.out$cluster, red$quality)

set.seed(1)
km.5.out <- kmeans(red.features.sc, 5, nstart=50)
table(km.5.out$cluster, red$quality)

set.seed(1)
km.6.out <- kmeans(red.features.sc, 6, nstart=50)
table(km.6.out$cluster, red$quality)

set.seed(1)
km.7.out <- kmeans(red.features.sc, 7, nstart=50)
table(km.7.out$cluster, red$quality)

set.seed(1)
km.8.out <- kmeans(red.features.sc, 8, nstart=50)
table(km.8.out$cluster, red$quality)

set.seed(1)
km.9.out <- kmeans(red.features.sc, 9, nstart=50)
table(km.9.out$cluster, red$quality)

set.seed(1)
km.10.out <- kmeans(red.features.sc, 10, nstart=50)
table(km.10.out$cluster, red$quality)

#plot each of the k-means clusters on the PCA axes
#did not include these in the report, since the table of kmeans cluster v. quality
#seemed to better indicate the performance of kmeans
pr.out <- prcomp(red.features, scale=TRUE)
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=km.2.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC2")
plot(pr.out$x[,c(1,3)], col=km.2.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC3")

plot(pr.out$x[,1:2], col=km.3.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC2")
plot(pr.out$x[,c(1,3)], col=km.3.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC3")

plot(pr.out$x[,1:2], col=km.4.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC2")
plot(pr.out$x[,c(1,3)], col=km.4.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC3")

plot(pr.out$x[,1:2], col=km.5.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC2")
plot(pr.out$x[,c(1,3)], col=km.5.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC3")

plot(pr.out$x[,1:2], col=km.6.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC2")
plot(pr.out$x[,c(1,3)], col=km.6.out$cluster+1, pch=19, cex=0.25, xlab="PC1", ylab="PC3")


# transpose the data and cluster
red.features.t.sc <- scale(t(red.features))
set.seed(1)
km.2t.out <- kmeans(red.features.t.sc, 2, nstart=50)
km.2t.out$cluster

set.seed(1)
km.3t.out <- kmeans(red.features.t.sc, 3, nstart=50)
km.3t.out$cluster

set.seed(1)
km.4t.out <- kmeans(red.features.t.sc, 4, nstart=50)
km.4t.out$cluster

set.seed(1)
km.5t.out <- kmeans(red.features.t.sc, 5, nstart=50)
km.5t.out$cluster

set.seed(1)
km.6t.out <- kmeans(red.features.t.sc, 6, nstart=50)
km.6t.out$cluster

set.seed(1)
km.7t.out <- kmeans(red.features.t.sc, 7, nstart=50)
km.7t.out$cluster

set.seed(1)
km.8t.out <- kmeans(red.features.t.sc, 8, nstart=50)
km.8t.out$cluster

set.seed(1)
km.9t.out <- kmeans(red.features.t.sc, 9, nstart=50)
km.9t.out$cluster

set.seed(1)
km.10t.out <- kmeans(red.features.t.sc, 10, nstart=50)
km.10t.out$cluster