# run PCA
red.features <- subset(red, select=-quality)
pr.out <- prcomp(red.features, scale=TRUE)
summary(pr.out)

# plot the variance explained by the first few principal components
par(mfrow=c(1,1))
plot(pr.out)

# plot the first two principal components
par(mfrow=c(1,1))
biplot(pr.out, scale=0)

# calculate the proportion of variance explained by each principal component
pr.var <- pr.out$sdev^2
pve <- pr.var / sum(pr.var)

# plot the proportion of variance explained by each principal component and
# the cumulative proportion of variance explained by the principal components
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="PVE", ylim=c(0,1), type="b", col="blue")
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative PVE", ylim=c(0,1),
     type="b", col="green")

# plot the projection onto the first three principal components, with points
# colored by quality
Cols <- function(vec){
  cols <- rainbow(length(unique(sort(vec))))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(red$quality), pch=19, cex=0.25, xlab="PC1", ylab="PC2")
legend(6, 6, legend=c(unique(sort(red$quality))),
       col=rainbow(length(unique(red$quality))), pch=19, cex=0.8)
plot(pr.out$x[,c(1,3)], col=Cols(red$quality), pch=19, cex=0.25, xlab="PC1", ylab="PC3")
legend(6, 3, legend=c(unique(sort(red$quality))),
       col=rainbow(length(unique(red$quality))), pch=19, cex=0.8)
