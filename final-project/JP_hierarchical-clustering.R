# get the scaled features
red.features.sc <- scale(subset(red, select=-quality))

# run hierarchical clustering with 3 types of linkage
# based on euclidean distance
hc.complete <- hclust(dist(red.features.sc), method="complete")
hc.average <- hclust(dist(red.features.sc), method="average")
hc.single <- hclust(dist(red.features.sc), method="single")

# create plots
plot(hc.complete, main="Complete Linkage", xlab="", ylab="", sub="", cex=0.9)
plot(hc.average, main="Average Linkage", xlab="", ylab="", cex=0.9)
plot(hc.single, main="Single Linkage", xlab="", ylab="", cex=0.9)

# create table
table(cutree(hc.complete,25), red$quality)
