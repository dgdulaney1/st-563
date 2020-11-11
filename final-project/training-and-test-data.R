red <- read.csv("red_wine.csv")

set.seed(1)
train <- sample(dim(red)[1], dim(red)[1]/2)
red.train <- red[train,]
red.test <- red[-train,]
write.csv(red.train, "red_wine_train.csv", quote=FALSE)
write.csv(red.test, "red_wine_test.csv", quote=FALSE)
