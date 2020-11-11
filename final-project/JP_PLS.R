library(pls)

# fit PLS model on training data
set.seed(1)
pls.fit <- plsr(quality~., data=red.train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP", type="b")

# a near-minimum cross-validation MSE at M=2; could also have chosen 1

# predict for test set and calculate test MSE
pls.pred <- predict(pls.fit, red.test, ncomp=2)
test.mse <- mean((pls.pred-red.test$quality)^2)
test.mse

# fit PLS on the full data set
pls.fit <- plsr(quality~., data=red, scale=TRUE, ncomp=2)
summary(pls.fit)