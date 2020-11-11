library(pls)

# fit PCR model on training data
set.seed(1)
pcr.fit <- pcr(quality~., data=red.train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP", type="b")

# choose to use 3 components because the cross-validation MSE is nearly as low
# as at higher values

# predict for test set and calculate test MSE
pcr.pred <- predict(pcr.fit, red.test, ncomp=3)
test.mse <- mean((pcr.pred - red.test$quality)^2)
test.mse

# fit PCR on the full data set
pcr.fit <- pcr(quality~., data=red, scale=TRUE, ncomp=3)
summary(pcr.fit)