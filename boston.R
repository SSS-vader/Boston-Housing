# Boston Housing
boston = read.csv("boston.csv")
str(boston)

plot(boston$LON, boston$LAT)
# Show charles river properties in solid blue dots on same plot
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch = 19)
points(boston$LON[boston$TRACT==3531], boston$LAT[boston$TRACT==3531], col="red", pch=19)

summary(boston$NOX)
points(boston$LON[boston$NOX >= 0.55], boston$LAT[boston$NOX >= 0.55], col = "green", pch = 19)

plot(boston$LON, boston$LAT)
summary(boston$MEDV)

points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

# Trying linear regression for house prices based on latitude and longitude
latlonlm = lm(MEDV ~ LAT + LON, data = boston)
summary(latlonlm)

# Revisiting the above median value properties
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

# Seeing what latlonlm model sees as above median houses
points(boston$LON[latlonlm$fitted.values>=21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col = "blue", pch="$")


# REGRESSIPN TREES
library(rpart)
library(rpart.plot)

latlontree = rpart(MEDV ~ LAT + LON, data = boston)
prp(latlontree)

# Again see lat and lon plots and what the tree thinks
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues >= 21.2], boston$LAT[fittedvalues >= 21.2], col = "blue", pch = "$")

# Tree was too complicated, now minbucket size put 50
latlontree = rpart(MEDV ~ LAT + LON, data = boston, minbucket = 50)
plot(latlontree)
text(latlontree)

# Making rectangular sections as seen in tree to visualize cheapest price neighbourhood
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV >= 21.2], boston$LAT[boston$MEDV >= 21.2], col = "red", pch = 19)
abline(v = -71.07)
abline(h = 42.21)
abline(h = 42.17)

# SPLITTING TEST TRAIN
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split == TRUE)
test = subset(boston, split == FALSE)

linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
summary(linreg)

linreg.pred = predict(linreg, newdata = test)
linreg.sse = sum((linreg.pred - test$MEDV)^2)
linreg.sse

# CAN WE BEAT THE SSE USING REGRESSION TREE?
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train)
prp(tree)
tree.pred = predict(tree, newdata = test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse

# ONE LAST TREE USING CROSS VALIDATION
library(caret)
library(e1071)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)

tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

tr

best.tree = tr$finalModel
prp(best.tree)
cvtree.pred = predict(best.tree, newdata = test)
cvtree.sse = sum((cvtree.pred - test$MEDV)^2)
cvtree.sse