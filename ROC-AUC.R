library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")
## Generate weight and obesity datasets.
set.seed(420) # this will make my results match yours
num.samples <- 100
## genereate 100 values from a normal distribution with
## mean 172 and standard deviation 29, then sort them
weight <- sort(rnorm(n=num.samples, mean=172, sd=29))
## Now we will decide if a sample is obese or not.
## NOTE: This method for classifying a sample as obese or not
## was made up just for this example.
## rank(weight) returns 1 for the lightest, 2 for the second lightest, ...
##              ... and it returns 100 for the heaviest.
## So what we do is generate a random number between 0 and 1. Then we see if
## that number is less than rank/100. So, for the lightest sample, rank = 1.
## This sample will be classified "obese" if we get a random number less than
## 1/100. For the second lightest sample, rank = 2, we get another random
## number between 0 and 1 and classify this sample "obese" if that random
## number is < 2/100. We repeat that process for all 100 samples
obese <- ifelse(test=(runif(n=num.samples) < (rank(weight)/num.samples)),
                yes=1, no=0)
obese ## print out the contents of "obese" to show us which samples were
## classified "obese" with 1, and which samples were classified
## "not obese" with 0.
## plot the data
plot(x=weight, y=obese)
## fit a logistic regression to the data...
glm.fit=glm(obese ~ weight, family=binomial)
lines(weight, glm.fit$fitted.values)
## draw ROC and AUC using pROC
## NOTE: By default, the graphs come out looking terrible
## The problem is that ROC graphs should be square, since the x and y axes
## both go from 0 to 1. However, the window in which I draw them isn't square
## so extra whitespace is added to pad the sides.
roc(obese, glm.fit$fitted.values, plot=TRUE)
## Now let's configure R so that it prints the graph as a square.
##
par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region
roc(obese, glm.fit$fitted.values, plot=TRUE)
## NOTE: By default, roc() uses specificity on the x-axis and the values range
## from 1 to 0. This makes the graph look like what we would expect, but the
## x-axis itself might induce a headache. To use 1-specificity (i.e. the
## False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE)
## If you want to rename the x and y axes...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")
## We can also change the color of the ROC line, and make it wider...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)
## If we want to find out the optimal threshold we can store the
## data used to make the ROC graph in a variable...
roc.info <- roc(obese, glm.fit$fitted.values, legacy.axes=TRUE)
str(roc.info)
## and then extract just the information that we want from that variable.
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
## We can calculate the area under the curve...
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
## ...and the partial area under the curve.
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")
## Now let's fit the data with a random forest...
rf.model <- randomForest(factor(obese) ~ weight)
## ROC for random forest
roc(obese, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)
## Now layer logistic regression and random forest ROC graphs..
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)
## Now that we're done with our ROC fun, let's reset the par() variables.
## There are two ways to do it...
par(pty = "m")

#2
library(randomForest)
library(pROC)
# generate some random data
set.seed(1111)
train <- data.frame(condition = sample(c("mock", "lethal", "resist"), replace = T, size = 1000))
train$feat01 <- sapply(train$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
train$feat02 <- sapply(train$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
train$feat03 <- sapply(train$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
head(train)

test <- data.frame(condition = sample(c("mock", "lethal", "resist"), replace = T, size = 1000))
test$feat01 <- sapply(test$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
test$feat02 <- sapply(test$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
test$feat03 <- sapply(test$condition, (function(i){ if (i == "mock") { rnorm(n = 1, mean = 0)} else if (i == "lethal") { rnorm(n = 1, mean = 1.5)} else { rnorm(n = 1, mean = -1.5)} }))
head(test)
model <- randomForest(formula = condition ~ ., data = train, ntree = 10, maxnodes= 100, norm.votes = F)
# predict test set, get probs instead of response
predictions <- as.data.frame(predict(model, test, type = "prob"))
predictions$predict <- names(predictions)[1:3][apply(predictions[,1:3], 1, which.max)]
predictions$observed <- test$condition
head(predictions)
# 1 ROC curve, mock vs non mock
roc.mock <- roc(ifelse(predictions$observed=="mock", "mock", "non-mock"), as.numeric(predictions$mock))
plot(roc.mock, col = "gray60")

# others
roc.lethal <- roc(ifelse(predictions$observed=="lethal", "lethal", "non-lethal"), as.numeric(predictions$mock))
roc.resist <- roc(ifelse(predictions$observed=="resist", "resist", "non-resist"), as.numeric(predictions$mock))
lines(roc.lethal, col = "blue")
lines(roc.resist, col = "red")

# 3
data(aSAH)
rocobj1 <- roc(aSAH$outcome, aSAH$s100b)
rocobj2 <- roc(aSAH$outcome, aSAH$wfns)
rocobj3 <- roc(aSAH$outcome, aSAH$ndka)
auc(rocobj1)
auc(rocobj2)
auc(rocobj3)
#绘制曲线
plot(rocobj1)
#其他参数美化（自定义网络线颜色等等）
plot(rocobj1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)
g3 <- ggroc(list(s100b=rocobj, wfns=rocobj2, ndka= rocobj3))