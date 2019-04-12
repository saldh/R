library(doBy)
summaryBy(iris)
library(ggplot2)
library(sqldf)

# ROCR示例
library(ROCR)
set.seed(137)
probs <- runif(100)
labels <- as.factor(ifelse(probs > .5 & runif(100) < .4,'A','B'))
pred <- prediction(probs,labels)
plot(performance(pred,'tpr','fpr'))

# caret包交叉检验（10 fold）
library(caret)
library('lattice','ggplot2')
(parts <- createDataPartition(iris$Species,p=0.8))
table(iris[parts$Resample1,'Species'])
createFolds(iris$Species,k=10)

# glm 逻辑回归训练分类器
d <- subset(iris,Species == 'virginica' | Species == 'versicolor')
str(d)
d$Species <- factor(d$Species)
str(d)
(m<-glm(Species ~ .,data = d,family = 'binomial'))
fitted(m)[c(1:15,51:65)]
predict(m,newdata = d[c(1,10,55),],type = 'response')
pre <- prediction(d,m)

# iris随机森林分类器1
m <- randomForest(Species ~. ,data = iris,importance= T)
importance(m)
varImpPlot(m,main = 'varimpplot of iris')
pre <- predict(m,newdata=iris)

# 随机森林2
library(randomForest)
data(iris)
set.seed(100)
ind <- sample(2,nrow(iris),replace = T,prob = c(0.8,0.2))
iris.rf <- randomForest(Species~.,iris[ind ==1,],ntree=50,nPerm=10,mtry=3,
                        proximity =T ,importance =T)
print(iris.rf)


# ROCR评估模型预测能力
library(ROCR)
library(e1071)
install.packages("C50")
library(C50)
data(churn)
set.seed(2)
# 设置训练集和测试集
ind <- sample(2,nrow(churnTrain),replace = T,prob = c(0.7,0.3))
trainset <- churnTrain[ind ==1,]
testset <- churnTrain[ind ==2,]
# svm模型
svmfit <- svm(churn ~.,data=trainset,prob =T)
#基于模型对测试集进行预测
pred <- predict(svmfit,testset[,!names(testset) %in% c('churn')],
                probability = T)
# yes概率
pred.prob <- attr(pred,'probabilities')
pred.to.roc <- pred.prob[,2]
# 产生预测结果
pred.rocr <- prediction(pred.to.roc,testset$churn)
# 性能评估
perf.rocr <- performance(pred.rocr,measure = 'auc',
                         x.measure = 'cutoff')
perf.tpr.rocr <- performance(pred.rocr,'tpr','fpr')
plot(perf.tpr.rocr,colorize=T,main=paste('AUC:',(perf.rocr@y.name)))


# caret 比较ROC
library(pROC)
library(caret)
control <- trainControl(method = 'repeatedcv',
                        number = 10,
                        repeats = 3,
                        classProbs = T,
                        summaryFunction = twoClassSummary)
# glm训练分类器
glm.model <- train(churn~.,data=trainset,method='glm',
                   metric='ROC',trControl= control)
# rf训练分类器
rf.model <- train(churn~.,data=trainset,method='rf',
                   metric='ROC',trControl= control)
# svm训练分类器
svm.model <- train(churn~.,data=trainset,method='svmRadial',
                   metric='ROC',trControl = control)
# rpart查看rpart在训练数据上的运行情况
rpart.model <- train(churn ~.,data=trainset,method='rpart',
                     metric='ROC',trControl=control)
# 使用模型分别进行预测
glm.probs <- predict(glm.model,testset[,!names(testset)%in% c('churn')],
                     type = 'prob')
rf.probs <- predict(rf.model,testset[,!names(testset)%in% c('churn')],
                     type = 'prob')
svm.probs <- predict(svm.model,testset[,!names(testset)%in% c('churn')],
                     type = 'prob')
rpart.probs <- predict(rpart.model,testset[,!names(testset)%in% c('churn')],
                     type = 'prob')
# 同一张图中生成每个模型的ROC曲线
glm.roc <- roc(response = testset[,c('churn')],
               predictor = glm.probs$yes,
               print.thres=T,plot = T,
               print.auc=T,
               levels = levels(testset[,c('churn')]))
plot(glm.roc,type='S',col='red',print.auc=T,auc.polygon=T,
     grid=c(0.1,0.2),grid.col='green',max.auc.polygon=T,
     auc.polygon.col='skyblue',print.thres=T)
rf.roc <- roc(response = testset[,c('churn')],
               predictor = rf.probs$yes,
               print.thres=T,plot = T,
               print.auc=T,
               levels = levels(testset[,c('churn')]))
plot(rf.roc,type='S',col='red',print.auc=T,auc.polygon=T,
     grid=c(0.1,0.2),grid.col='green',max.auc.polygon=T,
     auc.polygon.col='skyblue',print.thres=T)

svm.roc <- roc(response = testset[,c('churn')],
               predictor = svm.probs$yes,
               levels = levels(testset[,c('churn')]))
plot(svm.roc,type='S',add=T,col='blue')

rpart.roc <- roc(response = testset[,c('churn')],
               predictor = rpart.probs$yes,
               levels = levels(testset[,c('churn')]))
plot(rpart.roc,type='S',add=T,col='yellow',print.auc=T,auc.polygon=T,
     grid=c(0.1,0.2),grid.col='green',max.auc.polygon=T,
     auc.polygon.col='skyblue',print.thres=T)

ggroc(glm.roc,alpha = 0.5, colour = "red", linetype = 1, size = 2) + 
  theme_minimal() + ggtitle("My ROC curve")
# partial AUC
plot(glm.roc,print.auc=T,auc.polygon=T,partial.auc=c(1,0.8),
     partial.auc.focus='sp',grid=c(0.1,0.2),grid.col=c('green','red'),
     max.auc.polygon=T,auc.polygon.col='skyblue',print.thres=T,
     reuse.auc=F)
#计算混淆矩阵
confusionMatrix(predict(rf.model,testset,type = 'raw'),
                testset$churn)

# ggolot2绘制ROC曲线
library(ggplot2)
library(ROCR) ##用于计算ROC
data(ROCR.simple) ###画图数据集
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)  
perf <- performance(pred,"tpr","fpr") 
x <- unlist(perf@x.values)  ##提取x值
y <- unlist(perf@y.values)
plotdata <- data.frame(x,y) 
names(plotdata) <- c("x", "y")
# 画图
##先确定映射图层geom_path,labs层修改标题，scale_colour_gradient层修改图例（为何是这个看2.0），
# theme层精细修改标题。##  
g <- ggplot(plotdata) + 
  geom_path(aes(x = x, y = y, colour = x), size=1) + 
  labs(x = "False positive rate", y = "Ture positive rate", title ="ROC曲线") +
  scale_colour_gradient(name = 'False positive rate', low = 'blue', high = 'red') +
  theme(plot.title = element_text(face = 'bold',size=15))
g
# ROC绘图
library(plotROC)
set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)
test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)
ggplot(test, aes(d = D, m = M1)) + geom_roc()

# pROC绘图
install.packages("pROC")
# 导入
library(pROC)
data(aSAH)
plot.roc(aSAH$outcome, aSAH$s100b, # data
         percent=TRUE, # show all values in percent
         partial.auc=c(100, 90), partial.auc.correct=TRUE, # define a partial AUC (pAUC)
         print.auc=TRUE, #display pAUC value on the plot with following options:
         print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",
         auc.polygon=TRUE, auc.polygon.col="#1c61b6", # show pAUC as a polygon
         max.auc.polygon=TRUE, max.auc.polygon.col="#1c61b622", # also show the 100% polygon
         main="Partial AUC (pAUC)")
plot.roc(aSAH$outcome, aSAH$s100b,
         percent=TRUE, add=TRUE, type="n", # add to plot, but don't re-add the ROC itself (useless)
         partial.auc=c(100, 90), partial.auc.correct=TRUE,
         partial.auc.focus="se", # focus pAUC on the sensitivity
         print.auc=TRUE, print.auc.pattern="Corrected pAUC (100-90%% SE):\n%.1f%%", print.auc.col="#008600",
         print.auc.y=40, # do not print auc over the previous one
         auc.polygon=TRUE, auc.polygon.col="#008600",
         max.auc.polygon=TRUE, max.auc.polygon.col="#00860022")

library(pROC)
data(aSAH)
rocobj1 <- plot.roc(aSAH$outcome, aSAH$s100,
                    main="Statistical comparison", percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")
testobj <- roc.test(rocobj1, rocobj2)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)

data(iris)
iris[1,'Sepal.Length']
str(iris)
flower.type <- data.frame(Species='setosa',Folwer='iris')
merge(flower.type,iris[1:3,],by='Species')
head(iris[order(iris$Sepal.Length,decreasing = T),])
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
sapply(iris[1:4], sd,na.rm=T)
summary(iris)
#xiang guan xing
cor(iris[,1:4])
# xie fang cha
cov(iris[,1:4])
t.test(iris$Sepal.Width[iris$Species=='setosa'],iris$Sepal.Width[iris$Species=='versicolor'])
cor.test(iris$Sepal.Length,iris$Sepal.Width)
table.spe <- table(iris$Species)
pie(table.spe)
boxplot(Petal.Width ~ Species,data=iris)
plot(x=iris$Petal.Length,y=iris$Petal.Width,col=iris$Species)
str(Titanic)
train.data <- read.csv('D:\\data/prectise/Titanic-dataset/train.csv',na.strings = c('NA',''))
str(train.data)
is.na(train.data$Age)
sum(is.na(train.data$Age)==T)
# 可视化 NA
install.packages('Amelia')
require(Amelia)
library(Rcpp)
missmap(train.data,main = 'missing map')
# fill NA 
train.data$Embarked[which(is.na(train.data$Embarked))] = 'S';
table(train.data$Embarked,useNA = 'always')
train.data$Name= as.character(train.data$Name)
table_words <- table(unlist(strsplit(train.data$Name,'\\s+')))
sort(table_words [grep('\\.',names(table_words))],decreasing = T)
library(stringr)
tb <- cbind(train.data$Age,str_match(train.data$Name, '[a-zA-z]+\\.'))
table(tb[is.na(tb[,1]),2])

# jueceshu
# 数据划分函数
split.data <- function(data,p=0.7,s=666){
  set.seed(s)
  index=sample(1:dim(data)[1])
  train <- data[index[i:floor(dim(data)[1]*p),]]
  test <- data[index[((ceiling(dim(data)[1]*p)) + 1) : dim(data)[1]],]
  return(list(train = train, test = test))
}
#将数据集分成两部分
allset <- split.data(train.data, p= 0.7)
trainset <- allset$train
allset 

library(randomForest)
data(iris)
set.seed(10)
ind <- sample(2,nrow(iris),replace = T,prob = c(0.8,0.2))
str(ind)
iris.rf=randomForest(Species~.,iris[ind==1,],ntree=50,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
print(iris.rf)
iris.pred = predict(iris.rf,iris[ind==2,])
table(observed = iris[ind==2,'Species'],predicted = iris.pred)
importance(iris.rf,type = 1)
importance(iris.rf,type =2 )
varImpPlot(iris.rf)

set.seed(315)
iris.rfr <- randomForest(Species ~.,data = iris,importance=T,
                         proximity=T)
print(iris.rf)
round(importance(iris.rfr),2)
iris.mds <- cmdscale(1-iris.rfr$proximity,eig = T)
op <- par(pty='s')
pairs(cbind(iris[,1:4],iris.mds$points),cex=0.6,gap=0,
      col=c('red','green','blue')[as.numeric(iris$Species)],
      main='iris data')
par(op)
print(iris.mds$GOF)
varImpPlot(iris.rfr)


# pe250
library(randomForest)
library(caret)
library(pROC)
design <-read.table('mappingfile.txt',row.names = 1,header = T,comment.char = '')
otu_table <- read.table('sum_taxa/otu_table4_L5.txt',header = T,row.names = 1,sep = '\t',comment.char = '')
# 数据筛选
sub_design <- design[design$genotype %in% c('KO','WT'),]
idx <- rownames(sub_design) %in% colnames(otu_table)
sub_design <- sub_design[idx,]
sub_otu <- otu_table[,rownames(sub_design)]
data <- as.data.frame(t(sub_otu))
data <- data.frame(data,sub_design$genotype)
# Error in randomForest.default(m, y, ...) : Can't have empty classes in y.
data$sub_design.genotype <- droplevels(data$sub_design.genotype)
# Error in eval(predvars, data, env):object ' ' not found : names(data) <- make.names(names(data))
set.seed(315)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
otu_trainset <- data[ind ==1,]
otu_testset <- data[ind ==2,]
set.seed(315)
subotu.rf <- randomForest(sub_design.genotype ~ . , data=otu_trainset, prob=T,
                          importance=T , proximity=T)
print(subotu.rf)
otu_importance <- as.data.frame(subotu.rf$importance)
plot(otu_importance)
# 验证
otu_pre <- predict(subotu.rf,otu_testset[,!names(otu_testset) %in% c('sub_design.genotype')])

# caret进行k折交叉检验
library(caret)
control <- trainControl(method = 'repeatedcv',number = 10,repeats = 3,classProbs = T,
                        summaryFunction = twoClassSummary)
otu_model <- train(sub_design.genotype~.,data=otu_trainset,method='rf',
                   preProcess='scale',trControl=control)
# 对重要变量排序
importance <- varImp(otu_model,scale=F)
importance
plot(importance)
# 混淆矩阵评测
otu.pred <- predict(otu_model,otu_testset[,!names(otu_testset) %in% c('sub_design.genotype')])
table(otu.pred,otu_testset[,c('sub_design.genotype')])
confusionMatrix(otu.pred,otu_testset[,c('sub_design.genotype')])
# ROC评测
set.seed(315)
rf.model <- train(sub_design.genotype~.,data = otu_trainset,method='rf',
                  metric='ROC',trControl = control)
rf.probs <- predict(rf.model,otu_testset[,!names(otu_testset)%in% c('sub_design.genotype')],
                    type = 'prob')
rf.roc <- roc(response=otu_testset[,c('sub_design.genotype')],
              predictor = rf.probs$KO,levels = levels(otu_testset[,c('sub_design.genotype')]))
plot(rf.roc,type='s',col='red')
rf.importance <- varImp(rf.model,scale = T)
# feature
imp <- as.data.frame(rf.importance$importance)
imp <- imp[order(imp[,1],decreasing = T),]
head(imp)

# 交叉验证
result <- rfcv(data,data$sub_design.genotype,cv.fold = 10)
result$error.cv


# 交叉验证
set.seed(315)
result <- rfcv(data,data$sub_design.genotype ,cv.fold = 10,na.action=na.omit)
result$error.cv
with(result,plot(n.var,error.cv,log='x',type='o',lwd=2))
# 选择feature
imp <- as.data.frame(subotu.rf$importance)
imp <- imp[order(imp[,1],decreasing = T),]
head(imp)
varImpPlot(subotu.rf,main = 'top 14 feature importance',n.var = 25,
           bg=par('bg'),color=par('fg'),gcolor=par('fg'),lcolor='grey')
# 贡献度柱状图
imp <- head(imp,n = 14)
imp <- imp[order(1:4,decreasing = T),]
#   去除公共部分
imp$temp = gsub("k__Bacteria.p__","",rownames(imp),perl=TRUE) 
# 提取门名称 
imp$phylum = gsub(".[\\w-\\[\\]_]+","",imp$temp,perl=TRUE) 
# rowname unallowed same name 
imp$phylum = gsub("[\\[\\]]+","",imp$phylum,perl=TRUE) 
# 提取纲名称 
imp$class = gsub("[\\w\\[\\]._]+.c__","",imp$temp,perl=TRUE) 
imp$class = gsub("[\\[\\]]+","",imp$class,perl=TRUE)
imp$class=factor(imp$class,levels = imp$class)
p=ggplot(data = imp, mapping = aes(x=class,y=X.IncMSE,fill=phylum)) + 
  geom_bar(stat="identity")+coord_flip()
p

## ROC曲线绘制
setwd('D://data/qiime-data/q4-FMT/')
design <-read.table('otu_table_tax_L4.txt',row.names = 1,header = T,
                    comment.char = '',sep = '\t')
otu_table <- read.csv('level-5.csv',header = T,row.names = 1,comment.char = '',sep = ',')
# 数据筛选
sub_design <- design[design$SampleType %in% c('swab','stool'),]
#idx <- rownames(sub_design) %in% rownames(otu_table)
#sub_design <- sub_design[idx,]
# sub_otu <- otu_table[,colnames(sub_design)]
#data <- as.data.frame(t(sub_otu))
# data <- cbind(data,sub_design)
data <- otu_table[,1:58 ]
data <- data[,-57]
data <- data[data$sample.type %in% c('swab','stool'), ]
data <- data[,-which(names(data) %in% c('D_0__Bacteria.__.__.__.__'))]
# data <- subset(data,select=-c(''))
# 设置数据集
set.seed(315)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
trainset <- data[ind ==1,]
testset <- data[ind ==2,]
set.seed(315)
# logistic 回归，生成概率预测值
model <- glm(sample.type ~ . , data=trainset ,family = 'binomial' )
pre <- predict(model,type = 'response')
# 合并数据
model.pre <- data.frame(prob=pre,obs=trainset$sample.type)
# 按预测值排序
model.pre <- model.pre[order(model.pre$prob),]
n <- nrow(model.pre)
tpr <- fpr <- rep(0,n)
# 根据不同临界值threshold计算TPR FPR
for(i in 1:n){
  threshold <- model.pre$prob[i]
  tp <- sum(model.pre$prob > threshold & model.pre$obs == 1)
  fp <- sum(model.pre$prob > threshold & model.pre$obs == 0)
  tn <- sum(model.pre$prob > threshold & model.pre$obs == 0)
  fn <- sum(model.pre$prob > threshold & model.pre$obs == 1)
  tpr[i] <- tp/(tp + fn) # 真阳性
  fpr[i] <- fp/(tn + fp)
}
plot(fpr,tpr,type='l')
abline(a=0,b=1)

# roc 2
setwd('D://data/prectise/RiceTimeCourse-master/data/')
design <-read.table('design.txt',row.names = 1,header = T,
                    comment.char = '',sep = '\t')
otu_table <- read.csv('otu_table_tax_L5.txt',header = T,row.names = 1,comment.char = '',sep = '\t')
# 设置数据集
# 筛选品种作为训练集 sub_map = tc_map[tc_map$genotype %in% c("A50"),] # ,"IR24" # 筛选 OTU idx = rownames(sub_map) %in% colnames(otu_table) sub_map = sub_map[idx,] sub_otu = otu_table[, rownames(sub_map)] 随机森林回归 library(randomForest) set.seed(315)
sub_design = design[design$genotype %in% c("A50"),]
sub_design = sub_design[rownames(sub_design) %in% colnames(otu_table),] 
sub_otu = otu_table[, rownames(sub_design)] 
data <- as.data.frame(t(sub_otu))
site <- sub_design[,colnames(sub_design) %in% c('site')]
data <- data.frame(data,site)
library(randomForest) 
set.seed(315)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
trainset <- data[ind ==1,]
testset <- data[ind ==2,]
rf = randomForest(site ~ . ,trainset , importance=TRUE, proximity=TRUE, ntree = 1000) 
print(rf)
set.seed(315) 
# 随机数据保证结果可重复 
result = rfcv(trainset, site, cv.fold=10) 
result$error.cv
plot(result$n.var,result$error.cv)
# pROC 1
library(pROC)
rf = randomForest(site ~ . ,trainset , importance=TRUE, proximity=TRUE, ntree = 1000)
race.prediction <- as.data.frame(predict(rf,testset,type = 'prob'))
race.prediction$predict <- names(race.prediction)[1:2][apply(race.prediction[,1:2],1,which.max)]
race.prediction$observed <- testset$site
head(race.prediction)
race.roc <- roc(ifelse(race.prediction$observed == 'Cp','Cp','non-Cp'),
                as.numeric(race.prediction$Cp))
plot(race.roc,col=c('red','yellow'),print.auc=T,auc.polygon=T,
     grid=c(0.1,0.2),grid.col='green',max.auc.polygon=F,
     auc.polygon.col='yellow',print.thres=T)
lines(race.roc,col='blue')
library(ggplot2)
library(plotROC)
roc.plot <- ggplot(race.prediction,aes(d=race.prediction$predict,m=race.prediction$Sz)) + 
  geom_roc()+ style_roc()
roc.plot
rf$
# pROC 2
library(caret)
control <- trainControl(method = 'repeatedcv',number = 10,
                        repeats = 3,classProbs = T,
                        summaryFunction = twoClassSummary)
# rf训练分类器
rf.model <- train(site~.,data=trainset,method='rf',
                   metric='ROC',trControl= control)
rf.probs <- predict(rf.model,testset[,!names(testset)%in% c('site')],
                       type = 'prob')
# 同一张图中生成每个模型的ROC曲线
rf.roc <- roc(response = testset[,c('site')],
               predictor = rf.probs$Cp,print.thres=T,plot = T,
               print.auc=T,levels = levels(testset[,c('site')]))
plot(rf.roc,type='S',col='red',print.auc=T,auc.polygon=T,
     grid=c(0.1,0.2),grid.col='green',max.auc.polygon=T,
     auc.polygon.col='skyblue',print.thres=T)
