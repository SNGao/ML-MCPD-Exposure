## 定义作图函数
#pdf('/Users/gsn/Desktop/MLR1.pdf')
ActualvsPredict <- function(){
  plot(
    x = fold.test$dietaryMCPD,
    y = testpred,
    xlab = 'Actual',
    ylab = 'Prediction',
    main = 'Actual value vs Predicted value',
    #sub = 'test set'
  )
  
  testlinmod <- lm(testpred~fold.test$dietaryMCPD)
  abline(testlinmod, col='blue', lwd=2.5, lty = 'solid')
  abline(a=0, b=1, col='red', lwd=2.5, lty = 'dashed')
  legend(
    'topleft',
    legend= c('Model', 'Base'),
    col = c('black','red'),
    lwd = 2.5,
    lty = c('solid','dashed')
  )
}
#dev.off()


library(modelr)
library(mice)
library(VIM)
library(DMwR2)
library(rpart)
setwd("/Users/gsn/Desktop/")
data_MCPD <- read.csv('data_MCPD.csv',header = TRUE)

pdf(file = 'MLR&GAM.pdf', width = 4, height = 3)

sum(is.na(data_MCPD)) #查共有几个缺失值=0
sum(!complete.cases(data_MCPD))#不完整样品个数=0

# 定义分类变量
data_MCPD$SEX<-as.factor(data_MCPD$SEX)
data_MCPD$FAMINCOM5w<-as.factor(data_MCPD$FAMINCOM5w)
data_MCPD$SMOKE<-as.factor(data_MCPD$SMOKE)
data_MCPD$MARRIAGE<-as.factor(data_MCPD$MARRIAGE)
data_MCPD$education<-as.factor(data_MCPD$education)
data_MCPD$alcohol<-as.factor(data_MCPD$alcohol)
data_MCPD$CVD<-as.factor(data_MCPD$CVD)
data_MCPD$db<-as.factor(data_MCPD$db)
data_MCPD$HBP<-as.factor(data_MCPD$HBP)

# 对响应变量进行log处理
data_MCPD$dietaryMCPD <- log(data_MCPD$dietaryMCPD)
data_MCPD$DHPMA_a <- log(data_MCPD$DHPMA_a)
# 对定量数据进行标准化
data_MCPD[1:12]<- scale(data_MCPD[1:12],center=F,scale=T)

# 响应变量除去异常值(n=1585)
list = c()
for (i in 1){
  value <- data_MCPD$dietaryMCPD
  QL <- quantile(value, probs = 0.25)
  QU <- quantile(value, probs = 0.75)
  a <- which(value > QU + 1.5*(QU-QL)) # 上界异常值
  b <- which(value < QL - 1.5*(QU-QL)) # 下界异常值
  temp <- append(a,b)
  list <- append(list, temp)
  list <- list[!duplicated(list)]
}
data_MCPD <- data_MCPD[-list,]



#正态性检验
library(nortest)
lillie.test(data_MCPD$dietaryMCPD) #正态分布样本
hist(data_MCPD$dietaryMCPD)

# Lasso回归寻找预测变量
pdf('图1-3.pdf')
library(glmnet)
x = model.matrix(dietaryMCPD~.,data_MCPD)
y = data_MCPD$dietaryMCPD
# LASSO回归
grid = 10^seq(5,-5,length=20)
lasso.model = glmnet(x,y, alpha=1, lambda=grid)
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
lam_1se <- cv.out$lambda.1se
lasso.pred <- predict(lasso.model, s = bestlam, newx = x)
lasso.coef = predict(lasso.model, type = 'coefficients', s = bestlam)
lasso.coef
plot(lasso.model)
plot(cv.out)

dev.off()
data_MCPD$WEIGHT = NULL

# MLR模型及结果
model.MLR1 <-lm(dietaryMCPD ~ age+SEX+education+MARRIAGE+alcohol+SMOKE
                  +FAMINCOM5w
                  +DHPMA_a, 
                  data = data_MCPD)
summary(model.MLR1)

model.MLR2 <-lm(dietaryMCPD ~ age+SEX+education+MARRIAGE+alcohol+SMOKE
                +FAMINCOM5w
                +HEIGHT+BMI+CREATININE+MET+CVD+HBP+MET+db+HBP
                +DHPMA_a, 
                data = data_MCPD)
summary(model.MLR2)

model.MLR3 <-lm(dietaryMCPD ~ age+SEX+education+MARRIAGE+alcohol+SMOKE
                +FAMINCOM5w
                +HEIGHT+BMI+CREATININE+MET+CVD+HBP+MET+db+HBP
                +totalenergy+BEANOILintake+PEANUTOIintake+LARDOILintake
                +DHPMA_a, 
                data = data_MCPD)
summary(model.MLR3)


library(car)
vif(model.MLR3)

## 单个变量的作用
colnames(data_MCPD)
varList <- c("CREATININE","HEIGHT","BMI","age","BEANOILintake",
             "PEANUTOIintake","LARDOILintake","MET","totalenergy",'db',
             "MARRIAGE","SMOKE","SEX","FAMINCOM5w","CVD","HBP","alcohol","education")

## 检验DHPMA与其余变量的交互作用
results_BA_CES <- c()
for (i in varList){
  fit1 <- lm(dietaryMCPD~ .+DHPMA_a:data_MCPD[,i], data_MCPD)
  print(i);print(summary(fit1))
  #cmp = anova(fit1, fit2)
  #results_BA_CES <- rbind(results_BA_CES,
  #                        c(i,cmp[8][2,1]))
}
results_BA_CES



# 模型的拟合能力（根据lasso,变量选择）
model.MLR2 <-lm(dietaryMCPD~., data = data_MCPD)
summary(model.MLR2)
par(mfrow=c(2,2))
pdf('图1-6.pdf')
plot(model.MLR2)
dev.off()

# 非线性建模
library(splines)
library(gam)
str(data_MCPD)
names(data_MCPD)
model.gam <- gam(dietaryMCPD~s(CREATININE)+s(HEIGHT)+s(BMI)+s(age)+s(BEANOILintake)
                             +s(PEANUTOIintake)+s(LARDOILintake)+s(MET)+s(DHPMA_a)
                             +s(totalenergy)+MARRIAGE+SEX+FAMINCOM5w+CVD+SMOKE          
                             +HBP+alcohol+education, data = data_MCPD)
summary(model.gam)
# CREATININE,age,BEANOILintake,PEANUTOIintake,LARDOILintake,totalenergy表现出P<0.1的非线性特征

model.gam <- lm(dietaryMCPD~bs(CREATININE)+HEIGHT+BMI+bs(age)+bs(BEANOILintake)
                 +bs(PEANUTOIintake)+bs(LARDOILintake)+MET+DHPMA_a
                 +bs(totalenergy)+MARRIAGE+SEX+FAMINCOM5w+CVD      
                 +HBP+alcohol+education, data = data_MCPD)
summary(model.gam)

## SVR model
library(e1071)
model.svm <- svm(dietaryMCPD~., kernal = 'radial basis',type = 'eps-regression',data=data_MCPD)
summary(model.svm)
defaultSummary(data.frame(obs = data_MCPD$dietaryMCPD, pred = predict(model.svm, data = data_MCPD)))

## RF model
library(randomForest)
pdf('/Users/gsn/Desktop/rf.pdf')
model.RF <- randomForest(dietaryMCPD~., data=data_MCPD, importance = TRUE, ntree=500)
defaultSummary(data.frame(obs = data_MCPD$dietaryMCPD, pred = predict(model.RF, data = data_MCPD)))
importance(model.RF)
varImpPlot(model.RF) #MeanDecreaseGini
dev.off()
# 引入RF的显著性
# https://blog.csdn.net/woodcorpse/article/details/115302125
#library(rfPermute)
#model.RF <- rfPermute(dietaryMCPD~., data=data_MCPD,
#                      ntree = 500, nrep = 5, num.cores = 1,
#                      importance = TRUE)
#importance(model.RF)


#####5-fold交叉验证环节
Result_total = data.frame(model = c(0),
                          train_RMSE=c(0), train_R2=c(0),train_MAE=c(0),
                          test_RMSE=c(0), test_R2=c(0),test_MAE=c(0))
library(caret)
K=5
## MLR1-model
set.seed(208)
folds <- createFolds(y=data_MCPD$dietaryMCPD, k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  model.gam <- lm(dietaryMCPD~., data = fold.train)
  trainpred <- predict(model.gam,newdata = fold.train)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(model.gam, newdata = fold.test)
  
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])
# 测试集结果
R2_MLR1 <- result.test[2:(K+1),2]
RMSE_MLR1 <- result.test[2:(K+1),1]
MAE_MLR1 <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[1,1:7] = c('MLR1',mean(result.train[2:(K+1),1]),
                           mean(result.train[2:(K+1),2]),
                           mean(result.train[2:(K+1),3]),
                           mean(result.test[2:(K+1),1]),
                           mean(result.test[2:(K+1),2]),
                           mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/MLR1.pdf')
ActualvsPredict()
dev.off()

data_MCPD$WEIGHT = NULL

## MLR2 model
set.seed(208)
folds <- createFolds(y=data_MCPD$dietaryMCPD, k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  model.gam <- lm(dietaryMCPD~., data = fold.train)
  
  trainpred <- predict(model.gam,newdata = fold.train)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(model.gam, newdata = fold.test)
  
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])

R2_MLR2 <- result.test[2:(K+1),2]
RMSE_MLR2 <- result.test[2:(K+1),1]
MAE_MLR2 <- result.test[2:(K+1),3]

Result_total[2,1:7] = c('MLR2',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/MLR2.pdf')
ActualvsPredict()
dev.off()



## GAM model
set.seed(208)
folds <- createFolds(y=data_MCPD$dietaryMCPD, k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  model.gam <- lm(dietaryMCPD~ns(CREATININE)+HEIGHT+BMI+ns(age)+ns(BEANOILintake)
                  +ns(PEANUTOIintake)+ns(LARDOILintake)+MET+DHPMA_a
                  +ns(totalenergy)+MARRIAGE+SEX+FAMINCOM5w+CVD      
                  +HBP+alcohol+education, data = fold.train)
  
  trainpred <- predict(model.gam,newdata = fold.train)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(model.gam, newdata = fold.test)
  
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])
R2_GAM <- result.test[2:(K+1),2]
RMSE_GAM <- result.test[2:(K+1),1]
MAE_GAM <- result.test[2:(K+1),3]

Result_total[3,1:7] = c('GAM',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/GAM.pdf')
ActualvsPredict()
dev.off()


## SVR model
library(e1071)
set.seed(1)
folds <- createFolds(y=data_MCPD$dietaryMCPD, k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  model.svm <- svm(dietaryMCPD~., #kernal = 'radial basis',type = 'eps-regression',
                   data=fold.train)
  
  trainpred <- predict(model.svm,newdata = fold.train)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(model.svm, newdata = fold.test)
  
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])

R2_SVR <- result.test[2:(K+1),2]
RMSE_SVR <- result.test[2:(K+1),1]
MAE_SVR <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[4,1:7] = c('SVR',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/SVR.pdf')
ActualvsPredict()
dev.off()


## RF model

library(randomForest)
set.seed(208)
folds <- createFolds(y=data_MCPD$dietaryMCPD, k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  model.RF <- randomForest(dietaryMCPD~., data=fold.train, importance = TRUE)
  
  trainpred <- predict(model.RF,newdata = fold.train)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(model.RF, newdata = fold.test)
  
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])
R2_RF <- result.test[2:(K+1),2]
RMSE_RF <- result.test[2:(K+1),1]
MAE_RF <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[5,1:7] = c('RF',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/RF.pdf')
ActualvsPredict()
dev.off()
pdf('/Users/gsn/Desktop/plot_rf_MSE.pdf', width = 8, height = 6)
importance(model.RF)
varImpPlot(model.RF, n.var = 15)
dev.off()


## lightgbm model(没有进行labelencoding)
library(lightgbm)
set.seed(208)
folds <- createFolds(y=data_MCPD$dietaryMCPD,k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))

for (i in c(1:K)){
  test <- data_MCPD[folds[[i]],]
  trains.all <- data_MCPD[-folds[[i]],]
  
  trains <- sample(rownames(trains.all), nrow(trains.all)*3/4) # 后者为抽样样本的数量
  valids <- setdiff(rownames(trains.all), trains) # 获得两个数据集的差值
  
  data_train <- data_MCPD[trains,]
  data_valid <- data_MCPD[valids,]
  data_test <- test
  sum(duplicated(rownames(data_train), rownames(data_test)))
  sum(duplicated(rownames(data_valid), rownames(data_test)))
  
  data_trainx <-data.matrix(data_train[2:dim(data_MCPD)[2]]) 
  data_trainy <- data.matrix(data_train[1])
  
  data_testx <-data.matrix(data_test[2:dim(data_MCPD)[2]]) 
  data_testy <- data.matrix(data_test[1])
  
  data_validx <-data.matrix(data_valid[2:dim(data_MCPD)[2]]) 
  data_validy <- data.matrix(data_valid[1])
  
  dtrain <- lgb.Dataset(data = data_trainx,
                        label = data_trainy)
  
  dvalid <- lgb.Dataset(data = data_validx,
                        label = data_validy)
  
  dtest <- lgb.Dataset.create.valid(dataset=dvalid,
                                    data=data_validx,
                                    label=data_validy)
  
  valids <- list(test=dtest)
  
  lgb1 <- lgb.train(data=dtrain,
                    valids = valids,
                    objective = 'regression_l2',
                    n_estimators = 1500,
  ) 
  # 预测
  trainpred <- predict(lgb1, data_trainx)
  temp.train = defaultSummary(data.frame(obs = data_train$dietaryMCPD,pred = trainpred))
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  # 测试集预测效果
  testpred <- predict(lgb1, data_testx)
  
  temp.test = defaultSummary(data.frame(obs = data_test$dietaryMCPD, pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3]))) 
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2])

R2_LGBM <- result.test[2:(K+1),2]
RMSE_LGBM <- result.test[2:(K+1),1]
MAE_LGBM <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[6,1:7] = c('LGBM',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))

# 变量重要性
importance_matrix <- lgb.importance(model = lgb1, percentage = TRUE)
print(importance_matrix)
pdf('/Users/gsn/Desktop/LGBM.pdf')
lgb.plot.importance(tree_imp = importance_matrix,
                    top_n = 20L,
                    measure = 'Cover')
#pdf('/Users/gsn/Desktop/LGBM.pdf')
#ActualvsPredict()
dev.off()



## xgboost model
library(xgboost)
set.seed(1)
folds <- createFolds(y=data_MCPD$dietaryMCPD,k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  test <- data_MCPD[folds[[i]],]
  trains.all <- data_MCPD[-folds[[i]],]
  
  trains <- sample(rownames(trains.all), nrow(trains.all)*3/4) # 后者为抽样样本的数量
  valids <- setdiff(rownames(trains.all), trains) # 获得两个数据集的差值
  
  data_train <- data_MCPD[trains,]
  data_valid <- data_MCPD[valids,]
  data_test <- test
  sum(duplicated(rownames(data_train), rownames(data_test)))
  sum(duplicated(rownames(data_valid), rownames(data_test)))
  
  # 数据准备
  colnames(data_MCPD)
  dvfunc <- dummyVars(~., data = data_train[, 2:dim(data_MCPD)[2]], fullRank = T)
  
  # 将分类数据转为矩阵形式，fullRank=T去除变量的共线性
  data_trainx <- predict(dvfunc, newdata = data_train[,2:dim(data_MCPD)[2]])
  data_trainy <- data_train$dietaryMCPD
  
  data_validx <- predict(dvfunc, newdata = data_valid[,2:dim(data_MCPD)[2]])
  data_validy <- data_valid$dietaryMCPD
  
  data_testx <- predict(dvfunc, newdata = data_test[,2:dim(data_MCPD)[2]])
  data_testy <- data_test$dietaryMCPD
  
  dtrain <- xgb.DMatrix(data = data_trainx,
                        label = data_trainy)
  
  dvalid <- xgb.DMatrix(data = data_validx,
                        label = data_validy)
  
  dtest <- xgb.DMatrix(data = data_testx,
                       label = data_testy)
  
  # 提前终止数据准备
  watchlist <- list(train = dtrain, test=dvalid)
  
  # 模型训练
  fit_xgb_reg <- xgb.train(
    data = dtrain,
    objective = 'reg:squarederror', # 明确所训练模型的类型
    nrounds = 500, #迭代次数，训练树的数量
    watchlist = watchlist,
    #训练终止策略，模型训练好后在validation上进行验证效果，
    verbose = 0
  )
  
  # 预测
  trainpred <- predict(fit_xgb_reg,
                       newdata = dtrain)
  # 训练集
  temp.train = defaultSummary( # 源自caret包
    data.frame(obs = data_train$dietaryMCPD,
               pred = trainpred))
  
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  
  # 测试集预测效果
  testpred <- predict(fit_xgb_reg,
                      newdata = dtest)
  
  temp.test = defaultSummary(data.frame(obs = data_test$dietaryMCPD,
                                        pred = testpred))
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2]) #0.8~0.92

R2_XGB <- result.test[2:(K+1),2]
RMSE_XGB <- result.test[2:(K+1),1]
MAE_XGB <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[7,1:7] = c('XGB',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))

importance_matrix <- xgb.importance(model = fit_xgb_reg)
importance_matrix2 <- data.frame(importance_matrix)
#write.csv(importance_matrix2, '/Users/gsn/Desktop/importance_matrix.csv')
print(importance_matrix)
pdf('/Users/gsn/Desktop/XGB.pdf')
xgb.plot.importance(importance_matrix = importance_matrix,
                    top_n = 20L,
                    measure = 'Cover')
dev.off()
pdf('/Users/gsn/Desktop/xgboost.pdf')
for (i in c(1)){
  plot(
    x = data_test$dietaryMCPD,
    y = testpred,
    xlab = 'Actual',
    ylab = 'Prediction',
    main = 'Actual value vs Predicted value',
    #sub = '测试集'
  )
  testlinmod <- lm(testpred~data_test$dietaryMCPD)
  abline(testlinmod, col='blue', lwd=2.5, lty = 'solid')
  abline(a=0, b=1, col='red', lwd=2.5, lty = 'dashed')
  legend(
    'topleft',
    legend= c('Model', 'Base'),
    col = c('blue','red'), # 修改了作图颜色
    lwd = 2.5,
    lty = c('solid','dashed')
  )
}
dev.off()



## catboost model
#data_temp = data_MCPD
library(catboost)
coulumn_description_vector = c()
coulumn_description_vector = rep('numeric',11)
coulumn_description_vector[12:19] = rep('factor',8)
cat_features <- c(12:19)
for (i in cat_features){
  data_MCPD[,i] <- as.numeric(factor(data_MCPD[,i]))}

target = c(1)
cat_features <- c(10:16)
K=5
set.seed(1)
folds <- createFolds(y=data_MCPD$dietaryMCPD,k=K)
result.train <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
result.test <<- data.frame(RMSE=c(0), R2=c(0),MAE=c(0))
for (i in c(1:K)){
  fold.test <- data_MCPD[folds[[i]],]
  fold.train <- data_MCPD[-folds[[i]],]
  
  train_pool <- catboost.load_pool(as.matrix(fold.train[,-target]),	
                                   label = as.matrix(fold.train[,target]),	
                                   cat_features = cat_features)	
  test_pool <- catboost.load_pool(as.matrix(fold.test[,-target]),	
                                  label = as.matrix(fold.test[,target]),	
                                  cat_features = cat_features)	
  
  fit_params <- list(iterations = 300,	
                     thread_count = 10,
                     loss_function = 'RMSE',	
                     #border_count = 32,	
                     #depth = 5,	
                     #learning_rate = 0.03,	
                     #l2_leaf_reg = 3.5,	
                     train_dir = 'train_dir',	
                     logging_level = 'Silent'	
  )	
  model.cat <- catboost.train(train_pool, test_pool, fit_params)
  
  trainpred <- catboost.predict(model.cat, train_pool)
  temp.train = defaultSummary(data.frame(obs = fold.train$dietaryMCPD,
                                         pred = trainpred))
  temp.train
  result.train <<- rbind(result.train[1:3],c(mean(temp.train[1]),
                                             mean(temp.train[2]),
                                             mean(temp.train[3])))
  # 测试集预测效果
  testpred <- catboost.predict(model.cat, test_pool)
  temp.test = defaultSummary(data.frame(obs = fold.test$dietaryMCPD,
                                        pred = testpred))
  temp.test
  
  result.test <<- rbind(result.test[1:3],c(mean(temp.test[1]),
                                           mean(temp.test[2]),
                                           mean(temp.test[3])))
}
mean(result.train[2:(K+1),2])
mean(result.test[2:(K+1),2]) 
R2_CAT <- result.test[2:(K+1),2]
RMSE_CAT <- result.test[2:(K+1),1]
MAE_CAT <- result.test[2:(K+1),3]
# 训练集、测试集平均结果
Result_total[8,1:7] = c('CAT',mean(result.train[2:(K+1),1]),
                        mean(result.train[2:(K+1),2]),
                        mean(result.train[2:(K+1),3]),
                        mean(result.test[2:(K+1),1]),
                        mean(result.test[2:(K+1),2]),
                        mean(result.test[2:(K+1),3]))
pdf('/Users/gsn/Desktop/CAT.pdf')
ActualvsPredict()
dev.off()

VIP_I_cat = catboost.get_feature_importance(model.cat, test_pool, type = 'Interaction')
VIP_cat = catboost.get_feature_importance(model.cat, test_pool, type = 'FeatureImportance')
VN_cat = colnames(data_MCPD)[-1]
write.csv(VIP_I_cat, '/Users/gsn/Desktop/VIP_I_cat.csv')
write.csv(VIP_cat, '/Users/gsn/Desktop/VIP_cat.csv')
write.csv(VN_cat, '/Users/gsn/Desktop/VN_cat.csv')




###### 绘图
library(ggplot2)
Result.Rsquare <- data.frame(R2_MLR1, R2_MLR2, R2_GAM, R2_SVR, R2_RF,
                             R2_CAT, R2_LGBM, R2_XGB)
Result.RMSE <- data.frame(RMSE_MLR1, RMSE_MLR2, RMSE_GAM, RMSE_SVR, RMSE_RF,
                             RMSE_CAT, RMSE_LGBM, RMSE_XGB)
Result.MAE <- data.frame(MAE_MLR1, MAE_MLR2, MAE_GAM, MAE_SVR, MAE_RF,
                             MAE_CAT, MAE_LGBM, MAE_XGB)

write.csv(Result.Rsquare,'Result.Rsquare.csv')
write.csv(Result.RMSE,'Result.RMSE.csv')
write.csv(Result.MAE,'Result.MAE.csv')
write.csv(Result_total,'Result_total.csv')


## 绘制箱型图
pdf('xx.pdf',width=9,height=6)
#ggplot(Result.Rsquare)
boxplot(Result.Rsquare, ylim=c(0.2,0.6)) 
dev.off()
## 将结果数据合并
Result2=data.frame()
label = c()
for (i in colnames(Result.Rsquare)){
  temp = rep(i,5)
  label = append(label, temp)
}
Result = Result.Rsquare
colnames(Result) = c(rep('Rsquare',8))

for (i in c(1:dim(Result.Rsquare)[2])){
  Result2 = rbind(Result2, Result[i])
}
Result2 = cbind(label, Result2)

## 绘制箱刑图1
pdf('xxx.pdf',width = 9,height = 6)
ggplot(Result2, aes(label, Rsquare))+
  geom_boxplot()+theme_bw()

## 绘制箱刑图2
attach(Result2)
boxplot(Result2$Rsquare~Result2$label)
points(Result2$Rsquare~factor(Result2$label))
detach(Result2)
dev.off()
## 导出作图数据
write.csv(Result.Rsquare, 'Result.Rsquare.csv')
write.csv(Result2, 'Result2.csv')

## xgboost变量重要性绘图
importance_matrix <- xgb.importance(model = fit_xgb_reg)
importance_matrix2 <- data.frame(importance_matrix)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = 'Frequency')

write.csv(importance_matrix2, '/Users/gsn/Desktop/importance_matrix.csv')

## 最后一折测试集中Xgboost预测值与实际值的回归直线
library(ggplot2)
data_xgb_plot = data.frame(cbind(testpred, fold.test$dietaryMCPD))
colnames(data_xgb_plot) = c('predict', 'dietaryMCPD')

library(visreg)
model_1 <- lm(dietaryMCPD ~ predict, data = data_xgb_plot)
visreg(model_1)

ggplot(data_xgb_plot, aes(dietaryMCPD, predict)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data_xgb_plot, aes(dietaryMCPD, predict, color = predict)) + theme_bw() 
  + theme(panel.grid=element_blank()) + geom_point()scale_x_continuous(limits = c(1,16)) + stat_smooth(method = lm, se = TRUE) + theme(axis.title = element_text(size = 13, face = 'bold')) + labs(title = 'R = 0.46, P<1.627e-15', x = 'SFA concentrations', y = 'Predicted score', colour = 'Score') + theme(
  plot.title = element_text(size = 13, 
                            #family = 'Times New Roman', 
                            face = 'bold'),
  axis.text = element_text(size = 11, face = 'bold'))
ggsave('/Users/gsn/Desktop/sfa.TIFF',width = 4, height = 4)

dev.off()


#
library(Hmisc)
library(corrplot)
result.cor = rcorr(as.matrix(data_MCPD))
pdf('/Users/gsn/Desktop/cor.pdf')
corrplot(result.cor$r, type = "upper", tl.pos = "lu", tl.col = "black", tl.cex = 0.5)
corrplot(result.cor$r, p.mat = result.cor$P, type = "lower", method = "square", tl.pos = "n", add=T, sig.level = .1)
dev.off()