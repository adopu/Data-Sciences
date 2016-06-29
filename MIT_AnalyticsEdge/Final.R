## 1.FORECASTING INTEREST RATE HIKES BY THE U.S. FEDERAL RESERVE
fedFunds <- read.csv("federalFundsRate.csv", stringsAsFactors=FALSE)
str(fedFunds)

table(fedFunds$Chairman)

as.factor(fedFunds$Chairman)
as.factor(fedFunds$DemocraticPres)
as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
training = subset(fedFunds, spl=TRUE)
testing = subset(fedFunds, spl=FALSE)

LR_model = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, family="binomial")
summary(LR_model)

bank = data.frame(PreviousRate=1.7, Streak=-3, Unemployment=5.1, HomeownershipRate=65.3, DemocraticPres=0, MonthsUntilElection=18)
predict(LR_model, newdata=bank, type="response")

LR_predict = predict(LR_model, newdata=testing, type="response")
table(LR_predict>0.5, testing$RaisedFedFunds)

library(ROCR)
ROCR_pred = prediction(LR_predict, testing$RaisedFedFunds)
as.numeric(performance(ROCRpred, "auc")@y.values)

PredictROC = predict(LR_model, newdata=testing)
pred = prediction(PredictROC, testing$RaisedFedFunds)
perf = performance(pred, "tpr", "fpr")
plot(perf, main="ROC curve", colorize=TRUE)

set.seed(201)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
fitTing = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.001)
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl = fitTing, tuneGrid = cartGrid )

CART_model = rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "class", control = rpart.control(cp = 0.016))
prp(tree)

CART_pred = predict(CART_model, newdata=testing, type="class")
table(CART_pred, testing$RaisedFedFunds)



## 2.FORECASTING INTEREST RATE HIKES BY THE U.S. FEDERAL RESERVE
Households <- read.csv("Households.csv")
mean(Households$MorningPct)
mean(Households$AfternoonPct)
table(Households$NumVisits, Households$MorningPct)
.2873369*2500
.5144897*2500
sum(Households$MorningPct == 0)
sum(Households$AfternoonPct == 0)
##WRONG, Use subset and MorningPct >= 100
##Problem 2
150plus <- subset(Households, AvgSalesValue >= 150)
mean(Households$NumVisits > 300)
Disc25Plus <- subset(Households, AvgDiscount > 25)
min(Disc25Plus$AvgSalesValue)
##Problem 3
#NumVisits
##Problem 4
library(caret)

preproc = preProcess(Households)

HouseholdsNorm = predict(preproc, Households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)

##Correct!
##Problem 5
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

##WRONG
##Problem 6
set.seed(200)
KMC = kmeans(HouseholdsNorm, centers = 10)
table(KMC$cluster)
?centroid
plot(KMC$cluster)
mean(KMC$cluster)
KMC$cluster
##Problem 12
set.seed(5000)
KMC = kmeans(HouseholdsNorm, centers = 5)
table(KMC$cluster)
##Problem 13

