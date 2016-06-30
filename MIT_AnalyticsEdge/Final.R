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




## 2.UNDERSTANDING RETAIL CONSUMERS
Households <- read.csv("Households.csv")
sum(Households$MorningPct >= 100)
sum(Households$AfternoonPct >= 100)

problemTwoData = subset(Households, AvgSalesValue >= 150)
min(problemTwoData$AvgDiscount)
problemTwoData = subset(Households, AvgDiscount >= 25)
min(problemTwoData$AvgSalesValue)
problemTwoData = subset(Households, NumVisits >= 300)
str(problemTwoData)

library(caret)
preproc = preProcess(Households)
HouseholdsNorm = predict(preproc, Households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)

set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)

set.seed(200)
KMC_model = kmeans(HouseholdsNorm, centers = 10)
table(KMC_model$cluster)

KMC_clusters = split(HouseholdsNorm, KMC_model$cluster)
colMeans(KMC_clusters[[1]])
colMeans(KMC_clusters[[2]])
colMeans(KMC_clusters[[3]])
colMeans(KMC_clusters[[4]])
colMeans(KMC_clusters[[5]])
colMeans(KMC_clusters[[6]])
colMeans(KMC_clusters[[7]])
colMeans(KMC_clusters[[8]])
colMeans(KMC_clusters[[9]])
colMeans(KMC_clusters[[10]])

set.seed(5000)
KMC = kmeans(HouseholdsNorm, centers = 5)
table(KMC$cluster)

KMC_clusters = split(HouseholdsNorm, KMC_model$cluster)
colMeans(KMC_clusters[[1]])
colMeans(KMC_clusters[[2]])
colMeans(KMC_clusters[[3]])
colMeans(KMC_clusters[[4]])
colMeans(KMC_clusters[[5]])




## 3.PATTERNS IN RENEWABLE ENERGY GENERATION
energy = read.csv("energy.csv")
mean(subset(energy, STATE == "AZ")$GenTotalRenewable)
mean(subset(energy, STATE == "CA")$GenTotalRenewable)
mean(subset(energy, STATE == "ID")$GenTotalRenewable)
mean(subset(energy, STATE == "MA")$GenTotalRenewable)
subset(energy, STATE == "ID")

mean(subset(energyNoNA, presidential.results == 0)$AllSourcesCO2)
mean(subset(energyNoNA, presidential.results == 1)$AllSourcesCO2)

cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use = "complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use = "complete")
cor(energy$AllSourcesCO2, energy$EsalesCommercial, use = "complete")

library(ggplot2)
p = ggplot(energy, aes(factor(STATE), EPriceTotal))
p + geom_boxplot()

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
mod = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data=test, family = "binomial")
summary(mod)

mod_predict = predict(mod, newdata = test, type = "response")
table(mod_predict > 0.5, test$GenSolarBinary)
test_republican = subset(test, presidential.results == 0)
mod_predict = predict(mod, newdata = test_republican, type = "response")
table(mod_predict > 0.5, test_republican$GenSolarBinary)
test_democrat = subset(test, presidential.results == 1)
mod_predict = predict(mod, newdata = test_democrat, type = "response")
table(mod_predict > 0.5, test_democrat$GenSolarBinary)

train.limited = subset(train, select=c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import"))
test.limited = subset(test, select=c("CumlRegulatory", "CumlFinancial", "presidential.results", "Total.salary", "Import"))
library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
preproc = preProcess(test.limited)
train.norm = predict(preproc, test.limited)
set.seed(144)
KMC_model = kmeans(train.norm, centers = 2, iter.max = 1000)
library(flexclust)
KMC_model.kcca = as.kcca(KMC_model, train.norm)
cluster.train = predict(KMC_model.kcca)
train1 = subset(train, cluster.train == 1)
train2 = subset(train, cluster.train == 2)
colMeans(train1)
colMeans(train2)

mod1 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train1, family = "binomial")
summary(mod1)

mod2 = glm(GenSolarBinary ~ GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train2, family = "binomial")
summary(mod1)

mod2_predict = predict(mod2, newdata = test2, type = "reponse")
mod2_outcome = table(mod2 > 0.5, test2$GenSolarBinary)
mod_predict = predict(mod, newdata = test2, type = "reponse")
mod_outcome = table(mod > 0.5, test2$GenSolarBinary)

AllPredictions = c(mod_predict, mod2_predict)
AllOutcomes = c(mod_outcome, mod2_outcome)
