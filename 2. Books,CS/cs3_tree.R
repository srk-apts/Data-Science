require(latticeExtra)
require(corrgram)
require(rpart)
require(rpart.plot)
require(partykit)
require(caret)
require(klaR)
require(pROC)
require(e1071)

## READ DATA

pima.train <-
  read.table("http://www.stats.ox.ac.uk/pub/PRNN/pima.tr", header=T)
pima.train2 <-
  read.table("http://www.stats.ox.ac.uk/pub/PRNN/pima.tr2", header=T)
pima.test <-
  read.table("http://www.stats.ox.ac.uk/pub/PRNN/pima.te", header=T)

## Attribute Information:
## 1. Number of times pregnant
## 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
## 3. Diastolic blood pressure (mm Hg)
## 4. Triceps skin fold thickness (mm)
## 5. Body mass index (weight in kg/(height in m)^2)
## 6. Diabetes pedigree function
## 7. Age (years)
## 8. Response variable: Diabetes (Yes or No)

#################################################################
###################### Exploratory Analysis #####################

# Merge Data and Finding Duplicates
pima.full <- rbind(pima.test,pima.train2)
which(duplicated(pima.full))
#Diabetes Frequency Barplot
ylim <- c(0, 1.2*max(as.numeric(table(pima.full[8]))))
xx <- barplot(table(pima.full[8]), width = 0.85, ylim = ylim, ylab ="Number of Cases")
text(x =xx, y = as.numeric(table(pima.full[8])), label = as.numeric(table(pima.full[8])),
     pos = 3, cex = 0.8, col = "red")
title("Barplot for Diabetes in Pima Indians Data")

#Attributes Boxplot

plot.new()
title(main = "\nPima Indians Data Attributes Boxplots", outer=T)
plot(bwplot(npreg ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Times pregnant"),
     split = c(1, 1, 4, 2), newpage = FALSE)
plot(bwplot(glu ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Glucose concentration"),
     split = c(2, 1, 4, 2), newpage = FALSE)
plot(bwplot(bp ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Blood pressure"),
     split = c(3, 1, 4, 2), newpage = FALSE)
plot(bwplot(skin ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Triceps skin thickness"),
     split = c(1, 2, 4, 2), newpage = FALSE)
plot(bwplot(bmi ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Body mass index"),
     split = c(2, 2, 4, 2), newpage = FALSE)
STAT 6850 - Case Study 3
plot(bwplot(ped ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Diabetes pedigree"),
     split = c(3, 2, 4, 2), newpage = FALSE)
plot(bwplot(age ~ type, data = pima.full, pch="|",
            xlab = "Diabetes", ylab="Age"),
     split = c(4, 1, 4, 2), newpage = FALSE)

#Correlation Tests

(cormatrix <- cor(pima.full[1:7],pima.full[1:7], use="p"))
corrgram(pima.full, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main ="Correlation Matrix Pima Indians Data")
cat("Correlation Matrix\n",file = "exploratory.csv")
cat( "\t", colnames(cormatrix), "\n", file = "exploratory.csv", sep = ",",append=T)
write.table(cormatrix, file = "exploratory.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
#Missing Values
(train2miss <- colSums(is.na(pima.full[1:6])))
cat("\n,", "Missing Values\n", file = "exploratory.csv", append = T)
write.table(train2miss, file = "exploratory.csv", sep = ",", col.names = F,
            qmethod = "double", append=T)

###################################################################
########Training with Complete Observations (pima.train)###########

set.seed(12345)
rp1 <- rpart(type ~ ., data=pima.train, method='class',
             control=NA)
rpart.plot(rp1, main = "Pima.Train Diabetes Tree Model") #Better Plot
plot(as.party(rp1), main="Pima.Train Diabetes Tree Model") #Informative Plot

######Training pima.train Results

srp1 <- summary(rp1)
srp1$variable.importance
srp1$cptable
cat("pima.train Variable Importance\n", file = "pima.train.csv")
write.table(srp1$variable.importance, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
#Training pima.train Plots
dotplot(sort(srp1$variable.importance, decreasing = F), type=c("p","h"),xlab="%",
        main ="pima.train Training Variable Importance")
partimat(type ~ glu+bmi+age+ped, data=pima.train, method="rpart",
         imageplot=F, gs=c(pch=8,pch=16)[unclass(pima.train$type)],
         main="Pima.Train Partition ~ Atttributes")
legend(locator(1), xpd = TRUE, horiz = T, lwd=1, pch = c(8,16), bty='n', text.width=0.2,
       cex=0.9, legend=c("No","Yes"))
#CP Table
cat("\n", "pima.train CP Table\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(srp1$cptable), "\n", file = "pima.train.csv", sep = ",", append=T)
write.table(srp1$cptable, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
plotcp(rp1, col="red", lty=2, main="train")
crp1 <- confusionMatrix(pima.train[["type"]], predict(rp1, pima.train, type="class"))
crp1$table
rp1res <-
  (cbind(crp1$overall["Accuracy"],cbind(sensitivity(crp1$table),specificity(crp1$table))))
colnames(rp1res) = c("Accuracy","Sensitivity","Specificity")
STAT 6850 - Case Study 3
rownames(rp1res) = "Results"
print(rp1res)
cat("\n", "pima.train Prediction Matrix\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(crp1$table), "\n", file = "pima.train.csv", sep = ",", append=T)
write.table(crp1$table, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "pima.train Training Results\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(rp1res), "\n", file = "pima.train.csv", sep = ",", append=T)
write.table(rp1res, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)

###Predicting test with complete observations (pima.train)

test1 <- confusionMatrix(pima.test[["type"]], predict(rp1, pima.test, type="class"))
test1$table
test1res <-
  (cbind(test1$overall["Accuracy"],cbind(sensitivity(test1$table),specificity(test1$table))))
colnames(test1res) = c("Accuracy","Sensitivity","Specificity")
rownames(test1res) = "Results"
print(test1res)
cat("\n", "Prediction Matrix Test using pima.train\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(test1$table), "\n", file = "pima.train.csv", sep = ",", append=T)
write.table(test1$table, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with pima.train\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(test1res), "\n", file = "pima.train.csv", sep = ",", append=T)
write.table(test1res, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)

##Pruning Training tree

pru1 <- prune(rp1, cp=rp1$cptable[which.min(rp1$cptable[,"xerror"]),"CP"])
rpart.plot(pru1, main = "Pima.Train Diabetes Pruned Tree Model") #Better Plot
plot(as.party(pru1), main="Pima.Train Diabetes Pruned Tree Model") #Informative Plot

###Training set with pruned complete observations (pruned pima.train)

cprurp1 <- confusionMatrix(pima.train[["type"]], predict(pru1, pima.train, type="class"))
cprurp1$table
prurp1res <-
  (cbind(cprurp1$overall["Accuracy"],cbind(sensitivity(cprurp1$table),specificity(cprurp1$table))))
colnames(prurp1res) = c("Accuracy","Sensitivity","Specificity")
rownames(prurp1res) = "Results"
print(prurp1res)
cat("\n", "Pruned pima.train Prediction Matrix\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(cprurp1$table), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(cprurp1$table, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Pruned pima.train Training Results\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(prurp1res), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(prurp1res, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)

###Test set with pruned complete observations (pruned pima.train)

testpr1 <- confusionMatrix(pima.test[["type"]], predict(pru1, pima.test, type="class"))
testpr1$table
testpr1res <-
  (cbind(testpr1$overall["Accuracy"],cbind(sensitivity(testpr1$table),specificity(testpr1$table))))
colnames(testpr1res) = c("Accuracy","Sensitivity","Specificity")
rownames(testpr1res) = "Results"
print(testpr1res)
STAT 6850 - Case Study 3
cat("\n", "Prediction Matrix Test using Pruned pima.train\n", file = "pima.train.csv", append =
      T)
cat( "\t", colnames(testpr1$table), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(testpr1$table, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with Pruned pima.train\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(testpr1res), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(testpr1res, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)

###################################################################
########Training with Incomplete Observations (pima.train2)########

set.seed(12345)
rp2 <- rpart(type ~ ., data=pima.train2, method='class', na.action = na.rpart,
             control=NA)
rpart.plot(rp2, main = "Pima.Train2 Diabetes Tree Model") #Better Plot
plot(as.party(rp2), main="Pima.Train2 Diabetes Tree Model") #Informative Plot
#Training pima.train2 Results
srp2 <- summary(rp2)
srp2$variable.importance
srp2$cptable
cat("pima.train2 Variable Importance\n", file = "pima.train2.csv")
write.table(srp2$variable.importance, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
#Training pima.train2 Plots
dotplot(sort(srp2$variable.importance, decreasing = F), type=c("p","h"),xlab="%",
        main ="pima.train2 Training Variable Importance")
partimat(type ~ glu+bmi+age+ped, data=pima.train2, method="rpart",
         imageplot=F, gs=c(pch=8,pch=16)[unclass(pima.train2$type)],
         main="Pima.Train2 Partition ~ Atttributes")
legend(locator(1), xpd = TRUE, horiz = T, lwd=1, pch = c(8,16), bty='n', text.width=0.2,
       cex=0.9, legend=c("No","Yes"))
#CP Tables
cat("\n", "pima.train2 CP Table\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(srp2$cptable), "\n", file = "pima.train2.csv", sep = ",", append=T)
write.table(srp2$cptable, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
plotcp(rp2, col="red", lty=2, main="train2")
crp2 <- confusionMatrix(pima.train2[["type"]], predict(rp2, pima.train2, type="class"))
crp2$table
rp2res <-
  (cbind(crp2$overall["Accuracy"],cbind(sensitivity(crp2$table),specificity(crp2$table))))
colnames(rp2res) = c("Accuracy","Sensitivity","Specificity")
rownames(rp2res) = "Results"
print(rp2res)
cat("\n", "pima.train2 Prediction Matrix\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(crp2$table), "\n", file = "pima.train2.csv", sep = ",", append=T)
write.table(crp2$table, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "pima.train2 training Results\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(rp2res), "\n", file = "pima.train2.csv", sep = ",", append=T)
write.table(rp2res, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
STAT 6850 - Case Study 3

###Predicting test with incomplete observations (pima.train2)

test2 <- confusionMatrix(pima.test[["type"]], predict(rp2, pima.test, type="class"))
test2$table
test2res <-
  (cbind(test2$overall["Accuracy"],cbind(sensitivity(test2$table),specificity(test2$table))))
colnames(test2res) = c("Accuracy","Sensitivity","Specificity")
rownames(test2res) = "Results"
print(test2res)
cat("\n", "Prediction Matrix Test using pima.train2\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(test2$table), "\n", file = "pima.train2.csv", sep = ",", append=T)
write.table(test2$table, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with pima.train2\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(test2res), "\n", file = "pima.train2.csv", sep = ",", append=T)
write.table(test2res, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
##Pruning Test tree2
pru2 <- prune(rp2, cp=rp2$cptable[which.min(rp2$cptable[,"xerror"]),"CP"])
rpart.plot(pru2, main = "Pima.Train2 Diabetes Pruned Tree Mode") #Better Plot
plot(as.party(pru2), main="Pima.Train2 Diabetes Pruned Tree Mode") #Informative Plot

###Training set with pruned complete observations (pruned pima.train2)

cprurp2 <- confusionMatrix(pima.train2[["type"]], predict(pru2, pima.train2, type="class"))
cprurp2$table
prurp2res <-
  (cbind(cprurp2$overall["Accuracy"],cbind(sensitivity(cprurp2$table),specificity(cprurp2$table))))
colnames(prurp2res) = c("Accuracy","Sensitivity","Specificity")
rownames(prurp2res) = "Results"
print(prurp2res)
cat("\n", "Pruned pima.train2 Prediction Matrix\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(cprurp2$table), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(cprurp2$table, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Pruned pima.train2 Training Results\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(prurp2res), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(prurp2res, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)

###Test set with pruned incomplete observations (pruned pima.train2)

testpr2 <- confusionMatrix(pima.test[["type"]], predict(pru2, pima.test, type="class"))
testpr2$table
testpr2res <-
  (cbind(testpr2$overall["Accuracy"],cbind(sensitivity(testpr2$table),specificity(testpr2$table))))
colnames(testpr2res) = c("Accuracy","Sensitivity","Specificity")
rownames(testpr2res) = "Results"
print(testpr2res)
cat("\n", "Prediction Matrix Test using Pruned pima.train2\n", file = "pima.train2.csv", append =
      T)
cat( "\t", colnames(testpr2$table), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(testpr2$table, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with Pruned pima.train2\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(testpr2res), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(testpr2res, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
STAT 6850 - Case Study 3

###################################################################
################## Test Cross Validation ##########################

## Using pima.train
##Create a full tree from train

set.seed(12345)
rcontrol <- rpart.control(minsplit = 2, cp=0, xval = 10)
full1 <- rpart(type ~ ., data=pima.train, method='class',
               control=rcontrol)
rpart.plot(full1, main = "Pima.Train Diabetes Full Size Tree Model") #Better Plot
##10-Fold X-Val on CP
set.seed(12345)
cvf1 <- createFolds(pima.train$type, k=10)
control <- rpart.control(minsplit = 2, cp=0, xval = 10)
cvres1<-matrix(0,21,10)
for (i in 1:10)
{
  data1<-pima.train[-cvf1[[i]],]
  set.seed(12345)
  cvrp1 <- rpart(type ~., data=data1, method="class", control=rcontrol)
  cvrp1$cptable
  z=0
  for (j in 0:20/100)
  {
    pr1<- prune(cvrp1, cp=j)
    x <- nrow(pr1$cptable)
    y <- (pr1$cptable)[x,4]
    z=z+1
    cvres1[z,i] <- round(y,4)
  }
}
colnames(cvres1)= c(1:10)
rownames(cvres1)= c(0:20/100)
plot(rowMeans(cvres1), type="o", xaxt='n', xlab= "CP Values",
     ylab = "X-Val Relative Error", main = "pima.train Cross-Validation Results on Test Set")
axis(1,at=c(1:21), labels=rownames(cvres1))
#Pruning pima.train full tree
(min1 <- max((which(rowMeans(cvres1)== min(rowMeans(cvres1)))-1)/100))
fullpru1 <- prune(full1, cp=min1)
plot(as.party(fullpru1), main="Pima.Train Diabetes X-Val Pruned Tree Model")
#Predicting Pruned pima.train on Test Set
finalpru1 <- confusionMatrix(pima.test[["type"]], predict(fullpru1, pima.test, type="class"))
finalpru1$table
finalpru1res <-
  (cbind(finalpru1$overall["Accuracy"],cbind(sensitivity(finalpru1$table),specificity(finalpru1$tab
                                                                                      le))))
colnames(finalpru1res) = c("Accuracy","Sensitivity","Specificity")
rownames(finalpru1res) = "Results"
print(finalpru1res)
cat("\n", "Prediction Matrix Test using X-Val Pruned Tree\n", file = "pima.train.csv", append =
      T)
cat( "\t", colnames(finalpru1$table), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(finalpru1$table, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with X-Val Pruned tree\n", file = "pima.train.csv", append = T)
cat( "\t", colnames(finalpru1res), "\n", file = "pima.train.csv", sep = ",",append=T)
write.table(finalpru1res, file = "pima.train.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
STAT 6850 - Case Study 3

######## Using pima.train2
##Create a full tree from train2

set.seed(12345)
rcontrol <- rpart.control(minsplit = 2, cp=0, xval = 10)
full2 <- rpart(type ~ ., data=pima.train2, method='class',
               control=rcontrol)
rpart.plot(full2, main = "Pima.Train2 Diabetes Full Size Tree Model") #Better Plot
##10-Fold X-Val on CP
set.seed(12345)
cvf2 <- createFolds(pima.train2$type, k=10)
control <- rpart.control(minsplit = 2, cp=0, xval = 10)
cvres2<-matrix(0,21,10)
for (i in 1:10)
{
  data2<-pima.train2[-cvf2[[i]],]
  set.seed(12345)
  cvrp2 <- rpart(type ~., data=data2, method="class", control=rcontrol)
  cvrp2$cptable
  z=0
  for (j in 0:20/100)
  {
    pr2<- prune(cvrp2, cp=j)
    x <- nrow(pr2$cptable)
    y <- (pr2$cptable)[x,4]
    z=z+1
    cvres2[z,i] <- round(y,4)
  }
}
colnames(cvres2)= c(1:10)
rownames(cvres2)= c(0:20/100)
plot(rowMeans(cvres2), type="o", xaxt='n', xlab= "CP Values",
     ylab = "X-Val Relative Error", main = "pima.train2 Cross-Validation Results on Test Set")
axis(1,at=c(1:21), labels=rownames(cvres2))
#Pruning pima.train2 full tree
(min2 <- max((which(rowMeans(cvres2)== min(rowMeans(cvres2)))-1)/100))
fullpru2 <- prune(full2, cp=min2)
plot(as.party(fullpru2), main="Pima.Train2 Diabetes X-Val Pruned Tree Model")
#Predicting Pruned pima.train2 on Test Set
finalpru2 <- confusionMatrix(pima.test[["type"]], predict(fullpru2, pima.test, type="class"))
finalpru2$table
finalpru2res <-
  (cbind(finalpru2$overall["Accuracy"],cbind(sensitivity(finalpru2$table),specificity(finalpru2$tab
                                                                                      le))))
colnames(finalpru2res) = c("Accuracy","Sensitivity","Specificity")
rownames(finalpru2res) = "Results"
print(finalpru2res)
cat("\n", "Prediction Matrix Test using X-Val Pruned Tree 2\n", file = "pima.train2.csv", append
    = T)
cat( "\t", colnames(finalpru2$table), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(finalpru2$table, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Test results with X-Val Pruned tree 2\n", file = "pima.train2.csv", append = T)
cat( "\t", colnames(finalpru2res), "\n", file = "pima.train2.csv", sep = ",",append=T)
write.table(finalpru2res, file = "pima.train2.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)