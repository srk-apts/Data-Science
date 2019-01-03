require(latticeExtra)
require(corrgram)
require(caret)
require(pROC)
require(randomForest)
require(nnet)
require(e1071)
library(devtools)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/e297a212033087021bdd770625a0f09024a22882/nnet_plot_update.r')
source_url('http://www.stat.wmich.edu/wang/685/Rcodes/pnlcorg.R')
### South African Heart Disease Data (SAheart.data)
##A retrospective sample of males in a heart-disease high-risk region
##of the Western Cape, South Africa. There are roughly two controls per
##case of CHD. Many of the CHD positive men have undergone blood
##pressure reduction treatment and other programs to reduce their risk
##factors after their CHD event. In some cases the measurements were
##made after these treatments
##sbp systolic blood pressure
##tobacco cumulative tobacco (kg)
##ldl low densiity lipoprotein cholesterol
##adiposity
##famhist family history of heart disease (Present, Absent)
##typea type-A behavior
##obesity
##alcohol current alcohol consumption
##age age at onset
##chd response, coronary heart disease
#seed = 12345
#set.seed(seed)

## READ DATA

ESLdata <- "http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets"
SAheart <- read.csv(paste(ESLdata,"SAheart.data",sep="/"),
                    row.names=1)
SAheart$chd <- factor(SAheart$chd,labels=c("no","yes"))
rm(ESLdata)

##SPLIT

nrow(SAheart)
with(SAheart, table(chd))
with(SAheart, table(famhist,chd))
(sizes <- round(with(SAheart,table(famhist,chd))*.4))
train <- NULL
for(f in levels(SAheart$famhist))
  for(c in levels(SAheart$chd)){
    s <- with(SAheart,which((famhist==f)&(chd==c)))
    train <- c(train,sample(s,sizes[f,c]))
  }
heart.train <- SAheart[train,]
heart.test <- SAheart[-train,]
rm(sizes,train,f,c,s)

######## DESCRIPTIVE STATISTICS #########
#Coronary Heart Disease Barplot

ylim <- c(0, 1.2*max(as.numeric(table(SAheart[10]))))
xx <- barplot(table(SAheart[10]), width = 0.85, ylim = ylim, ylab ="Number of Cases")
text(x =xx, y = as.numeric(table(SAheart[10])), label = as.numeric(table(SAheart[10])),
     pos = 3, cex = 0.8, col = "red")
title("Barplot for South African Coronary Heart Disease")
rm(xx,ylim)
STAT 6850 - Case Study 4
#Mosaic plots for Categorical Attributes
mosaicplot(~ famhist + chd, data=SAheart, main="Mosaic Plot of Heart Disease vs Family History",
           xlab = "Family History", ylab="Coronary Heart Disease")
#Boxplots for other attributes
plot.new()
title(main = "\nSouth African Coronary Heart Disease Attributes Boxplots", outer=T)
plot(bwplot(sbp ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Systolic Blood Pressure"),
     split = c(1, 1, 4, 2), newpage = FALSE)
plot(bwplot(tobacco ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Cumulative Tobacco (kg)"),
     split = c(2, 1, 4, 2), newpage = FALSE)
plot(bwplot(ldl ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="LDL cholesterol"),
     split = c(3, 1, 4, 2), newpage = FALSE)
plot(bwplot(adiposity ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Adiposity"),
     split = c(4, 1, 4, 2), newpage = FALSE)
plot(bwplot(typea ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Type-A behavior"),
     split = c(1, 2, 4, 2), newpage = FALSE)
plot(bwplot(obesity ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Obesity"),
     split = c(2, 2, 4, 2), newpage = FALSE)
plot(bwplot(alcohol ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Current Alcohol Consumption"),
     split = c(3, 2, 4, 2), newpage = FALSE)
plot(bwplot(age ~ chd, data = SAheart, pch="|",
            xlab = "Coronary Heart Disease", ylab="Age"),
     split = c(4, 2, 4, 2), newpage = FALSE)
#Correlation Tests
(cormatrix <- cor(SAheart[-c(5,10)],SAheart[-c(5,10)], use="p"))
ord <- order.dendrogram(as.dendrogram(hclust(dist(cormatrix))))
levelplot(cormatrix[ord,ord], at=do.breaks(c(-1.01,1.01),20),
          xlab=NULL, ylab=NULL, # no axes labels
          main="Correlation Matrix South African Coronary Heart Disease",
          scales=list(x=list(rot=90)),
          panel=panel.corrgram, # use correlogram panel function
          label=TRUE, # with labels of correlations x 100
          col.regions=colorRampPalette(c("red","white","blue")),
          colorkey=list(space="top")) # using color key on the top
rm(ord)
cat("Correlation Matrix\n",file = "exploratory.csv")
cat( "\t", colnames(cormatrix), "\n", file = "exploratory.csv", sep = ",",append=T)
write.table(cormatrix, file = "exploratory.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
(findCorrelation(cor(SAheart[-c(5,10)]), 0.9)) #Multicollinearity not significantly present

######## DATA PRE-PROCESSING ########
#Near Zero Variance

dim(SAheart)
(nzv <- nearZeroVar(SAheart[-10], saveMetrics = TRUE))
STAT 6850 - Case Study 4
cat("\n" , "NearZeroVariance Table\n",file = "exploratory.csv", append=T)
cat("\t", colnames(nzv), "\n", file = "exploratory.csv", sep = ",",append=T)
write.table(nzv, file = "exploratory.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
#Scaling and Centering
str(heart.train)
preproc <- function( x, z, excludeClasses=c("factor"), ... ) {
  whichToExclude <- sapply( x, function(y) any(sapply(excludeClasses, function(excludeClass) is(y,excludeClass) )) )
  processedMat <- predict( preProcess( x[!whichToExclude], ...), newdata=z[!whichToExclude] )
  z[!whichToExclude] <- processedMat
  z
}
proc.full <- preproc(heart.train, SAheart)
proc.train <- preproc(heart.train, heart.train)
proc.test <- preproc(heart.train, heart.test)
#Variable Importance
ctrl <- trainControl(verboseIter = FALSE, classProbs = TRUE)

#Random Forest

#set.seed(seed)
rfFit <- randomForest(chd~., data=proc.train, ntree= 2000, importance=T)
(rfimp <- varImp(rfFit))
rfimp <- rfimp[order(row.names(rfimp)),][1]
colnames(rfimp) <- "Random Forest"
print(rfimp)
#Partial Least Squares
#set.seed(seed)
plsFit <- train(chd~., data=proc.train, method = "pls",
                tuneGrid = data.frame(.ncomp = 1:9), trControl = ctrl)
(plsimp <- varImp(plsFit$finalModel))
plsimp <- data.frame(plsimp[order(rownames(plsimp)),])
rownames(plsimp) <- row.names(rfimp)
colnames(plsimp) <- "PLS"
print(plsimp)

#Generalized boosting model
#set.seed(seed)
gbmGrid <- expand.grid(interaction.depth = c(1, 3, 5, 7),
                       n.trees = c(50,500, 1000),
                       shrinkage = 0.1)
gbmFit <- train(chd~.,data=proc.train, method = "gbm",
                tuneGrid = gbmGrid, verbose = FALSE)
(gbmimp <- varImp(gbmFit$finalModel))
gbmimp <- data.frame(gbmimp[order(rownames(gbmimp)),])
rownames(gbmimp) <- row.names(rfimp)
colnames(gbmimp) <- "GBM"
print(gbmimp)

#MARS model
eaFit <- bagEarth(chd~., data=proc.train, glm=list(family=binomial))
(eaimp <- varImp(eaFit, value = "gcv"))
eaimp["famhistPresent",] = sum(eaimp[c("famhistAbsent","famhistPresent"),])
eaimp <- data.frame(eaimp[order(rownames(eaimp)),])
eaimp <- data.frame(eaimp[-5,])
rownames(eaimp) <- row.names(rfimp)
colnames(eaimp) <- "MARS"
print(eaimp)
STAT 6850 - Case Study 4

#Model Free
(ROCFit <- filterVarImp(proc.train[-10], proc.train[[10]]))
ROCimp <- ROCFit[order(row.names(ROCFit)),][1]
colnames(ROCimp) <- "ROC"
print(ROCimp)
(Imp <- cbind(rfimp, plsimp, gbmimp, eaimp, ROCimp))
cat("\n" , "Variable Importance Table\n",file = "exploratory.csv", append=T)
cat("\t", colnames(Imp), "\n", file = "exploratory.csv", sep = ",",append=T)
write.table(Imp, file = "exploratory.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
#Var Importance plots
splom(Imp, type = c("p", "r"),
      main = "Variable Importance: All Predictors")
splom(Imp, type = c("p", "smooth"),
      main = "Variable Importance: All Predictors")
ntop <- 9
n <- 5
vars <- NULL # vector to collect 3 list of variable names, initialized
hasMore <- c(rep(T,n-1),F)
for(i in 1:n){ # for each method
  ord <- order(Imp[,i], decreasing=TRUE)[ntop:1]
  x <- Imp[ord,i]
  y <- rownames(Imp)[ord] vars <- c(vars, y)
  y <- factor(y, levels=y) plot(dotplot(y ~ x, type=c("p","h"), xlab="variable\nImportance",
                                        main=names(Imp)[i]),
                                split=c(i,1,n,1), more=hasMore[i])
}
rm(ntop,vars,hasMore,i,x,y,ord, n)

#Subsetting by Var Importance
(Impsc <- preproc(Imp, Imp))
x <- as.data.frame(c(rep("Random Forest",9),rep("PLS",9),rep("GBM",9),rep("MARS",9),rep("ROC",9)))
x[2] <- factor(rep(rownames(Impsc),5))
x[3] <- c(Impsc[[1]],Impsc[[2]],Impsc[[3]],Impsc[[4]],Impsc[[5]])
colnames(x) <- c("Method", "Predictor", "varImp")
(y <- as.data.frame(sort(rowMeans(-Impsc))))
x$Predictor <- factor(x$Predictor, levels=rownames(y))
dotplot(varImp ~ Predictor, data = x, groups = Method,
        main = list("Scaled Variable Importance by Method", cex=1.6),
        par.settings = list(superpose.symbol = list(pch = 15:19, cex=1.2),
                            box.rectangle = list(col="black",alpha=0.2),
                            box.umbrella = list(col="black",alpha=0.2)),
        auto.key=list(columns = 5, cex=1.2, pch = 15:19),
        cex=1.2, scales=list(cex=1.2), alpha=0.8,
        ylab="Scaled Variable Importance",
        panel = function(...) {
          panel.dotplot(...)
          panel.abline(h=0, col=2, lty=2)
          panel.bwplot(...,pch=8, col="black")
        })
(include <- rownames(which(y < 0, arr.ind=TRUE)))
imp.full <- proc.full[c(include,"chd")]
imp.train <- proc.train[c(include,"chd")]
imp.test <- proc.test[c(include,"chd")]
rm(x,y,include)
STAT 6850 - Case Study 4

################# NEURAL NETWORK #####################
#set.seed(seed)
NN <- nnet(chd ~ ., data=imp.train, size=5, Hess=T,
           decay=0.05, rang=0.1, maxit=200)
summary(NN)
NNCM <- confusionMatrix(imp.test[["chd"]], predict(NN, imp.test, type="class"))
NNCM$table
NNCMres <- (cbind(NNCM$overall["Accuracy"],cbind(sensitivity(NNCM$table),specificity(NNCM$table))))
colnames(NNCMres) = c("Accuracy","Sensitivity","Specificity")
rownames(NNCMres) = "Results"
print(NNCMres)
(NNHess <- eigen(NN$Hess, only.values=T)$values)
cat("\n", "Neural Network Prediction Matrix\n", file = "NN.csv")
cat( "\t", colnames(NNCM$table), "\n", file = "NN.csv", sep = ",", append=T)
write.table(NNCM$table, file = "NN.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Neural Network Test Prediction Results\n", file = "NN.csv", append = T)
cat( "\t", colnames(NNCMres), "\n", file = "NN.csv", sep = ",", append=T)
write.table(NNCMres, file = "NN.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Neural Network Hessian Evaluation\n", file = "NN.csv", append = T)
write.table(NNHess, file = "NN.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
#Neural Network Plot
plot(NN, main ="South African Heart Disease Neural Network")
#Neural Network X-Val
fitControl <- trainControl(method = "repeatedcv",
                           number = 10, repeats = 5,
                           classProbs = TRUE)
nnGrid <- expand.grid(size=1:7, decay=5*10^(-(4:1)))
#set.seed(seed)
NNXV <- train(chd ~ ., data=imp.train, method="nnet",
              trace=F,
              tuneGrid=nnGrid,
              trControl=fitControl)
summary(NNXV)
trellis.par.set(caretTheme())
plot(NNXV, main ="10-fold Cross-Validated Neural Network Parameters")
NNXVCM <- confusionMatrix(imp.test[["chd"]], predict(NNXV, imp.test))
NNXVCM$table
NNXVCMres <- (cbind(NNXVCM$overall["Accuracy"],cbind(sensitivity(NNXVCM$table),specificity(NNXVCM$table))))
colnames(NNXVCMres) = c("Accuracy","Sensitivity","Specificity")
rownames(NNXVCMres) = "Results"
print(NNXVCMres)
cat("\n", "X-Val Neural Network Prediction Matrix\n", file = "NN.csv", append = T)
cat( "\t", colnames(NNXVCM$table), "\n", file = "NN.csv", sep = ",", append=T)
write.table(NNXVCM$table, file = "NN.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "X-Val Neural Network Test Prediction Results\n", file = "NN.csv", append = T)
cat( "\t", colnames(NNXVCMres), "\n", file = "NN.csv", sep = ",", append=T)
write.table(NNXVCMres, file = "NN.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
#NN X-val Plot
plot.nnet(NNXV, main ="SA Heart Disease X-Val Neural Network")
STAT 6850 - Case Study 4

################# Support Vector Machines #####################

svm1 <- svm(chd ~ ., data=imp.train, cost=100, gamma=1)
SVMCM <- confusionMatrix(imp.train[["chd"]], predict(svm1, imp.train))
SVMCM$table
SVMCMres <- (cbind(SVMCM$overall["Accuracy"],cbind(sensitivity(SVMCM$table),specificity(SVMCM$table))))
colnames(SVMCMres) = c("Accuracy","Sensitivity","Specificity")
rownames(SVMCMres) = "Radial"
svm2 <- svm(chd ~ ., data=imp.train, cost=100, gamma=1,
            kernel='sigmoid', type='C-classification')
SVMCM2 <- confusionMatrix(imp.train[["chd"]], predict(svm2, imp.train))
SVMCM2$table
SVMCM2res <- (cbind(SVMCM2$overall["Accuracy"],cbind(sensitivity(SVMCM2$table),specificity(SVMCM2$table))))
colnames(SVMCM2res) = c("Accuracy","Sensitivity","Specificity")
rownames(SVMCM2res) = "Sigmoid"
svm3 <- svm(chd ~ ., data=imp.train, cost=100, gamma=1,
            kernel='linear', type='C-classification')
SVMCM3 <- confusionMatrix(imp.train[["chd"]], predict(svm3, imp.train))
SVMCM3$table
SVMCM3res <- (cbind(SVMCM3$overall["Accuracy"],cbind(sensitivity(SVMCM3$table),specificity(SVMCM3$table))))
colnames(SVMCM3res) = c("Accuracy","Sensitivity","Specificity")
rownames(SVMCM3res) = "Linear"
svm4 <- svm(chd ~ ., data=imp.train, cost=100, gamma=1,
            kernel='polynomial', type='C-classification')
SVMCM4 <- confusionMatrix(imp.train[["chd"]], predict(svm4, imp.train))
SVMCM4$table
SVMCM4res <- (cbind(SVMCM4$overall["Accuracy"],cbind(sensitivity(SVMCM4$table),specificity(SVMCM4$table))))
colnames(SVMCM4res) = c("Accuracy","Sensitivity","Specificity")
rownames(SVMCM4res) = "Polynomial"
(allsvm <- cbind(t(SVMCMres), t(SVMCM2res), t(SVMCM3res), t(SVMCM4res)))
cat("\n" , "Support Vector Machine Models\n",file = "SVM.csv")
cat("\t", colnames(allsvm), "\n", file = "SVM.csv", sep = ",",append=T)
write.table(allsvm, file = "SVM.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
#PLOT KERNEL MODELS
xx <- colnames(allsvm)
oldpar <- par(mfrow=c(2,2), mar=c(4.1,4.1,1.5,0.5))
plot(allsvm[1,], pch=16, type="o", col="red", ylab=colnames(allsvm)[1],
     xaxt = "n", xlab = "", main = "Kernel Accuracy Analysis")
axis(1, 1:4, labels= xx)
plot(allsvm[2,], pch=16, type="o", col="red", ylab=colnames(allsvm)[2],
     xaxt = "n", xlab = "", main = "Kernel Sensitivity Analysis")
axis(1, 1:4, labels= xx)
plot(allsvm[3,], pch=16, type="o", col="red", ylab=colnames(allsvm)[3],
     xaxt = "n", xlab = "", main = "Kernel Specificity Analysis")
axis(1, 1:4, labels= xx)
oldpar <- par(mfrow=c(1,1), mar=c(4.1,4.1,1.5,0.5))
STAT 6850 - Case Study 4

#####Tuning SVM

#Radial
svmkernel <- "radial"
set.seed(12345)
svmTuned <- tune(svm, chd ~ ., data=imp.train, kernel=svmkernel,
                 ranges = list(gamma=2^(-1:1), cost = 1:50),
                 tunecontrol = tune.control(sampling = "fix"))
head(summary(svmTuned))
plot(svmTuned, color.palette=heat.colors)
set.seed(12345)
svmTuned <- tune(svm, chd ~ ., data=imp.train, kernel=svmkernel,
                 ranges = list(gamma=2^(seq(-6, 0, .25)), cost = 1:10),
                 tunecontrol = tune.control(sampling = "fix"))
head(summary(svmTuned))
plot(svmTuned, color.palette=heat.colors)
plot(svmTuned,type='p',theta=240,phi=0)
#Best Tune
set.seed(12345)
svmopt <- best.tune(svm, chd ~ ., data=imp.train, kernel=svmkernel,
                    ranges = list(gamma=2^(seq(-6, 0, .25)), cost = 1:10),
                    tunecontrol = tune.control(sampling = "fix"))
summary(svmopt)
SV <- 1:184 %in% svmopt$index + 1 # 1=non Support Vector, 2=Support Vector
pairs(imp.train[-5], col=as.integer(imp.train[[5]])+1,
      pch=c(1,3)[SV],cex=c(1,1.25)[SV], main = "Support Vectors by Paired Attributes")
legend(locator(1), xpd = TRUE, horiz = F, lwd=1, pch = c(1,1), bty='n', text.width=0.2,
       cex=0.9, legend=c("No","Yes"))
# SVM Test Set Prediction
SVMBest <- confusionMatrix(imp.test[["chd"]], predict(svmopt, imp.test))
SVMBest$table
SVMBestres <- (cbind(SVMBest$overall["Accuracy"],cbind(sensitivity(SVMBest$table),specificity(SVMBest$table))))
colnames(SVMBestres) = c("Accuracy","Sensitivity","Specificity")
rownames(SVMBestres) = "Radial"
print(SVMBestres)
cat("\n", "Tuned SVM Test Prediction Matrix\n", file = "SVM.csv", append = T)
cat( "\t", colnames(SVMBest$table), "\n", file = "SVM.csv", sep = ",", append=T)
write.table(SVMBest$table, file = "SVM.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)
cat("\n", "Tuned SVM Test Prediction Results\n", file = "SVM.csv", append = T)
cat( "\t", colnames(SVMBestres), "\n", file = "SVM.csv", sep = ",", append=T)
write.table(SVMBestres, file = "SVM.csv", sep = ",",
            qmethod = "double", append = T, col.names=F)