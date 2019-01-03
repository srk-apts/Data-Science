require(latticeExtra)
require(corrgram)
require(caret)
require(MASS)
require(cluster)
require(useful)
require(clValid)
library(devtools)

source_url('http://www.stat.wmich.edu/wang/685/Rcodes/pnlcorg.R')
source_url('https://raw.githubusercontent.com/Altons/Rlib/master/multiKmeans.R')
source_url('https://raw.githubusercontent.com/Altons/Rlib/master/elbowGraph.R')
source_url('https://raw.githubusercontent.com/Altons/Rlib/master/plot.kmeans2.R')

## Perform the clustering of the observations using both hierarchical clustering methods and K-means clustering.
##
##
##

### READ DATA AND SCALE ###
uci <- "http://archive.ics.uci.edu/ml/machine-learning-databases"
dir <- "water-treatment"
dname <- "water-treatment.data"
water <- read.csv(paste(uci, dir, dname, sep="/"),
                  header=F, # without header
                  na.strings="?", # in which missing value code is ?
                  strip.white=T, # strip-off white spaces
                  row.names=1) # use first field as row names
rm(dir,dname,uci)
colnames(water) = scan(what=' ', nmax=38)
QE ZNE PHE DBOE DQOE SSE SSVE SEDE CONDE PHP
DBOP SSP SSVP SEDP CONDP PHD DBOD DQOD SSD SSVD
SEDD CONDD PHS DBOS DQOS SSS SSVS SEDS CONDS RDDBOP
RDSSP RDSEDP RDDBOS RDDQOS RDDBOG RDDQOG RDSSG RDSEDG
water2 <- subset(water, subset=complete.cases(water))
water2 <- scale(water2)

########## DESCRIPTIVE STATISTICS #############
#Histograms
histogram(~CONDE+PHP+DBOP+DQOE+QE+PHE+DBOE+CONDD+DBOD+DQOD+SSD+SSVD,
          data=subset(water, subset=complete.cases(water)),
          type="density",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.rug(x, ...)
            panel.mathdensity(dmath = dnorm, col = "red",
                              args = list(mean=mean(x),sd=sd(x)))},
          scales=list(relation="free"), breaks=NULL, main ="Histograms for Water Treatment Variables")
histogram(~SSVP+CONDP+PHD+RDDQOG+RDDBOP+RDDQOS+DQOS+SSVS+CONDS+RDSSP+SSVE+PHS,
          data=subset(water, subset=complete.cases(water)),
          type="density",
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.rug(x, ...)
            panel.mathdensity(dmath = dnorm, col = "red",
                              args = list(mean=mean(x),sd=sd(x)))},
          scales=list(relation="free"), breaks=NULL, main ="Histograms for Water Treatment Variables")
STAT 6850 - Case Study 5
histogram(~SSP+SSE+SEDE+ZNE+SEDD+DBOS+SEDP+SSS+SEDS,
          data=subset(water, subset=complete.cases(water)),
          type="density", layout=c(3,3),
          panel = function(x, ...) {
            panel.histogram(x, ...)
            panel.rug(x, ...)
            panel.mathdensity(dmath = dexp, col = "red",
                              args = list(rate=(fitdistr(x,"exponential")$estimate)))},
          scales=list(relation="free"), breaks=NULL, main ="Histograms for Water Treatment Variables")
histogram(~RDDBOG+RDSSG+RDSEDP+RDDBOS,
          data=subset(water, subset=complete.cases(water)),
          type="density",
          scales=list(relation="free"), breaks=NULL, main ="Histograms for Water Treatment Variables")
#Correlation Tests
(cormatrix <- cor(water2,water2, use="p"))
ord <- order.dendrogram(as.dendrogram(hclust(dist(cormatrix))))
levelplot(cormatrix[ord,ord], at=do.breaks(c(-1.01,1.01),20),
          xlab=NULL, ylab=NULL, # no axes labels
          main="Water Treatment Plan Data Set Correlation Matrix",
          scales=list(x=list(rot=90)),
          panel=panel.corrgram, # use correlogram panel function
          label=TRUE, # with labels of correlations x 100
          col.regions=colorRampPalette(c("red","white","blue")),
          colorkey=list(space="top")) # using color key on the top
(corrcol <- colnames(water[(findCorrelation(cormatrix, 0.9))])) #Multicollinearity Present
nearZeroVar(water2) #No low variability

############# CLUSTERING PROCEDURES ###################
### Hierarchical clustering procedures ####

# Agglomerative Clustering Procedures for distance-linkage combinations
distances = c("euclidean","manhattan","gower")
linkage = c("single", "complete", "average", "centroid", "ward.D2")
fitlabels <- matrix(0,5,3)
for(i in 1:5){
  for(j in 1:3){
    dist <- daisy(water2, metric = distances[j])
    fit <- paste("fit",i,j,sep="")
    fitlabels[i,j]=paste(linkage[i], "Linkage Clustering using", distances[j], "distance (",fit,")")
    plot(assign(fit, hclust(dist, method=linkage[i])),
         hang=-1,labels = FALSE,
         main=fitlabels[i,j])
  }}
rm(fit)
good = list(fit51,fit52,fit53)
goodlabels = c(fitlabels[5,1],fitlabels[5,2],fitlabels[5,3])
for (i in 1:length(good)){
  for(nk in 3:8){
    plot(good[[i]], hang=-1,labels = FALSE,
         main=paste(goodlabels[i], " k=", nk))
    rect.hclust(good[[i]],k=nk,border=2:7)
  }}
d <- daisy(water2, metric = "gower")
plot(silhouette(cutree(fit53,k=5),d), main = "Ward-Gower Silhouette Plot with K=5")
STAT 6850 - Case Study 5
# Divisive Clustering Procedure-Diana (R command: diana)
dialabels <- matrix(0,3,1)
for(i in 1:3){
  dist <- daisy(water2, metric = distances[i])
  dia <- paste("dia",i,sep="")
  dialabels[i,1]=paste("Divisive Clustering using", distances[i], "distance (",dia,")")
  plot(assign(dia, diana(dist)),
       which=2, labels = FALSE,
       main=dialabels[i,1])
}
rm(dia)
good = list(dia3)
goodlabels = c(dialabels[3,1])
for (i in 1:length(good)){
  for(nk in 3:8){
    plot(good[[i]], hang=-1,labels = FALSE, which=2,
         main=paste(goodlabels[i], " k=", nk))
    rect.hclust(good[[i]],k=nk,border=2:7)
  }}

################ Nonhierarchical Clustering Procedures ################
# K-means clustering (R command: kmeans)

error = matrix(0,14,2)
for(k in 2:15){
  set.seed(123456)
  kfit <- paste("kfit",k,sep="")
  a<- assign(kfit,kmeans(water2, centers = k,
                         iter.max = 20))
  error[k-1,1]=k
  error[k-1,2]=as.numeric(a[6])/as.numeric(a[3])
}
rm(kfit)
colnames(error)=c("K-clusters","Between Cluster SS Proportion")
cat("\t" , "K-Means Cluster Analysis\n",file = "results.csv")
cat("\t", colnames(error), "\n", file = "results.csv", sep = ",",append=T)
write.table(error, file = "results.csv", sep = ",",append=T,
            qmethod = "double",col.names = F)
plot(error, pch=16, type="o", col="red", main = "K-Cluster Between Sum of Squares Proportion")
pairs(water2[,c(1,2,4,14,34)], col=kfit4$cluster+1,gap=.1)
## FOR VALIDATING K CLUSTER SELECTION
cl = multiKmeans(water2, 20, 200)
(elbowGraph(cl$css))
clus = cl$cluster[[3]]
plot.kmeans2(clus, data = water2)
## Partition Around Medoids (PAM)
opar <- par(mar=par('mar')+c(0,3,0,0)) # add 3-line text and
pamlabels <- matrix(0,3,1)
for(i in 1:3){
  for(nk in 3:8){
    dist <- daisy(water2, metric = distances[i])
    STAT 6850 - Case Study 5
    pamfit <- paste("pamfit",i,nk,sep="")
    pamlabels[i,1]=paste("Partition Around Medoids with", distances[i], "distance with k=", nk, "(",pamfit,")")
    clusplot(assign(pamfit, pam(dist, k=nk)), main=pamlabels[i,1])
  }}
rm(pamfit)
plot(pamfit17,max.strlen=15, main = "PAM - Euclidean Silhouette Plot with K=7")
par(opar) # resume old par

############ CLUSTER ANALYSIS ############

summary(clValid(water2, 3:8, clMethods = c("hierarchical","diana", "kmeans", "pam"),
                metric = "euclidean", method=c("ward", "single", "complete", "average"), validation = "internal"))
summary(clValid(water2, 3:8, clMethods = c("hierarchical","diana", "kmeans", "pam"),
                metric = "manhattan", method=c("ward", "single", "complete", "average"), validation = "internal"))