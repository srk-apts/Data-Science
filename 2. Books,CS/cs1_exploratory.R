## Data Frame creation from tables

vertebral <- read.table("column_2C.dat", header=F)
colnames(vertebral) <- c('pincidence','ptilt','angle','slope',
                         'pradius','grade','class2')
vertebral$class3 <- read.table("column_3C.dat", header=F)[[7]]
head(vertebral)
summary(vertebral)

##Identifying Outliers

require(outliers)
which(abs(scores(vertebral[1:6], type="iqr")) > 3, arr.ind =T)
vertebral[116,c(4,6)]
vertebral2 <- vertebral[-116,]

##Summary Stats

summary(vertebral2)

##Correlation Matrix
(cormatrix <- cor(vertebral2[1:6],vertebral2[1:6]))
require(corrgram)
corrgram(vertebral2, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
title("Correlation Matrix \nVertebral Column Biomechanical Attributes", line = 2)

##Boxplot

boxplot(vertebral2[1:6], main = "Box Plot for Vertebral Column Data Set from UCI",
        xlab = "Biomechanical Attributes")
#Class2
oldpar <- par(mfrow=c(2,3), mar=c(4.1,4.1,0.5,0.5), oma=c(0,0,2,0))
boxplot(pincidence ~ class2, data=vertebral2, ylab="Pincidence")
boxplot(ptilt ~ class2, data=vertebral2, ylab="Ptilt")
boxplot(angle ~ class2, data=vertebral2, ylab="Angle")
boxplot(slope ~ class2, data=vertebral2, ylab="Slope")
boxplot(pradius ~ class2, data=vertebral2, ylab="Pradius")
boxplot(grade ~ class2, data=vertebral2, ylab="Grade")
title(main="Box Plot for Vertebral Data by Class", outer=T, line=0, cex.main=2)
#Class3
boxplot(pincidence ~ class3, data=vertebral2, ylab="Pincidence")
boxplot(ptilt ~ class3, data=vertebral2, ylab="Ptilt")
boxplot(angle ~ class3, data=vertebral2, ylab="Angle")
boxplot(slope ~ class3, data=vertebral2, ylab="Slope")
boxplot(pradius ~ class3, data=vertebral2, ylab="Pradius")
boxplot(grade ~ class3, data=vertebral2, ylab="Grade")
title(main="Box Plot for Vertebral Data by Sub-Class", outer=T, line=0, cex.main=2)

##Barplots

cl3names <- c("Spondylolisthesis", "Normal", "Disk Hernia")
barplot(sort(table(vertebral2$class3),dec=T), names.arg = cl3names,
        main = "Barplot Vertebral Column Data Set by Class")

##Histogram

require(lattice)
histogram(~pincidence+ptilt+angle+slope+pradius+grade,data=vertebral2,
          main = "Histograms for Vertebral Column Biomechanical Attributes", xlab="")
useOuterStrips(histogram(~pincidence+ptilt+angle+slope+pradius+grade|class3,data=vertebral2,
                         type="density", panel = function(x, ...) {
                           panel.histogram(x, ...)
                           panel.rug(x, ...)
                           panel.mathdensity(dmath = dnorm, col = "red",
                                             args = list(mean=mean(x),sd=sd(x)))
                         },
                         main = "Histograms for Vertebral Biomechanical Attributes by Sub-Class", xlab=""))


##Scatter Plots Class2

cols <- c(rgb(.8,0,0),rgb(0,.8,0))
pchs <- c(1,2)
#x11()
pairs(vertebral2[1:6],col=cols[vertebral2$class2],pch=pchs[vertebral2$class2],gap=.3,cex=.8,
      main = "Scatterplots Matrix on Vertebral Column Biomechanical Attributes")
legend(.34,.05, legend=c("Abnormal","Normal"),pch=pchs,col=cols,bty="n",xpd=NA, horiz=T)

##Scatter Plots Class3 for Abnormal

abnorm <- subset(vertebral2, class3!="NO")
abnorm$class3 <- droplevels(abnorm$class3)
cols <- c(rgb(0,0.6,0.6),rgb(0.8,0,0))
pchs <- c(3,1)
#x11()
pairs(abnorm[1:6],col=cols[abnorm$class3],pch=pchs[abnorm$class3],gap=.3,cex=.8,
      main = "Scatterplots Matrix on Vertebral Column Biomechanical Attributes")
legend(.25,.05, legend=c("Disk Hernia","Spondylolisthesis"),
       pch=pchs,col=cols,bty="n",xpd=NA, horiz=T)

##Parallel Coordinates Plot

require(GGally)
ggparcoord(data = vertebral2, columns = 1:6, groupColumn = 8,
           order = "anyClass", showPoints = TRUE,
           title = "Parallel Coordinates Plot\n Vertebral Data Sub-Class Analysis",
           alphaLines = 0.3)
parallelplot(~vertebral2[1:6] | class3, data=vertebral2, layout=c(3,1),
             main="Parallel Coordinates Plot")

##3D Visualization

require(rgl)
cols3d <- c(rgb(0,0,.8,0.5),rgb(0,.8,0,0.5),rgb(.8,0,0,0.5))
plot3d(vertebral2[c(4,2,5)], size=1.5, type='s', col=cols3d[vertebral2[[8]]])