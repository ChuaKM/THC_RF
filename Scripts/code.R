#
# read in previously generated medical data
# and run Cox proportion hazards regression
# in R
# 
options(repos=structure(c(CRAN="http://cran.rstudio.com/")))
# install.packages("survival")
options(scipen=999)
library(survival)
rm(list=ls())

# init vars
rootDir <- "/Users/jackchua/Documents/koon_project/"
dataDir <- paste(rootDir, "Data/", "averages.dat", sep="")

# read data and drop first observation
smokerData <- read.table(dataDir, sep="\t", header=TRUE)
colnames(smokerData) <- c("y", "THC", "ElevenOHTHC", "THCCOOH")
smokerData <- smokerData[0:-1,]

# TODO: run regular regression as specified in the
# Huestis et al study
# 1) truncated from only obs where y=0 to y=12
truncatedSmokerData <- smokerData[smokerData$y<=12,]
truncatedSmokerData$ratio <- truncatedSmokerData$THCCOOH / truncatedSmokerData$THC
model1 <- lm(log(y) ~ log(ratio), data=truncatedSmokerData)

# plots
#plot(log(smokerData[,1]), smokerData[,2], type="l", xlab="lg T", ylab="Plasma Concentration (micrograms per liter)")
#lines(log(smokerData[,1]), smokerData[,3], type="l", col="green")
#lines(log(smokerData[,1]), smokerData[,4], type="l", col="red")

### RANDOM FOREST

# first, just plot a damn tree
library(randomForest)
aTree <- tree(log(y) ~ THC+THCCOOH+ElevenOHTHC, data=smokerData)
plot(aTree)
text(aTree, cex=0.75)

# fit random forest
smokerData$ratio <- smokerData$THCCOOH / smokerData$THC
smokerData$log_ratio <- log(smokerData$THCCOOH / smokerData$THC)
smokerData$THC_THCCOOH <- smokerData$THC*smokerData$THCCOOH
smokerData$ElevenOHTHC_THCCOOH <- smokerData$ElevenOHTHC*smokerData$THCCOOH
smokerData$THC_ElevenOHTHC <- smokerData$THC*smokerData$ElevenOHTHC

randomForestRes <- randomForest(log(y) ~ THC+THCCOOH+ElevenOHTHC, data=smokerData)
randomForestRes2 <- randomForest(log(y) ~ THC+THCCOOH+ElevenOHTHC+log_ratio, smokerData)