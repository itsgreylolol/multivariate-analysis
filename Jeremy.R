workingDir <- "D:/School/multivariate-analysis/data"

setwd(workingDir)
mydata <- read.csv("Final.csv")
mydata.cln <- na.omit(mydata)
mydata.num <- mydata.cln[-c(1, 2, 3)]
write.csv(round(cor(mydata.num), 3), "../output/cor.csv")