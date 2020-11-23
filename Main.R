workingDir <- "D:/School/multivariate-analysis/data"

setwd(workingDir)
mydata <- read.csv("Final.csv")
mydata.cln <- na.omit(mydata)
mydata.num <- mydata.cln[-c(1, 2, 3, 4)]

pca <- princomp(mydata.num,
                cor=T)
pca$loadings

mydata.scale <- scale(mydata.num)
d <- 1-cor(mydata.num)
cmd <- cmdscale(d)
cmd
  
plot(cmd,
     pch = " ")
text(cmd,
     labels = rownames(d))

cor <- cor(mydata.num,
           use = "pairwise.complete.obs")

efa <- factanal(covmat = cor, 
                factors = 3, 
                n.obs = nrow(mydata.num))

print(efa$loadings, 
      cut="0.25")

# HCL is limited to 65536 observations, so we cannot complete that.

plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc) 
    wss[i] = kmeans(mydata,centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot") 
}

plot.wgss(mydata.scale, 10)

km <- kmeans(mydata.scale, 5)
table(km$cluster)

library(mclust)
mc <- Mclust(mydata.scale)