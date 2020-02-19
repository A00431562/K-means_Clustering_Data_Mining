library(ggplot2)
library(GGally)
library(DMwR)
library(dplyr)
#install.packages("dplyr")

getwd()
setwd("C:/Users/sidha/Documents/SMU/Semester 2/Data Mining/Assignments/Assignments Combined/Assignment 1/Assignment 1")


set.seed(55)
prod = read.csv("ProductClusterNew.csv",sep = ',')

prod.filter <- prod %>% filter(Revenue!= 0 & PricePerinvoice!= 0)
View(prod.filter)
View(prod)
ggpairs(prod.filter[, which(names(prod.filter) != "StockCode")], 
        upper = list(continuous = ggally_points), lower = list(continuous = "points"), title = "Products before outlier removal")

prod.clean <-prod.filter[prod.filter$StockCode != 22423, ]
View(prod.clean)

ggpairs(prod.clean[,which(names(prod.clean)!="StockCode")], 
        upper = list(continuous = ggally_points), lower = list(continuous = "points"), title = "Products after  removing outlier")


prod.scale = scale(prod.clean[-1])
View(prod.scale)

withinSSrange <-function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <-kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}

plot(withinSSrange(prod.scale,1,50,150))


set.seed(55)
pkm = kmeans(prod.scale, 5, 150)
pkm$tot.withinss
View(pkm)

prod.realCenters = unscale(pkm$centers,prod.scale)
View(prod.realCenters)
clusteredprod = cbind(prod.clean, pkm$cluster)
plot(pkm$cluster)


plot(clusteredprod[,2:7], col=pkm$cluster)
write.csv(clusteredprod, file = "FinalResultsagain.csv", col.names = FALSE)

