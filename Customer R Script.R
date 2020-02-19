# R Script for K means clustering of Customers
#Import libraries
library(ggplot2)
library(GGally)
library(DMwR)


getwd()
setwd("C:/Users/sidha/Documents/SMU/Semester 2/Data Mining/Assignments/Assignments Combined/Assignment 1/Assignment 1")


#Import Customer dataset
Customer <- read.csv("Customer.csv", header = T)

#Plot to view the data and check outliers
ggpairs(Customer[, which(names(Customer) != "CustomerID")],
        upper = list(continuous = ggally_points),lower = list(continuous = "points"),
        title = "Customer before outlier removal")

#View(Customer[, which(names(Customer) != "CustomerID")])
#Preparing Dataset with only Fact columns for K-means
Cust <- data.frame(Customer$NoOfDistinctProducts,Customer$NoOfProducts,Customer$Revenue,Customer$Visits,Customer$AvgBasketValue,Customer$AvgBasketSize,Customer$Recency)

#Scaling attributes
scaledCust <- scale(Cust)


#Function to determine the K value
withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}

#Passing the parameters in the above function and getting the elbow curve
plot(withinSSrange(scaledCust,1,20,150))

#Set seed value
set.seed(5580)

#Considering k =7
results <- kmeans(scaledCust,7,150)

#View the clusters
results

results$withinss

results$iter

results$ifault

#Plot the clusters
plot(results$cluster)


#Binding the customers to the actual dataset
clusteredCustomer = cbind(Customer, results$cluster)
View(clusteredCustomer)


#Grid view of the customers
plot(clusteredCustomer[,2:8], col=results$cluster)

#Writing the new dataset with clusters into a csv file
write.csv(clusteredCustomer,file ='clusteredcustomer_new.csv')
