title: "Iris Cluster Analysis"

----
  library(ggplot2)
library(readr)
library(cluster)
library(factoextra)
library(tidyverse)
library(dplyr)

#import Iris dataset 
data <- read_csv("~/Iris.csv") 

#view the structure of the dataset 
head(data)

#CLEAN DATA

#remove na values 
data <- na.omit(data)

# view scatter plots of different species to check factors that have biggest variance  

ggplot(data, aes(SepalLengthCm, SepalWidthCm, color = Species)) + geom_point()

ggplot(data, aes(SepalLengthCm, PetalWidthCm, color = Species)) + geom_point()

ggplot(data, aes(SepalLengthCm, PetalLengthCm, color = Species)) + geom_point()

ggplot(data, aes(PetalWidthCm, PetalLengthCm, color = Species)) + geom_point()

###more data cleaning
#remove species column 

data1 <- subset(data, select = -Species)

#scale/standardize data so that variables are on comparable scales(distance)

data1 <- scale(data1)
data_1 <- as.data.frame(data1)

### BUILD CLUSTERS 

#pick k = 3 cause we already know only 3 types of Iris flowers exist
#else we would have to use the Elbow Method to find an optimal value of k 

set.seed(123)

# function to compute total within-cluster sum of squares(wss) 
wss <- function(k) {
  kmeans(data_1, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 15
k_values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k_values, wss)

plot(k_values, wss_values,
     type="b", pch = 17, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# above plot also confirms that k = 3 is the optimal and (sensible in this case) number of clusters! 

iris_clusters <- kmeans(data_1, centers = 3, nstart = 25)

fviz_cluster(iris_clusters, data = data_1) + ggtitle(" Clusters with labels")

# add cluster column to initial data set to check how well the clustering algorith grouped the flowers

final_dataset <- data %>% 
  mutate(cluster = iris_clusters$cluster)

# this kmeans alogirthm incorrectly classified 6 flowers out of 150
# 96%  accuracy 




