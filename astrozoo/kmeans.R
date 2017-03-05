#apply k means clustering to the training data, clustering the training observations into 75 different clusters.  
#calculate euclidean distance of each test observate against each of the 75 clusters and identify the cluster with the minimum distance from the test observation.  Do this for all 70,000 or so test observations.  The solution to each 
#final data output should be in the format acceptable Kaggle for scoring

library(pdist)
#setwd("/home/willie/Desktop/Kaggle/astrozoo") 
setwd("/home/willie/data_science/kaggle/astrozoo") #set working directory to load all that data into R memory

test_data <- read.csv("vectors_test_processed.csv", sep = ";", header = TRUE) #load test data into memory
training_data <- read.csv("vectors_training_processed.csv", sep = ";", header = TRUE) #load training data into memory
training_solutions <- read.csv("training_solutions_rev1.csv", sep = ",", header = TRUE) 

kmeansdata <- kmeans(training_data[3:402], 150, 50) #apply k means clustering algorithm to the 400 pixel variables using 100 clusters and 25 iterations of algorithm##

#Join the kmeans cluster solutions back to training data.  
training_clusters <- cbind(training_data,kmeansdata$cluster)
names(training_clusters)[1] <- "GalaxyID"

#Join the training data to the training solution by cluster id.  
cluster_solutions <- merge(training_clusters[c(1,403)], training_solutions, by = "GalaxyID")
names(cluster_solutions)[2] <- "cluster" #rename cluster column to "cluster"

#Find the average cluster solution for each of the N (75) clusters.  Use the aggregate() function of data.frame to perform "group by" operations. 
cluster_average <- aggregate(x = cluster_solutions[,3:39], by = cluster_solutions[2], FUN = mean)
names(cluster_average)[1] <- "cluster" #rename cluster column to "cluster

cluster_center <- cbind(1:75, kmeansdata$center)  #create table of cluster and centers

#just 1 to 1
x1 <- cluster_center[1,2:401] #first cluster center
x2 <- test_data[1,3:402] #first row of test data 

#just 75 to 79975
x1 <- cluster_center[,2:401] #first cluster center
x2 <- test_data[,3:402] #first row of test data 

#calculate the distances best cluster centers and test data

mydistances <- pdist(x2,x1) #pdist function to calculate the distances between two matrices (with each row representing a vector)
mydistances <- as.matrix(mydistances) #convert pdist object into a matrix/dataframe

assignment <- as.data.frame(apply(mydistances, 1, which.min)) #assign the cluster to each one
names(assignment)[1] <- "cluster" #rename column to "cluster"
assignment <- cbind(test_data[1],assignment) #add galaxy_id to assignment

#merge assignment with cluster solutions to get the final output!
final_output <- merge(assignment,cluster_average, by = "cluster")
final_output <- final_output[order(final_output$id),] #order by galaxy id

write.table(final_output, "solutions2.csv", sep=",")

x2=x2) {
for (x in 1:75) {
x1 <- cluster_center[x,2:401]
distance_vector[x] <- dist(rbind(x1,x2))[1]
distance_vector
}
}

distance_vector <- list()
distance_vector <- rep(0,75)

euc_onepass <- function(
for (x in 1:75) {
x1 <- cluster_center[x,2:401]
distance_vector[x] <- dist(rbind(x1,x2))[1]
distance_vector
}
)

system.time(
for (x in 1:75) {
x1 <- cluster_center[x,2:401]
distance_vector[x] <- dist(rbind(x1,x2))
distance_vector
}
)

pdist(new_vectors, cluster_centers) -> dists
apply(dists, 1, which.min) -> assignment

