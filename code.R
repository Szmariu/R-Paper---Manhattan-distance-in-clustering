######LIBRARIES######
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)
library(flexclust)
library(fpc)
library(clustertend)
library(ClusterR)
library(NbClust)
library(microbenchmark)
library(multcomp)

options(scipen=999) #avoiding e10 notation

######DATA IMPORT######
# Importing main data table, NaN are ommited (about 10% of all data)
mainTable <- na.omit(read.csv("data/clean_googleplaystore.csv",header=TRUE))

# Import the ratings, NaN are ommited (about 30% of all data)
ratingsTable <- na.omit(read.csv("data/clean_googleplaystore_user_reviews.csv",header=TRUE))

# Some basic summary
str(mainTable)
summary(mainTable)

######DATA PROCESSING######
# Subsetting the average reviews and number of reviews
clusteringData <- mainTable[,c(2,3)]

# Finding the optimal number of clusters
c3<-NbClust(clusteringData , distance="euclidean", min.nc=2, max.nc=8, method="complete", index="ch")
c3$Best.nc

    ##### Comparing the speed in distance matrixes ######
# Sample distance matrix

# Speed test
timeDistanceMatrix <- microbenchmark(
  dist(clusteringData, method = "euclidean"),
  dist(clusteringData, method = "manhattan"),
  dist(clusteringData, method = "canberra"),
  dist(clusteringData, method = "minkowski"),
  times=200)
 
#Results
timeDistanceMatrix$expr <- revalue(timeDistanceMatrix$expr, c('dist(clusteringData, method = "euclidean")' = "Euclidian",
                                          'dist(clusteringData, method = "manhattan")' = "Manhattan",
                                          'dist(clusteringData, method = "canberra")' = 'Canberra',
                                          'dist(clusteringData, method = "minkowski")' = 'Minkowski'))
timeDistanceMatrixResults <- print(timeDistanceMatrix, unit = "s")
ggplot2::autoplot(timeDistanceMatrix)

# Investigating the outliers - evenly distributed ?
timeDistanceMatrix %>% 
  filter(timeDistanceMatrix$expr == 'dist(clusteringData, method = "euclidean")') %>% 
  droplevels.data.frame() %>%
  ggplot(aes(y=time, x=c(1:200))) + geom_point()

    ###### Comparing the speed in CClust ######
# Speed test
timeCClust <- microbenchmark(
  cclust(clusteringData, k = 6, dist = "euclidean", simple = FALSE, save.data=TRUE),
  cclust(clusteringData, k = 6, dist = "manhattan", simple = FALSE, save.data=TRUE),
  times=10)

# Results
timeCClustResults <- print(timeCClust, unit = "s")
ggplot2::autoplot(timeCClust)


    #### Comparing the speed in Clara ######
# Sample clustering
sampleClara<-clara(clusteringData, 6, metric="euclidean", stand=FALSE, samples=50,
                   sampsize=200, trace=0, medoids.x=TRUE,
                   rngR=TRUE, pamLike=FALSE, correct.d=TRUE) #cluster::
fviz_cluster(sampleClara, ellipse.type = "t", geom = "point", pointsize = 1 )
fviz_silhouette(sampleClara)

# Speed test
timeClara <- microbenchmark(
  clara(clusteringData, 6, metric="euclidean", stand=FALSE, samples=50,
        sampsize=200, trace=0, medoids.x=TRUE,
        rngR=TRUE, pamLike=FALSE, correct.d=TRUE),
  clara(clusteringData, 6, metric="manhattan", stand=FALSE, samples=50,
        sampsize=200, trace=0, medoids.x=TRUE,
        rngR=TRUE, pamLike=FALSE, correct.d=TRUE),
  times=100)

# Results
timeClaraResults <- print(timeClara, unit = "s", order = 'mean', signif = 3)
ggplot2::autoplot(timeClara)

# T test
x <- timeClara %>% filter(timeClara$expr == 'clara(clusteringData, 6, metric = "euclidean", stand = FALSE,      samples = 50, sampsize = 200, trace = 0, medoids.x = TRUE,      rngR = TRUE, pamLike = FALSE, correct.d = TRUE)')
y <- timeClara %>% filter(timeClara$expr != 'clara(clusteringData, 6, metric = "euclidean", stand = FALSE,      samples = 50, sampsize = 200, trace = 0, medoids.x = TRUE,      rngR = TRUE, pamLike = FALSE, correct.d = TRUE)')
t.test(x$time,y$time)


