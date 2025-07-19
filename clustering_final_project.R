# clustering
#install.packages('factoextra')
library(factoextra)
library(dplyr)
rm(list=ls())
set.seed(2)


car.df.cat.norm=read.csv('car_cleaned_data_normalized.csv')
car.df.cat=read.csv('cleaned_car_data.csv')
train_index=sample(1:nrow(car.df.cat.norm),size=0.6*nrow(car.df.cat.norm))
car.df.train.norm=car.df.cat.norm[train_index,]
car.df.valid.norm=car.df.cat.norm[-train_index,]
car.df.cat.train=car.df.cat[train_index,]
car.df.cat.valid=car.df.cat[-train_index]

#View(car.df.cat.norm)
#car.df.cat.train=car.df.cat.norm
# kmeans 
set.seed(2)
km=kmeans(car.df.train.norm,4)
km$cluster
#car.df.train.norm$cluster=km$cluster
#View(car.df.train.norm)

# determine and visualize optimal number of clusters

fviz_nbclust(car.df.train.norm, kmeans, method = "wss") 
#fviz_nbclust(car.df.train.norm, kmeans, method = "silhouette")
#fviz_nbclust(car.df.train.norm, kmeans, method = "gap_stat") 

# create cluster biplot
#fviz_cluster(kmeans(car.df.train.norm, centers = 5), data = car.df.train.norm)
fviz_cluster(list(data=car.df.train.norm,cluster=km$cluster))
  # visualize clusters using original variables
clusters <- kmeans(car.df.train.norm, centers = 4)
car.df.train.norm <- car.df.train.norm |> mutate(cluster = clusters$cluster)
car.df.cat.train <- car.df.cat.train |> mutate(cluster = clusters$cluster)
View(car.df.cat.train)
car.df.cat.train |> ggplot(aes(x = name, y = selling_price, col = as.factor(cluster))) + geom_point()


#plot and evaluation 
#row.names(car.df.cat.train) <- car.df.cat.train[,1]
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))
axis(1, at = c(1:23), labels = names(car.df.cat.train))
#View(car.df.cat.train)
# plot centroids
for (i in c(1:4))
  lines(km$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5, 7, 9),
                                                       "black", "dark grey"))
text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:4)))

km$size
centroid=aggregate(.~cluster,data=car.df.cat.train,FUN = mean)
centroid=aggregate(car.df.cat.train,by=list(km$cluster),FUN=mean)
centroid[,c(4,5,6,7,8)]
centroid = as.data.frame(t(centroid))
options(scipen = 999)










#model 2 
#car.df.train.norm=car.df.cat.norm
dim(car.df.train.norm)
car.df.train.norm=car.df.train.norm[,-23]
#euclidean distance normalized
d.norm=dist(car.df.train.norm,method = 'euclidean')
View(car.df.train.norm)


#a]
#Hierarchical clustering with 'Ward.D' linkage with normalized 
hc1=hclust(d.norm,method = 'ward.D')
plot(hc1,hang = -1,ann = FALSE)
memb_normalized=cutree(hc1,k=4)
memb_normalized
rect.hclust(hc1,k=4,border='red')


#c]
car.df.train.norm$cluster=memb_normalized
#car.df.cat.train$cluster=memb_normalized
#View(car.df.cat.train)
#View(airlines.df.norm)

#centroid=aggregate(.~cluster,data=car.df.cat.train,FUN = mean)
centroid=aggregate(car.df.cat.train,by=list(car.df.cat.train$cluster),FUN=mean)
centroid=centroid[,c(4,5,6,7,8)]
print(centroid)








