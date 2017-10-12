

require(ggplot2)
clusData <- data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9_clustering_set.csv",sep = ",",header = FALSE))
set.seed(100)
xval<-as.data.frame(clusData$V1,drop=FALSE)
yval<-as.data.frame(clusData$V2,drop=FALSE)

kclus<- function(x,y,nclus,random.seed=123){
  set.seed(random.seed)
  #start with random cluster centers
  xcen <-runif(n=nclus,min = min(x),max = max(x))
  ycen<- runif(n=nclus,min = min(y),max = max(y))
  
  #creating the datapoint dataframe with fields
  data<-data.frame(xval=x,yval=y,clus=NA)
  clus<-data.frame(name=1:nclus,xcen=xcen,ycen=ycen)
  
  #initialising the finish=false ie. iteration of Kmeans 
  finish<-FALSE  
  while(finish==FALSE){
    #assigning cluster with minimum dist to each data point
    for(i in 1:nrow(xval)){
      dist<- sqrt((x[i,]-clus$xcen)^2 +(y[i,]-clus$ycen)^2)
      data$clus[i]<- which.min(dist)
    }
    xcen_old<-clus$xcen
    ycen_old <-clus$ycen
    
    #New Cluster centers
    for(i in 1:nclus){
    
      clus[i,2]<- mean(subset(data$xval, data$clus==i))#writing the new mean to clus"xcen"
      clus[i,3]<- mean(subset(data$yval,data$clus==i))
    }
    #stop the loop if there is no change in cluster coordination 
    if(identical(xcen_old,clus$xcen) & identical(ycen_old,clus$ycen)){
      finish<- TRUE
    }
    return(data)
  }
}
cluster <-kclus(xval,yval,2)
cluster.centers <- aggregate(.~clus, cluster, mean)
ggplot(cluster, aes(xval, yval, color = as.factor(clus))) + 
geom_point(size=1) 



  #+geom_point(data=cluster.centers, aes(xval, yval, col=as.factor(clus)), pch=8, size=5)



