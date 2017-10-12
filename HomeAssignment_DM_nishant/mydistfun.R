
mydistfun<- function(element1,element2,metricf){
  # this function returns the distace between the element1 and element2
  # according to the metricf
  
  dimensions=length(element1)
  sqd<-matrix(,dimensions,1)

  if (metricf=="eucledean"){
  #eucledean
  for(i in seq(along=element1)){
    
    sqd[i]<-(element1[i]-element2[i])^2
  }
  dist<-sqrt(colSums(sqd))
  
  }
  if (metricf=="manhattan"){
  #manhattan
  for(i in seq(along=element1)){
    sqd[i]<-abs(element1[i]-element2[i])
  }
  dist<-colSums(sqd)
  }
  
  if(metricf=="chebyshev"){
    for(i in seq(along=element1)){
      sqd[i]<-abs(element1[i]-element2[i])
    }
    dist<-max(sqd)
  }
  #Minkowski 
  if (metricf=="minkowski")
  {
    myInp<-readline("What is the value of p?")
    p<-as.integer(myInp)
    if(p==3){
      for (i in seq_along(element1)){
        sqd[i]<- (abs(element1[i]-element2[i]))^3
      }
      dist<- (colSums(sqd)^(1/3))
    }
    else if(p==4){
      for (i in seq_along(element1)){
        sqd[i]<- (abs(element1[i]-element2[i]))^4
      }
      dist<- (colSums(sqd)^(1/4))
    }
    else(p==5)
      for (i in seq_along(element1)){
        sqd[i]<- (abs(element1[i]-element2[i]))^5
      }
      dist<- (colSums(sqd)^(1/5))
    
  }
  #canberra
  if (metricf=="canberra"){
    for (i in seq_along(element1)){
      
      sqd[i]<- ((abs(element1[i]-element2[i]))/((abs(element1[i]))+(abs(element2[i]))))
    }
    dist<- colSums(sqd)
  }
   return(dist)
}

element1<-matrix(rexp(200, rate=.1), ncol=2)
element2<-matrix(rexp(200, rate=.1), ncol=2)
mydistfun(element1,element2,"eucledean")