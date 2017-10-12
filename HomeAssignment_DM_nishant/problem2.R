library(readxl)
#data1 <-data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9classification_set_1.csv",sep = ",",header = FALSE))

#data1
data1 <-data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9classification_set_1.csv",sep = ",",header = FALSE))
indexes1 = sample(1:nrow(data1), size=0.3*nrow(data1))
# Split data1
test1 = data1[indexes1,]
dim(test1)  # 30 2
train1= data1[-indexes1,]
dim(train1) # 70 2

#data2
data2 <-data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9classification_set_2.csv",sep = ",",header = FALSE))
indexes2 = sample(1:nrow(data2), size=0.3*nrow(data2))
# Split data2
test2 = data2[indexes2,]
train2= data2[-indexes2,]
#Data3
data3 <-data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9classification_set_3.csv",sep = ",",header = FALSE))
indexes3 = sample(1:nrow(data3), size=0.3*nrow(data3))
# Split data3
test3 = data3[indexes3,]
train3= data3[-indexes3,] # extracting all elements except "indexes3"

#Data4
data4 <-data.frame(read.csv("/home/nishant/3rd_sem/DataMining/Home Assignment/HomeAssignment_DM_nishant/dataset/student_9classification_set_4.csv",sep = ",",header = FALSE))
indexes4 = sample(1:nrow(data4), size=0.3*nrow(data4))

# Split data4
test4 = data4[indexes4,]
train4= data4[-indexes4,]

#joining all the test data and training data
set.seed(3)
test<-rbind(test1,test2,test3,test4)
head(test)
set.seed(3)
train <- rbind(train1,train2,train3,train4)
head(train)
summary(test)
summary(train)
# Euclidean Distance function 

euclideanDist<- function(a,b){
  dist=0
  for(i in c(1:(length(a)-1))){
    
    dist<-dist + (a[i]-b[i])^2
  }
  dist<-sqrt(dist)
  return(dist)
}
#Accuracy Calculation 
accuracy<- function(test){
  correct=0
  for(i in c(1:nrow(test))){
    if(test[i,3]== test[i,4]){
      correct=correct+1
    }
  }
  acc= correct/nrow(test) *100
  return(acc)
}

#Knn- function 
knn<-function(test,train,k_value){
  pred<-c()
  for (i in c(1:nrow(test))){
    euc_dist=c()
    euc_char=c()
    classA=0
    classB=0
    classC=0
    classD=0
    for (j in c(1:nrow(train))){
      #computing Euclidean distance b/w test data and trainning data
      #appending the Euclidean distance to euc_dist vector
      euc_dist<- c(euc_dist,euclideanDist(test[i,],train[j,])) 
      #adding class of training data in euc_char
      euc_char<-c(euc_char,as.character(train[j,][[3]]))
    }
    eu<- data.frame(euc_char,euc_dist)
    eu<- eu[order(eu$euc_dist),]
    eu<- eu[1:k_value,]
    
    #loop over eu and counts teh classes of neighbor 
    
    for (k in c(1:nrow(eu))){
      if(as.character(eu[k,"euc_char"])=="a"){
        classA=classA+1
      }
      else if(as.character(eu[k,"euc_char"])=="b"){
        classB=classB+1
      }
      else if (as.character(eu[k,"euc_char"])=="c"){
        classC=classC+1
      }
      else (as.character(eu[k,"euc_char"])=="d")
      classD=classD+1
      
    }
    
    #maxClass=max(classA,classB,classC,classD)
    #comparing the neighbor with class "good" or "bad"
if((classA>classB) && (classA>classC)&&(classA>classD)){
      pred<-c(pred,"a")
    }
    else if((classB>classA)&&(classB>classC) &&(classC>classD)){
      pred<-c(pred,"b")
    }
    else if((classC>classB)&&(classC>classA)&&(classC>classD)){
      pred<-c(pred,"c")
    }
    else ((classD > classB) &&(classD>classC)&&(classD>classA))
    pred<-c(pred,"d")
  }
  return(pred)
}

  K = 10
predictions <- knn(test,train,K)  #calling knn_predict()


test[,4] <- predictions #Ading predictions in test data as 4th column

print(accuracy(test))

    



