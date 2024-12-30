data(iris3)
train <- rbind(iris3[1:25,,1],iris3[1:25,,2],iris3[1:25,,3])
test <- rbind(iris3[26:50,,1],iris3[26:50,,2],iris3[26:50,,3])
c1 <- factor(c(rep("s",25),rep("c",25),rep("v",25)))
library(class)
knn(train,test,c1,k=3,prob=T)

train <- iris[c(1:25,51:75,101:125),1:4]
test <- iris[c(26:50,76:100,126:150),1:4]
test <- iris[-c(26:50,76:100,126:150),1:4]
cl_train <- iris[c(1:25,51:75,101:125),5]
cl_test <- iris[-c(26:50,76:100,126:150),5]
pred <- knn(train,test,cl_train,k=3,prob=T)
prob <- attr(pred,"prob")
d <-data.frame(cl_test,pred,prob)
table(cl_test,pred)

library(DMwR2)
data(iris)
idxs <- sample(1:nrow(iris), as.integer(0.7*nrow(iris))) #1부터있는 자리중에서 하나를 뽑아라
trainIris <- iris[idxs,]
testIris <- iris[-idxs,]

a <- sample(iris$Sepal.Length,11,replace=T) #복원추출 replace=T없는이는 비복원추출이 디폴트값
a

nn3 <- kNN(Species~., trainIris, testIris, stand=FALSE, k=3) #kNN일떄는 stand써야함 knn일떄는 norm
table(testIris[,'Species'],nn3)

library(readxl)
ex <- read_excel("C:/Users/admin/Desktop/데이터파일/지방간자료2.xlsx")
ex$성별 <-as.factor(ex$성별)
ex$지방간유무 <- as.factor(ex$지방간유무)
idxs <- sample(1:nrow(ex), as.integer(0.7*nrow(ex)))
train <- ex[idxs,-6]
test <- ex[-idxs,-6]
cl_train <- ex[idxs,6]
cl_test <- ex[-idxs,6]
pred <- knn(train,test,cl_train,k=3,prob=T)
prob <- attr(pred,"prob")
d <-data.frame(cl_test,pred,prob)
table(cl_test,pred)

nn3 <- kNN(Species~., trainIris, testIris, stand=FALSE, k=3)
table(testIris[,'Species'],nn3)

