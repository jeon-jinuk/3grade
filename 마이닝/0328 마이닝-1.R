a <- iris
plot(iris)
plot(iris$Species,iris$Sepal.Length)
library(rpart)
m1 <- lm(Petal.Length~Petal.Width,data=iris)
m1$coefficients
m2 <- lm(iris$Petal.Length~iris$Petal.Width)
m2$coefficients
c <- rpart(Species~.,data=iris)
c
plot(c,compress = T,margin = 0.3)
text(c,cex=1.5)
library(rpart.plot)
prp(c, type=4, extra=2)
p1 <- predict(m1,newdata=iris)
plot(iris$Petal.Width,p1,type="l")
par(new=T) #앞에 그림을 지우지말고 그려라 
plot(iris$Petal.Width,iris$Sepal.Length)
p2 <- predict(c,newdata=iris, type="class")
p2
str(p2)
install.packages("readxl")
library(readxl)
read_excel()
ex1 <- read_excel("C:/Users/admin/Desktop/데이터파일/지방간자료2.xlsx")
ex1
ex2 <- read.csv("C:/Users/admin/Desktop/데이터파일/지방간자료2.csv")
ex2
ex1$성별 <- as.factor(ex1$성별)
ex1$지방간유무 <- as.factor(ex1$지방간유무)

c2 <- rpart(지방간유무~., data=ex1)
predict(c2,newdata=ex1,type="class")
prp(c2,type=4,extra=2)

c3 <- rpart(ex1$체질량지수~ex1$간기능_GOT+ex1$당뇨)
predict(c3,newdata=ex1)
prp(c3,type=4,extra=0)


