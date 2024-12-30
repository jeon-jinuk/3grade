library(readxl)
library(dplyr)
body <- read_excel("C:/Users/HYUNCOM/Desktop/신체자료_수업용.xlsx")
body <- as.data.frame(a)

str(body) 

body$성별[body$성별=="남"] <- 1 
body$성별[body$성별=="여"] <- 0 
body$성별 <- as.numeric(body$성별) 
body$키 <- body$키/10



res <- 0
if(body$ =< 101 | body$성별 == 1) res <- res + 1


body <- subset (body,select=c(성별,키))
body <- body[order(-body$키),]
body 
head(body)
plot(body[,성별], body[,성별],)
                                                                                     
SplitRow <-rep(1:51, c(rep(100, 50), 92)) 
SplitB <- split(body, SplitRow)
view(SplitB) 
str(SplitB)

SA<- as.data.frame(SplitB[1:50])
SAL <- as.data.frame(SplitB[51])
library(dplyr)
view(SA)

# df1 - df52까지 52개의 데이터 프레임으로 나누기
for (i in seq_along(SplitB)) {
  assign(paste0("df", i), value = SplitB[[i]])
}

# 52개의 데이터 리스트별 성별 평균
for (i in seq_along(SplitB)){
  print(mean(SplitB[[i]]$성별))
}

# 52개의 데이터 리스트별 키 평균
for (i in seq_along(SplitB)){
  print(mean(SplitB[[i]]$키))
}

x<-c()
for (i in seq_along(SplitB)){
  x[[i]] <- mean(SplitB[[i]]$키)
}
x

y<-c()
for (i in seq_along(SplitB)){
  y[[i]] <- mean(SplitB[[i]]$성별)
}
y

plot(x,y)
plot(x,y,type = "l")







