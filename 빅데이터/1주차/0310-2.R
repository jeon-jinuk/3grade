#1-1
raw_moon <- readLines('C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt', encoding = 'UTF-8')
head(raw_moon)
tail(raw_moon)
moon0<-head(raw_moon)
write(moon0, 'C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt')
moon<-readLines('C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt')
moon
A0<- "남쪽 언덕 나뭇가지에 앉아. 
한번 울면
천지를 뒤흔듭니다."
A0
write(A0,'C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt' )
A<-readLines('C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt')
A

library(stringr)
txt <- "123 치킨은!! 맛있다. xyz 정말 맛있다!@#"
txt
str_replace_all(string = txt, pattern = "[^가-힣]", replacement = "")
#str_replace_all(txt, "[^가-힣]"," ")
str_replace_all(txt, "[[:punct:]]","")
str_replace_all(txt, "[^0-9가-힣]"," ")

head(raw_moon)
raw_moon1 <- str_replace_all(raw_moon, "[^가-힣]","")
raw_moon1 <- raw_moon %>% str_replace_all("[^가-힣]","")
head(raw_moon1)

str_squish(txt)
txt %>% str_squish()

head(raw_moon)
raw_moon12 <- raw_moon %>% str_replace_all( "[^가-힣]","") %>% str_squish()
head(raw_moon1)

library(dplyr)
moon <- as_tibble(raw_moon12)
head(moon)
head(moon$value)

library(tidytext)
raw_moon12
moon1 <- moon %>% unnest_tokens(input=value, output=word1, token="sentences")
moon1
head(moon1$word1)
moon2<- moon%>% unnest_tokens(input=value,output=word2, token="words")
moon2
head(moon2$word2)
moon3<-moon%>% unnest_tokens(input=value, output=word3, token="characters")
moon3
head(moon3$word3)

moon2
moon3 <- moon2 %>% count(word2, sort=T)
moon3

moon4 <- moon3 %>% filter(str_count(word2)>1)
moon4

top20 <- moon4 %>% head(20)
top20

library(ggplot2)
ggplot(top20, aes(x = reorder(word2, n), y = n))+geom_col()+coord_flip()# 단어빈도순 정렬, 회전 
ggplot(top20, aes(x = reorder(word2, n), y = n))+geom_col()+coord_flip() + 
  labs(x = "단어", y = "빈도수")  

raw_moon <- readLines('C:\\Users\\admin\\Desktop\\rtxt\\speech_moon.txt', encoding = 'UTF-8')
raw_moon
raw_moon12 <- raw_moon %>% str_replace_all( "[^가-힣]"," ") %>% str_squish()

moon <- as_tibble(raw_moon12)
moon
moon2<- moon %>% unnest_tokens(input=value,output=word2, token="words")
moon3 <- moon2 %>% count(word2, sort=T)
moon4 <- moon3 %>% filter(str_count(word2)>1)
top20 <- moon4 %>% head(20)
top20
