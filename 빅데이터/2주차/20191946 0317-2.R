FG0<-readLines('C:/Users/admin/Desktop/TXTM/FandG.txt', encoding='UTF-8')
FG0 
FG<-c(FG0, FG0[27])
tail(FG)
A<-"이솝 우화입니다."
A
FG<-c(FG, A)
tail(FG)
write(FG, 'C:/Users/admin/Desktop/TXTM/FandG.txt')

library(stringr)
head(FG)
FG1 <- FG %>% str_replace_all( "[^가-힣]", " ") %>% str_squish()
head(FG1)

library(dplyr)
FG2 <- as_tibble(FG1)
FG2
FG2$value

library(tidytext)
FG2
FG21 <- FG2 %>% unnest_tokens(input=value, output=word1, token="sentences")
FG21

FG22<-FG2%>% unnest_tokens(input=value, output=word2, token="words")
FG22

FG23 <- FG2 %>% unnest_tokens(input=value, output=word3, token="characters")
FG23

FG22
FG32 <- FG22 %>% count(word2, sort=T)
FG32

FG34 <- FG32 %>% filter(str_count(word2)>1)
FG34

top20 <- FG34 %>% head(20)
top20

library(ggplot2)
ggplot(top20, aes(x = reorder(word2, n), y = n))+geom_col()+coord_flip()+ 
  geom_text(aes(label = n), hjust = -0.3) + # 막대 밖 빈도 표시
  labs(title = "Fox and Goat 단어 빈도", x = NULL, y = NULL) + #그래프 제목, 축 이름 삭제
  theme(title = element_text(size = 12))         # 제목 크기

library(KoNLP): useNIADic()
a <-" 많은 전문가들은 COVID-19가 Endemic에 들어섰으며, 방역 조치를 오나화해야 한다고 한다."
SimplePos09(a)
extractNoun(a)

library(dplyr)
A <- tibble( value=c("형태소는 의미를 지닌 가장 작은 말의 단위이다." , "형태소 사전을 설정한다."))
A
extractNoun(A$value)

library(tidytext)
A %>% unnest_tokens(input=value, output=word, token=extractNoun)

moon0<-readLines('C:/Users/admin/Desktop/TXTM/speech_moon.txt', encoding='UTF-8')
library(stringr)
moon1 <-moon0 %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
moon1
moon2<-moon1 %>% unnest_tokens(input=value, output=word, token=extractNoun)
moon2

moon3 <-moon2 %>% count(word, sort=T) %>% filter(str_count(word)>1)
moon3

top30 <- moon3 %>% head(30)
top30

library(ggwordcloud)
ggplot(top30 , aes(label = word, size = n, col=n)) + 
  geom_text_wordcloud(seed = 1) +     
  scale_radius(limits = c(3, NA), range = c(3, 30))+      
  scale_color_gradient(low = "red", high ="blue")+ 
  theme_minimal()                          

moon1
library(tidytext)
moon11 <- moon1 %>% unnest_tokens(input=value, output=sentence, token="sentences")
moon11

B <- "치킨은!! 맛있다. 정말 맛있다"
str_detect(B, "치킨")
str_detect(B, "달걀")

people = moon11 %>% filter(str_detect(sentence, "국민"))
people

people$sentence
