#1장. 단어 빈도분석 18-44 (무엇을 강조?)
#박근혜 전대통령 연설문을 이용해서 막대그래프, 워드 클라우드(흑백), 워드 클라우드(색깔) 만들어 
#한글파일로 만들어 (파일이름= txtm1_본인이름.hwp)  rlagmltn980825@naver.com로 보내시오.

jean_park <- readLines('C:\\Users\\admin\\Desktop\\rtxt\\speech_park.txt', encoding = 'UTF-8')
jean_park12 <- jean_park %>% str_replace_all( "[^가-힣]"," ") %>% str_squish()
jean<- as_tibble(jean_park12)
jean2<- jean%>% unnest_tokens(input=value,output=word2, token="words")
jean3 <- jean2 %>% count(word2, sort=T)
jean4 <- jean3 %>% filter(str_count(word2)>1)
top20 <- jean4 %>% head(20)

library(ggplot2)
ggplot(top20, aes(x = reorder(word2, n), y = n))+geom_col()+coord_flip() + 
  labs(x = "단어", y = "빈도수")  

library(ggwordcloud)
ggplot(jean4, aes(label = word2, size = n)) +
  geom_text_wordcloud(seed = 12134) + 
  scale_radius(limits = c(3, NA),  range = c(3, 30))  

ggplot(jean4, aes(label = word2, size = n, col=n)) + 
  geom_text_wordcloud(seed = 1234) +     
  scale_radius(limits = c(3, NA), range = c(3, 30))+      
  scale_color_gradient(low = "blue", high ="red")+
  theme_minimal()                           
