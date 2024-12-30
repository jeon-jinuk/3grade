speech_park.txt 이용
speech_park.txt를 불러와서 
park0<-readLines('C:/Users/admin/Desktop/TXTM/speech_park.txt', encoding='UTF-8')
#1. 연설문(park0)에서 한글이외 문자제거 & 연속된 공백 제거해서 park1으로 저장하시오.
#(library(stringr); str_replace_all, str_squish() 이용)

library(stringr)
park1 <-park0 %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() 
park1

#2. park1을 tibble 구조로 만들어서 park2로 저장하시오.
#(library(dplyr); as_tibble() 이용)
library(dplyr)
park2 <- park1 %>% as_tibble()
park2

#3. park2를 moon2를 띄어쓰기 기준으로 토큰화해서 park21로 저장하시오. 
#(library(tidytext);unnest_tokens 이용)
library(tidytext)
park21 <- park2 %>% unnest_tokens(input=value, output=word2, token="words") 
park21

#4. park21에서 빈도수 내림차순 정렬해서 park22로 저장하시오.
#(count 이용)

park22 <- park21 %>% count(word2, sort=T)
park22

#5. park22에서 한글자로 된 단어 제거해서 park23으로 저장하시오.
#(filter 이용)

park23 <- park22 %>% filter(str_count(word2)>1)
park23


#6. park23에서 가장 자주 사용된 단어 20개 추출해서 top20으로 저장하시오. 
#(head(20) 이용)

top20 <- park23 %>% head(20)
top20

#7. top20를 이용해서 워드 클라우드(word cloud) 만드시오.

library(ggwordcloud)
ggplot( top20, aes(label = word2, size = n)) +
  geom_text_wordcloud(seed = 1234) +  
  scale_radius(limits = c(2, NA),  range = c(3, 30))  

#8. park2에서 명사를 추출하시오. (extractNoun 이용)

library(KoNLP): useNIADic()
extractNoun(park2)

#9. park2를 문장구조기준으로 토큰화해서 park3로 저장하시오.
#(library(tidytext); unnest_tokens 이용)
library(tidytext)
park3 <- park2 %>% unnest_tokens(input=value, output=sentence, token="sentences")
park3

#10. park3에서 “경제”가 사용된 문장을 구해서 econ 으로 저장하시오.
#(filter(str_detect()) 이용)

econ <- park3 %>% filter(str_detect(sentence, "경제"))
econ
