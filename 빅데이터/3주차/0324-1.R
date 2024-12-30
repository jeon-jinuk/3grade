#1. FandG.txt를 불러와서 FG0로 저장하시오.
FG0 <- readLines("C:/Users/admin/Desktop/txtm/FandG.txt")

#2. 한글만 & 연속공백 제거 & tibble 구조로 바꾸어 FG1로 저장하시오.
library(stringr); library(dplyr)
FG1 <-FG0 %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
FG1

#3. F1에서  unnest_tokens를 이용해서 명사 추출해서 FG2로 저장하시오.
library(tidytext); library(KoNLP) 
FG2 <- FG1 %>% unnest_tokens(input=value, output=word, token=extractNoun) 
FG2

#4. 빈도수 내림차순 정렬  & 한글자로 된 단어 제거해서 FG3로 저장하시오.
FG3 <- FG2 %>% count(word, sort=T) %>% filter(str_count(word)>1)
FG3
#5. FG3에서 가장 자주 사용된 단어 9개 추출해서 top9로 저장하시오.
top9 <- FG3 %>% head(9)
top9
#6. top9 이용해서 워드 클라우드(word cloud) 만들기  
library(ggwordcloud)
ggplot( top9, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +  
  scale_radius(limits = c(2, NA),  range = c(3, 30))  

#7. FG1을 문장기준으로 토큰화해서 FG11로 저장하시오.
library(tidytext)
FG11 <- FG1 %>% unnest_tokens(input=value, output=sentence, token="sentences")
FG11

#8. 특정단어("우물")가 사용된 문장 추출해서, W로 저장하시오.
w <- FG11 %>% filter(str_detect(sentence, "우물"))
w


