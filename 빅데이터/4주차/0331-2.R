# 예제(4후보) 불러와서 전처리
#install.packages("tidyverse")
library(tidyverse) # csv파일 불러오기, 순서=노-문-박-이 
four<-read_csv("C:/Users/admin/Desktop/TXTM/speeches_presidents.csv")
four

#library(stringr); library(dplyr) # 한글외 제거, tibble 구조로
four <- four %>% mutate(value=str_replace_all(value, "[^가-힣]"," "), value=str_squish(value))
library(tidytext): library(KoNLP)
four <- four %>% unnest_tokens(input = value, output = word, token = extractNoun)

#1. 단어빈도 구하기
library(stringr)
fourTF <- four %>% count(president, word) %>% filter(str_count(word)>1)
fourTF
tail(fourTF)

#2. TF-IDF 구하기
library(tidytext)
fourTF_IDF <-fourTF %>% bind_tf_idf(term=word, document=president, n=n)
fourTF_IDF

# TF-IDF 내림차순 정렬
fourTF_IDF <- fourTF_IDF %>% arrange(-tf_idf)

# 후보별 TF-IDF 높은 단어 보기
fourTF_IDF %>% filter(president=='노무현')
fourTF_IDF %>% filter(president=='문재인')
fourTF_IDF %>% filter(president=='이명박')
fourTF_IDF %>% filter(president=='박근혜')
#3. 막대 그래프
# 주요단어 5개 추출
top5 <- fourTF_IDF %>% group_by(president) %>% slice_max(tf_idf, n=5)
top5

# 그래프 순서
top5$president <- factor(top10$president, levels = c('문재인', '박근혜','이명박','노무현'))


# 막대 그래프
library(ggplot2)
ggplot(top5, aes(x = reorder_within(word, tf_idf, president), 
                 y = tf_idf, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president, scales="free",ncol=2) + 
  scale_x_reordered()+
  labs(x=NULL)
