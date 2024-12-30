#1. UKs.txt(실연극복), UKp.txt(선거후스트레스장애극복)를 불러와서 SK0, PK0로 저장하기.
SK0<-readLines('C:/Users/admin/Desktop/TXTM/UKs.txt', encoding='UTF-8')
PK0<-readLines('C:/Users/admin/Desktop/TXTM/UKp.txt', encoding='UTF-8')

#2. 한글외 제거, tibble 구조로 만들어 SK1, PK1로 저장하기.
library(stringr); library(dplyr)
sk1 <-SK0 %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
sk1

pk1 <-PK0 %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() %>% as_tibble()
pk1

#3. issue라는 변수를 만들고 값을 "love", "elect"으로. SK2, PK2로 저장하기.
sk2 <- sk1 %>% mutate(issue="love")
pk2 <- pk1 %>% mutate(issue="elect")

#4. 데이터 합치기: SK2, PK2 합치기. Both0로 저장하기.
both0 <- bind_rows(sk2, pk2) %>% select(issue, value)
both0
tail(both0)


#5. Both0에서 명사 추출해서 Both1으로 저장하기.
library(tidytext); library(KoNLP)
both1 <- both0 %>% unnest_tokens(input=value, output = word, token = extractNoun)
head(both1)
tail(both1)

#6. 단어 빈도수를 issue별로 정렬하고 두글자이상 단어만 남기기. Both2로 저장하기.
both2 <- both1 %>% count(issue, word) %>% filter(str_count(word)>1)
head(both2)
tail(both2)


#7. issue별로 단어를 내림차순으로 정렬 & 상위 7개 추출. top7로 저장하기.
top7 <- both2 %>% group_by(issue) %>% slice_max(n, n=7)
top7
tail(top7)


#8. 막대그래프(top7 이용). 변수별로
library(ggplot2)
ggplot(top7, aes(x = reorder(word, n), y = n, fill=issue))+geom_col()+coord_flip() +
  # 후보자별 legend, 단어빈도순 정렬, 회전
  facet_wrap(~ issue) + labs(x = "단어", y = "빈도수")

#9. 막대그래프(top7 이용). 그래프별로 y축 설정
ggplot(top7, aes(x = reorder(word, n), y = n, fill=issue))+geom_col()+coord_flip() +
  # issue별 legend, 단어빈도순 정렬, 회전
  facet_wrap(~ issue, scales="free_y") + labs(x = "단어", y = "빈도수")

#10. long-form을 wide-form으로. Both2를 Both2_wide로 저장하기.
library(tidyr)
both2_wide<- both2 %>% pivot_wider(names_from=issue, values_from=n, values_fill=list(n=0)) 
both2_wide


#11. both2_wide 변수 'oddsratio_elect' 만들어 오즈비(moon) 입력. Both2_wide1로 저장하기.
both2_wide1<-both2_wide%>%mutate(oddsratio_elect=((elect+1)/(sum(elect)+1))/((love+1)/(sum(love)+1)))
both2_wide1

#12. SK, PK 의 상대적 중요도 큰 단어 7개씩~ PK에서 상대적 중요도 큰 단어 7개, up7로 저장하기.
#SK의 상대적 중요도 작은 단어 7개. PK의 상대적 중요도 큰 단어 내림차순. up7로 저장하기.
up7<- both2_wide1 %>% filter(rank(-oddsratio_elect)<=7 | rank(oddsratio_elect)<=7) %>% 
  arrange(-oddsratio_elect)
up7


#13. up7에서 oddsratio_elect >1 이면 elect, 아니면 love로 표시되는 변수 issue 만들기. up7F로 저장하기.
up7F<-up7 %>% mutate(issue=ifelse(oddsratio_elect>1, "elect", "love"), n=ifelse (oddsratio_elect>1, elect, love))
up7F
tail(up7F)


#14. 막대그래프(오즈비 큰 순으로) (up7F이용 )
ggplot(up7F, aes(x = reorder_within(word, n, issue), y = n, fill=issue))+geom_col()+coord_flip() +
  facet_wrap(~ issue, scales="free_y") + labs(x = "단어", y = "빈도수") +
  scale_x_reordered()
#15. up7F에서 특정단어('있습니') 제거. up7FF로 저장하기.
up7FF<-up7F %>% filter(word !="있습니")
up7FF

#16. 막대그래프(오즈비 큰 순으로)(up7FF이용)
ggplot(up7FF, aes(x = reorder_within(word, n, issue), y = n, fill=issue))+geom_col()+coord_flip() +
  facet_wrap(~ issue, scales="free_y") + labs(x = "단어", y = "빈도수") +
  scale_x_reordered()
#17. Both1에서 단어 빈도 구하고 두글자이상 단어만 남기기. BothTF로 저장하기.
library(stringr)
BothTF <-both1  %>% count(issue, word) %>% filter(str_count(word)>1)
BothTF
tail(BothTF)

#18. TF-IDF 구하기. BothTF_IDF 로 저장하기.
library(tidytext)
BothTF_IDF <-BothTF %>% bind_tf_idf(term=word, document=issue, n=n)
BothTF_IDF

#19. TF-IDF 내림차순 정렬해서 BothTF1로 저장하기.
BothTF1 <- BothTF_IDF %>% arrange(-tf_idf)

#20. 주요단어 4개 추출해서 top4로 저장하기.
top4 <- BothTF1 %>% group_by(issue) %>% slice_max(tf_idf, n=4)
top4


#21. 막대 그래프(top4이용)
library(ggplot2)
ggplot(top4, aes(x = reorder_within(word, tf_idf, issue),
                 y = tf_idf, fill=issue))+geom_col()+coord_flip() +
  facet_wrap(~ issue, scales="free",ncol=2) +
  scale_x_reordered()+labs(x=NULL)
