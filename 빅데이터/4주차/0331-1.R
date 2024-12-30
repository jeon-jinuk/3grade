moon0<-readLines('C:/Users/admin/Desktop/TXTM/speech_moon.txt', encoding='UTF-8')
park0<-readLines('C:/Users/admin/Desktop/TXTM/speech_park.txt', encoding='UTF-8')
library(stringr): library(dplyr)
moon1 <- moon0 %>% str_replace_all("[^가-힣]"," ") %>% str_squish() %>% as_tibble()
moon1
moon2 <- moon1%>% mutate(president="moon")
moon2

park1 <- park0 %>% str_replace_all("[^가-힣]"," ") %>% str_squish() %>% as_tibble()
park1
park2 <- park1%>% mutate(president="park")
park2

both0 <- bind_rows(moon2, park2) %>% select(president, value)
both0
tail(both0)

library(tidytext): library(KoNLP)

both1 <- both0 %>% unnest_tokens(input=value, output = word, token = extractNoun)
head(both1)
tail(both1)

both2 <- both1 %>% count(president, word) %>% filter(str_count(word)>1)
head(both2)
tail(both2)

top10 <- both2 %>% group_by(president) %>% slice_max(n, n=10)
top10
tail(top10)

top10 %>% filter(president=="park")

library(ggplot2)
ggplot(top10, aes(x = reorder(word, n), y = n, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president) + labs(x = "단어", y = "빈도수")  

ggplot(top10, aes(x = reorder_within(word, n, president), y = n, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president, scales="free_y") + scale_x_reordered()+
  labs(x = "단어", y = "빈도수") 

top11<-top10 %>% filter(word !="하게")
ggplot(top11, aes(x = reorder_within(word, n, president), y = n, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president, scales="free_y") +scale_x_reordered()+ labs(x = "단어", y = "빈도수") 

library(tidyr)
both2_wide<- both2 %>% pivot_wider(names_from=president, values_from=n, values_fill=list(n=0)) 
both2_wide

v<-which(both2_wide$word=='복지국가'); #행번호
nmA<-both2_wide$moon[v];nmA #몇번 나왔는가
npA<-both2_wide$park[v];npA
nm<- sum(both2_wide$moon);nm
np<- sum(both2_wide$park);np
((nmA+1)/(nm+1))/((npA+1)/(np+1))

both2_wide1<-both2_wide%>%mutate(oddsratio_moon=((moon+1)/(sum(moon)+1))/((park+1)/(sum(park)+1)))
both2_wide1
both2_wide1[269,]

up10<- both2_wide1 %>% filter(rank(-oddsratio_moon)<=10 | rank(oddsratio_moon)<=10) %>% 
  arrange(-oddsratio_moon)
up10

up10F<-up10 %>% mutate(president=ifelse(oddsratio_moon>1, "moon", "park"), n=ifelse (oddsratio_moon>1, moon, park))
up10F
tail(up10F)

ggplot(up10F, aes(x = reorder_within(word, n, president), y = n, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president, scales="free_y") +
  scale_x_reordered()+labs(x = "단어", y = "빈도수") 

which(up10F$word=="대통령의") 
up10F[7,]
up10F[7,1]
up10F[7,1]="대통령"
ggplot(up10F, aes(x = reorder_within(word, n, president), y = n, fill=president))+geom_col()+coord_flip() +
  facet_wrap(~ president, scales="free_y")+scale_x_reordered()+ labs(x = "단어", y = "빈도수")


