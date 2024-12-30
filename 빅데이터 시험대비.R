install.packages("rjava")
install.packages("memoise")
install.packages("multilinguer")
install.packages(c('stringr','hash','tau','sejong','RSQLite','devtools'),type = "binary")

install.packages("KoNLP")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP',upgrade = "never",INSTALL_opts=c("--no-multiarch"))
library(KoNLP)



#1장. 단어 빈도분석 
#1-1. 
library(stringr); 
A<- "여우@FOX는 아주 쾌활한 듯 345      말했어요!!!"
#다음의 결과를 쓰시오.
#(1) (연속공백 없애기) 
A %>% str_squish() 
#(2) (특수문자 없애기 & 연속공백 없애기) 
A %>% str_replace_all( "[[:punct:]]", " ")%>% str_squish() 
#(3) (한글만 남기기 & 연속공백 없애기) 
A %>% str_replace_all( "[^가-힣]", " ") %>% str_squish()
#(4) (숫자와 한글남기기 & 연속공백 없애기) 
A %>% str_replace_all( "[^0-9가-힣]", " ") %>% str_squish()
#(5) (영어대문자와 한글남기기 & 연속공백 없애기)
A %>% str_replace_all( "[^A-Z가-힣]", " ") %>% str_squish()

#1-2.
library(dplyr); library(tidytext);
B0<- "맛있다 치킨은 맛있다 정말 맛있다 정말 내맘에 든다"
#다음의 결과를 쓰시오.
#(1) (tibble 구조) 
B <- as_tibble(B0); B$value
#(2) (토큰화 기본단위=문자) 
B1<- B %>% unnest_tokens(input=value, output=word1, token="characters");
head(B1); tail(B1)
#(3) (토큰화 기본단위=띄어쓰기(단어)) 
B2<- B  %>% unnest_tokens(input=value, output=word2, 
                                  token="words");  B2
#(4) (토큰화 기본단위=문장)
B3<- B %>% unnest_tokens(input=value, output=word3, token="sentences");
B3
#(5) (단어 빈도구하기) 
B4<- B2 %>% count(word2, sort=T); B4
#(6) (자주 사용된 4 글자만 선택) 
B5<- B4 %>% head(4); B5
#(7) (막대그래프 그리기) 
library(ggplot2)
ggplot(B5, aes(x = reorder(word2, n), y = n))+geom_col()+coord_flip() + 
  labs(x = "단어", y = "빈도수") 
#(8) (워드클라우드 그리기) 
library(ggwordcloud)
ggplot(B5, aes(label = word2, size = n)) +
  geom_text_wordcloud(seed = 0) +   
  scale_radius(limits = c(1, NA),  range = c(3, 30)) 

ggplot(B5, aes(label = word2, size = n)) +
  geom_text_wordcloud(seed = 0) +   
  scale_radius(limits = c(2, NA),  range = c(3, 30)) 


#2장. 형태소 분석을 이용한 단어 빈도분석 (형태소 : 의미를 가진 가장 작은 말의 단위)
#2-1.
library(stringr); library(dplyr); library(tidytext); library(KoNLP); 
C<-tibble( value=c("형태소는 의미를 지닌 가장 작은 말의 단위이다." , "형태소 사전을 설정한다."))
#다음의 결과를 쓰시오.
#(1) (tibble 구조)
C
#(2) (명사 추출) 
extractNoun(C$value)
#(3) (unnest_tokens 이용해서 명사추출) 
C1<- C%>% unnest_tokens(input=value, output=word, token=extractNoun); C1
#(4) (명사의 빈도수 구하기)
C2<- C1 %>% count(word); C2 
#(5) (명사의 빈도수 구하고 내림차순 정렬)
C3<- C1 %>% count(word, sort=T); C3 
#(6) (막대그래프 그리기) library(ggplot2)
ggplot(C3, aes(x = reorder(word, n), y = n))+geom_col()+coord_flip() + 
  labs(x = "단어", y = "빈도수") 

#3장. 비교분석 (여러 텍스트 비교)
#3-1. 
library(stringr); library(dplyr);library(tidytext); library(KoNLP)
D<-tibble( value=c("정권 교체하겠습니다", "정치 교체하겠습니다","시대 교체하겠습니다"))
E<-tibble( value=c("존경하는 국민 여러분",  "행복한 대한민국을 위하여","국민 여러분 선언합니다" ))
#다음의 결과를 쓰시오.
#(1) (candidate 라는 변수를 만들고 변수값을 "m" 으로 입력)
D1<-D%>% mutate(candidate="m"); D1
#(2) (candidate 라는 변수를 만들고 변수값을 "p" 로 입력)
E1<-E%>% mutate(candidate="p"); E1
#(3) (데이터 아래위로 합치기, 변수순서는 candidate-value)
DE<- bind_rows(D1,E1) %>% select(candidate, value); DE 
#(4) (명사추출) 
DE1 <- DE %>% unnest_tokens(input=value, output=word, token=extractNoun);
head(DE1); tail(DE1)
#(5) (단어 빈도수를 후보별로 정렬, 두글자이상 단어만 남기기)
DE2<-DE1%>% count(candidate, word) %>% filter(str_count(word)>1); DE2 
#(6) (후보별로 단어를 내림차순으로 정렬 & 상위 4개 추출)
DE3<- DE2 %>% group_by(candidate) %>% slice_max(n, n=4); DE3
#(7) (막대그래프 그리기)
library(ggplot2)
ggplot(DE3, aes(x = reorder(word, n), y = n, fill=candidate))+geom_col()+coord_flip() +
  facet_wrap(~ candidate, scales="free_y") + labs(x = "단어", y = "빈도수") 


#3-2. 다음의 결과를 쓰시오.
#(1) (데이터 옆으로 합치기, NA(상대방은 있으나 본인 없는 경우)를 0으로 표시)
library(tidyr); DE_wide<- DE3 %>% pivot_wider(names_from=candidate, values_from=n, 
                                              values_fill=list(n=0)); DE_wide
#(2) (로그오즈비(m 기준) 구하기 ~ DE_wide에 logO변수 생성
DE_wide1<-DE_wide %>% mutate(logO=log(((m+1)/(sum(m)+1))/((p+1)/(sum(p)+1)) ) )
DE_wide1
#(3) (새변수 hilight 만들기~ logO>0 이면 moon 아니면 park 으로 표시) 
DE_wide2<-DE_wide1 %>% mutate(hilight=ifelse(logO>0, "moon", "park")); DE_wide2
#(4) (막대그래프 그리기) library(ggplot2)
ggplot(DE_wide2, aes(x = reorder(word, Ozm),y =Ozm , fill=hilight))+geom_col()+coord_flip() +labs(x =NULL)

#4장. 감성분석 (텍스트에 어떤 감정이 담겨있는지 분석)
#4-1.
library(dplyr); library(tidytext); library(readr) 
F<-tibble(sentence=c('디자인 예쁘다','마감도 좋아서 만족스럽다','디자인은 괜찮다','그런데 마감이 나쁘다','가격도 비싸다'));
dic<- read_csv("C:/Users/HYUNCOM/Desktop/knu_sentiment_lexicon.csv")
#D:\Work\빅데이터_2023_1st\week7
dic<- read_csv("D:/Work/빅데이터_2023_1st/week7/knu_sentiment_lexicon.csv")
#다음의 결과를 쓰시오
#(1) (토큰화,띄었쓰기 기준)
F1 <- F %>% unnest_tokens(input=sentence, output=word, token="words", drop=FALSE); F1
#(2) (단어 감정점수(-2 ~ +2)를 나타내는 polarity변수 생성)(사전(dic)에 없는 단어의 감정점수=0) 
F2<- F1 %>% left_join(dic, by="word") %>% mutate(polarity=ifelse(is.na(polarity),0, polarity)); F2
#(3) (문장별로 감정점수합을 구해서 scorey변수 생성)
score_F2<- F2 %>% group_by(sentence) %>% summarize(score=sum(polarity)); score_F2 
#(4) ( 감정점수가 2이상이면 positive, -2이하 이면 negtive, 아니면 neutral로 표시되는 sentiment변수 생성)  
F3<- score_F2%>% mutate(sentiment=ifelse(score>=2,'positive', ifelse(score<=-2,'negative','neutral')));
F3
#(5) (감정단어(pos, neg neu)의 빈도수를 구하고  빈도율(%)을 나타낸는 prop변수 생성)
F4<- F3 %>% count(sentiment) %>% mutate(prop=n/sum(n)*100); F4 

#(6) (막대그래프 그리기) library(ggplot2)
ggplot(F4, aes(x = reorder(sentiment, n),  y = n, fill=sentiment))+geom_col()+ labs(x = "감정", y = "빈도수") +geom_text(aes(label=n), hjust=-0.3)

ggplot(F4, aes(x = reorder(sentiment, prop),  y =prop , fill=sentiment))+geom_col()+ labs(x = "감정", y = "빈도률(%)") +geom_text(aes(label=round(prop)), hjust=-0.3)


#중간시험 예제)
# 결과를 쓰시오.
#(1)
library(stringr); A<-"2023  MZ 시대 치킨!!"
A %>% str_replace_all( "[^가-힣]", " ") %>% str_squish() 
#(2)
library(dplyr); library(tidytext); B <- "치킨 교체 교체"
B <- as_tibble(B);
B
#2. 
C<-tibble( value=c("형태소는 의미를 지닌 가장 작은 말의 단위이다." , "형태소 사전을 설정한다."))
C2<- C1 %>% count(word)
#의 결과를 보기에서 고르시오.

