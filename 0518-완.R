library(tidyverse) 
P<-read_csv("C:/Users/admin/Desktop/TXTM/news_comment_parasite.csv")
P

library(stringr)
p0 <- P %>% select(reply) %>% mutate(reply=str_replace_all(reply, "[^가-힣]"," "),
                                     reply=str_squish(reply), id=row_number())                                     
p0

library(tidytext): library(KoNLP)
p1 <- p0 %>% unnest_tokens(input = reply, output=word, token=SimplePos22, drop=F)
p1

p1 %>% select(word,reply)

library(tidyr)
p2 <- p1 %>% separate_rows(word, sep="[+]")
p2

p2 %>% select(word,reply)

noun <- p2 %>% filter(str_detect(word, "/n")) %>%  mutate(word=str_remove(word,"/.*$"))
noun

pvpa <- p2 %>% filter(str_detect(word, "/pv|pa")) %>% mutate(word=str_replace(word, "/.*$","다"))
pvpa

p3 <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
p3

install.packages("widyr")
library(widyr)
p4 <- p3 %>% pairwise_count(item = word, feature = id, sort=T)
p4

p4 %>% filter(item1=="영화")

install.packages("tidygraph"): library(tidygraph)
p5 <- p4 %>% filter(n>=25) %>% as_tbl_graph()
p5

install.packages("ggraph"); library(ggraph)
ggraph(p5)+ geom_edge_link() +geom_node_point() + geom_node_text(aes(label=name))  

install.packages("showtext"); library(showtext)
font_add_google(name="Nanum Gothic", family="nanumgothic")
showtext_auto()

set.seed(12) 
ggraph(p5, layout="fr") + geom_edge_link(color="gray50", alpha=0.5)  + geom_node_point(color="lightcoral", size=5)  + geom_node_text(aes(label=name), repel=T,   size=5, family="nanumgothic")+ theme_graph() 

v=which(str_detect(p3$word, "감독")==TRUE)
p3$word[v]

p31 <- p3 %>% mutate(word=ifelse(str_detect(word, "감독"), "감독", word),
                     word=ifelse(str_detect(word, "축하"),"축화", word))
v=which(str_detect(p31$word, "감독")==TRUE)
p31$word[v]

p41 <- p31 %>% pairwise_count(item=word, feature=id, sort=T)
p51 <- p41 %>% filter(n>=25) %>% as_tb1_graph()

dev.new()
set.seed(12)
ggraph(P51, layout="fr") + geom_edge_link(color="gray50", alpha=0.5)  + geom_node_point(color="lightcoral", size=5)  + geom_node_text(aes(label=name), repel=T,   size=5, family="nanumgothic")+ theme_graph() 

set.seed(12)
p52 <- p41 %>% filter(n>=25) %>% as_tb1_graph(direted=F) %>% 
mutate(centrality=centrality_degree(), group=as.factor(group_infomap()))
p52

p52 %>% arrange(-centrality)

p52 %>% filter(name=="감독")

p52 %>% filter(group==1) %>% arrange(-centrality)

dev.new() 
set.seed(12) 
ggraph(P52, layout="fr") + geom_edge_link(color="gray50", alpha=0.5)  + geom_node_point(aes(size=centrality, color=group), show.legend=F)  + scale_size(range=c(5,15))+geom_node_text(aes(label=name), repel=T,   size=5, family="nanumgothic")+ theme_graph() 