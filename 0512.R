library(tidyverse) 
P<-read_csv("C:/Users/admin/Desktop/TXTM/news_comment_parasite.csv")
P

library(stringr)
p0 <- P %>% select(reply) %>% mutate(reply=str_replace_all(reply, "[^∞°-∆R]"," "),
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

pvpa <- p2 %>% filter(str_detect(word, "/pv|pa")) %>% mutate(word=str_replace(word, "/.*$","¥Ÿ"))
pvpa

p3 <- bind_rows(noun, pvpa) %>% filter(str_count(word)>=2) %>% arrange(id)
p3

install.packages("widyr")
library(widyr)
p4 <- p3 %>% pairwise_count(item = word, feature = id, sort=T)
p4

p4 %>% filter(item1=="øµ»≠")

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

v=which(str_detect(p3$word, "∞®µ∂")==TRUE)
p3$word[v]

p31 <- p3 %>% mutate(word=ifelse(str_detect(word, "∞®µ∂"), "∞®µ∂", word),
word=ifelse(str_detect(word, "√‡«œ"),"√‡»≠", word))
v=