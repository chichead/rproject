install.packages("ggraph")
install.packages("tidygrahp")

library(tidyverse)
library(ggraph)
library(tidygraph)
library(dplyr)

setwd("/Users/admin/R_project/202104_assembly")

#-------------------------------------------
# 결측치 확인
#-------------------------------------------
df.final.co <- read.csv("20대_0428_최종.csv")
df.final.co.origin <- read.csv("20대_0428_최종.csv")

colSums(is.na(df.final.co)) # co.member.no에서 결측치 발견
df.final.co[is.na(df.final.co$number), ] %>% View()

library(stringr)
df.final.co$co.member.name <- str_squish(df.final.co$co.member.name)

df.final.co %>% select(co.member.no, co.member.name) %>% unique() %>% write.csv(., "naming.csv")

# no na-value 임의값 넣어주기
number_clear <- read.csv("number_clear_20.csv")
df.final.co <- df.final.co %>% left_join(number_clear, by = "co.member.name")
df.final.co <- unique(df.final.co)
colSums(is.na(df.final.co))

# 시스템상 오류로 보임 김성태 2중으로 들어가면서 유니크값에서 빠짐 / 아마도 밑에 들어가는 의원은 김성태2인듯
df.final.co[298627, ] <- c("PRC_E1D8H0H8T3D0O1V7O1E5Q1E5S7V7X6",
                           "[2015185]국무총리 및 국무위원 출석요구의 건(국정에 관한 교섭단체 대표연설)(홍영표의원 등 3인 외 263인)",
                           9000194,
                           "김성태(미래통합당/金成泰)",
                           9000194)
df.final.co$number <- as.integer(df.final.co$number)

n_distinct(df.final.co$co.index)

df.final.co[is.na(df.final.co$number), ]
df.fianl.co %>% select(co.member.no, co.member.name, number) %>% unique() %>% write.csv(., "naming2.csv")


# df.final.co[is.na(df.final.co)] <- 1111111 # 임의로 김진애 의원 no 발급(21대)

# 정제된 노드값 가져오기
node_clear <- read.csv("node_clear_20.csv") # 정제된 노드값 가져오기
colnames(node_clear)[1] <- "number"
df.final.co <- df.final.co %>% left_join(node_clear, by = "number")
colSums(is.na(df.final.co))


df.final.co <- df.final.co %>% select(1, 2, 5, 8)
colnames(df.final.co)[3] <- "co.member.no"
colnames(df.final.co)[4] <- "co.member.name"
colSums(is.na(df.final.co))


# 잠깐. 법안에 얼마나 타 정당 의원들이 참여했는지 살펴보자

df.final.co2 <- df.final.co %>% select(1, 2, 5, 6, 7, 8)
df.net2 <- df.final.co2 %>% 
  group_by(co.index) %>% 
  mutate(indexing = row_number()) %>%
  ungroup()

df.percent <- df.net2 %>% group_by(co.title, party) %>%
  summarise(n = n()) %>%
  mutate(percent = prop.table(n))

df.main.name <- df.net2 %>% filter(indexing == 1) # 대표발의자 정당만 빼놓기

df.main.name <- df.main.name %>% left_join(df.percent, by = c("co.title", "party"))
write.csv(df.main.name, "얼마나타정당이원들이참여했는지살펴보자.csv")
mean(df.main.name$percent)










#-------------------------------------------
# from, to 다시 정제
#-------------------------------------------

df.net <- df.final.co %>% 
  group_by(co.index) %>% 
  mutate(indexing = row_number()) %>%
  ungroup() 


# df.net$co.member.name <- str_replace(df.net$co.member.name, "\\(", "-")
# df.net$co.member.name <- str_remove(df.net$co.member.name, "\\/.+")
# df.net$co.member.name <- str_squish(df.net$co.member.name)


df.net <- df.net %>%
  mutate(from = case_when(
    indexing == 1 ~ co.member.name,
    TRUE ~ NA_character_)) %>%
  mutate(from.no = case_when(
      indexing == 1 ~ co.member.no)) 

df.net <- df.net %>% fill(from, from.no)


colnames(df.net)[4] <- "to"
colnames(df.net)[3] <- "to.no"


#-------------------------------------------
# 대표발의자는 제거
#-------------------------------------------

df.net <- df.net %>% 
  filter(indexing != 1) %>% 
  select(-indexing) %>% 
  select(from, to)

edge.clear <- df.net
colnames(edge.clear)[1] <- "Target"
colnames(edge.clear)[2] <- "Source"

write.csv(edge.clear, "edge_clear_20.csv", row.names = FALSE)

#df.net <- df.net %>% 
  #separate(from, into = c("from.name", "from.party"), sep = "-", remove = FALSE) %>% 
  #separate(to, into = c("to.name", "to.party"), sep = "-", remove = FALSE)
  #filter(from.party != "") %>% 
  #filter(!str_detect(to, "-$")) 


#-------------------------------------------
# 의안정보시스템에 의원명에 정당 빠진 게 있음
# 일단 샘플 시각화용으로는 지우고 했는데 정당을 추가해도 됨
#-------------------------------------------

# df.net$from <- str_replace(df.net$from, "미래통합당", "국민의힘")
# df.net$to <- str_replace(df.net$to, "미래통합당", "국민의힘")

# df.net <- df.net %>% 
#   select(from, to)

write.csv(df.net, "node_assemble.csv")

# 네트워크 알고리즘은 cetrality_eigen으로 했음. 중심점 강조 알고리즘.

df.net <- df.net %>% 
  as_tbl_graph() %>% 
  mutate(eigen = centrality_eigen(),
         group = group_infomap()) 

#-------------------------------------------
# 샘플 시각화 제작
#-------------------------------------------
df.net %>% 
  as_tbl_graph() %>% 
  mutate(eigen = centrality_eigen(),
         group = group_infomap()) %>% 
  ggraph(layout = "nicely") +
  geom_edge_link(alpha = .2, color = "gray50") +
  geom_node_point(aes(color = factor(group), size = eigen), alpha = .3) +
  geom_node_text(aes(label = name), size = 1.5) +
  theme_graph()
