# 21대도 동일하게 진행-------------------------

#-------------------------------------------
# 결측치 확인
#-------------------------------------------
df.final.co.21 <- read.csv("21대_210415_최종.csv")
df.final.co.origin.21 <- read.csv("21대_210415_최종.csv")

colSums(is.na(df.final.co.21)) # co.member.no에서 결측치 발견
df.final.co.21[is.na(df.final.co.21$co.member.no), ] %>% View()

library(stringr)
df.final.co.21$co.member.name <- str_squish(df.final.co.21$co.member.name)

# no na-value 임의값 넣어주기
df.final.co.21[is.na(df.final.co.21)] <- 1111111 # 임의로 김진애 의원 no 발급(21대)

n_distinct(df.final.co.21$co.index)


# 정제된 노드값 가져오기
node_clear <- read.csv("node_clear.csv") # 정제된 노드값 가져오기
colnames(df.final.co.21)[3] <- "number"
colnames(node_clear)[1] <- "number"
df.final.co.21 <- df.final.co.21 %>% left_join(node_clear, by = "number")
colSums(is.na(df.final.co.21))




# 잠깐. 법안에 얼마나 타 정당 의원들이 참여했는지 살펴보자

df.final.co2.21 <- df.final.co.21 %>% select(1, 2, 3, 5, 6, 7)
df.net2.21 <- df.final.co2.21 %>% 
  group_by(co.index) %>% 
  mutate(indexing = row_number()) %>%
  ungroup()

df.percent.21 <- df.net2.21 %>% group_by(co.title, party) %>%
  summarise(n = n()) %>%
  mutate(percent = prop.table(n))

df.main.name.21 <- df.net2.21 %>% filter(indexing == 1) # 대표발의자 정당만 빼놓기

df.main.name.21 <- df.main.name.21 %>% left_join(df.percent.21, by = c("co.title", "party"))

write.csv(df.main.name, "얼마나타정당이원들이참여했는지살펴보자.csv")
mean(df.main.name.21$percent)


#-------------------------------------------
# 끼리끼리 비율을 보려면 원 데이터를 써야함
#-------------------------------------------

# 21대

df.net.new <- df.final.co.origin.21  %>% 
  group_by(co.index) %>% 
  mutate(indexing = row_number()) %>%
  ungroup() 

df.net.new$co.member.name <- str_replace(df.net.new$co.member.name, "\\(", "-")
df.net.new$co.member.name <- str_remove(df.net.new$co.member.name, "\\/.+")
df.net.new$co.member.name <- str_squish(df.net.new$co.member.name)

df.net.new <- df.net.new %>% 
  separate(co.member.name, into = c("name", "party"), sep = "-", remove = F)

df.percent.new <- df.net.new  %>% group_by(co.title, party) %>%
  summarise(n = n()) %>%
  mutate(percent = prop.table(n))

df.main.name.new <- df.net.new %>% filter(indexing == 1) # 대표발의자 정당만 빼놓기

df.main.name.new <- df.main.name.new %>% left_join(df.percent.new, by = c("co.title", "party"))

mean(df.main.name.new$percent)


# 20대

df.net.new <- df.final.co.origin  %>% 
  group_by(co.index) %>% 
  mutate(indexing = row_number()) %>%
  ungroup() 

df.net.new$co.member.name <- str_replace(df.net.new$co.member.name, "\\(", "-")
df.net.new$co.member.name <- str_remove(df.net.new$co.member.name, "\\/.+")
df.net.new$co.member.name <- str_squish(df.net.new$co.member.name)

df.net.new <- df.net.new %>% 
  separate(co.member.name, into = c("name", "party"), sep = "-", remove = F)

df.percent.new <- df.net.new  %>% group_by(co.title, party) %>%
  summarise(n = n()) %>%
  mutate(percent = prop.table(n))

df.main.name.new <- df.net.new %>% filter(indexing == 1) # 대표발의자 정당만 빼놓기

df.main.name.new <- df.main.name.new %>% left_join(df.percent.new, by = c("co.title", "party"))

mean(df.main.name.new$percent)
