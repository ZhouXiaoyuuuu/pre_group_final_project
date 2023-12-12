# Author: Anne Meisner

options(scipen=999)
library(ggplot2)
library(ggrepel)
library(tidyverse)


custom <- palette(c("#294F3C", # later: east asian: 488C6A, south asian: 115634   lighter 4c9470
                    "#ECA72C", # black    lighter f4cb86
                    "#9AC5CE", # indig    lighter bad8de
                    "#D86A2B", # latinx   lighter e8a782
                    "#FFFFFF", # other
                    "#40798C", # white: B9314F    lighter 73acbf
                    "#81B29A"))

lighter_custom <- palette(c("#436956", # asian
                            "#FFC146", # black
                            "#B4DFE8", # indig
                            "#F28445", # latinx
                            "#FFFFFF", # other
                            "#5A93A6"# white
                            ))

custom1 <- palette(c("#294F3C", # later: east asian: 488C6A, south asian: 115634
                    "#ECA72C", # black
                    "#9AC5CE", # indig
                    "#D86A2B", # latinx
                    "#151C5E", # other1E287F
                    "#40798C", # white 151C5E
                    "#81B29A"))


custom2 <- palette(c("#ECA72C", # black
                    "#488C6A", # east asian
                    "#9AC5CE", # indigenous  
                    "#D86A2B", # latinx
                    "#151C5E", # near eastern
                    "#115634", # south asian
                    "#40798C")) # white



genre_palette <- palette(c("#878787",
                           "#8C3863",
                           "#BB7E8C",
                           "#C9B6BE",
                           "#EDD7E0",
                           "#494949"))

genre_palette2 <- palette(c("#878787",
                           "#B9314F",
                           "#494949",
                           "#C9B6BE",
                           "#EDD7E0",
                           "#494949"))

pi_colors <- palette(c("#488C6A",
                       "#FFFFFF",
                       "#488C6A"))



quartile_data<-read.csv("quartile_rep.csv")
quartile_data$quartile<-factor(quartile_data$quartile, unique(quartile_data$quartile))
write.csv(quartile_data,"quartile_rep.csv")
library("RColorBrewer")

g1 <- ggplot(quartile_data, aes(x = quartile, y = score, alpha=factor(significant))) +
  geom_bar(aes(fill = race), position = "dodge", stat="identity") +
  theme_bw() +
  scale_alpha_manual(values = c("y"=1,"n"=1), guide=FALSE) +
  labs(title = "Quartile Representation",y = "Percentage Over/Underrepresented", x = "Quartile", fill = "Race")+
  scale_x_discrete(name="Quartile")+#,limits=quartile_data$quartile)+#,labels=c("25","50","75","100")) +
  scale_fill_manual(values = custom2) + 
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        axis.line = element_line(colour = "black"))#,
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank())
plot(g1)


topg2a <- data.frame(group=c("both men", "both women","man, woman","man, woman"), 
                     type=c("all","all","m-w","w-m"),
                     count=c(361, 46, 238, 142))

topg2a %>% #read_table() %>%
  filter(type!="all") %>%
  mutate(group = as.factor(group) %>% fct_reorder(count, sum)) %>%
  arrange(group, count) %>%
  mutate(type = as.factor(type) %>% fct_reorder2(group, count))

lvl0 <- tibble(group = "Parent", count = 0, level = 0, fill = NA)
lvl1 <- topg2a %>%
  group_by(group) %>%
  summarise(count = sum(count)) %>% 
  ungroup() %>%
  mutate(level= 1) %>%
  mutate(fill = group)

lvl2 <- topg2a %>%
  select(group = type, count, fill = group) %>%
  mutate(level = 2)

bind_rows(lvl0, lvl1, lvl2) %>%
  mutate(group = as.factor(group) %>% fct_reorder2(fill, count)) %>%
  arrange(fill, group) %>%
  mutate(level = as.factor(level)) %>%
  ggplot(aes(x = level, y = count, fill = fill, alpha = level)) +
  geom_col(width = 1, color = "gray90", size = 0.7, position = position_stack()) +
  #geom_text(aes(label = group), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = .5), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values = custom2, na.translate = F) +
  labs(x = NULL, y = NULL, title = "Who Speaks the Most? Gender of Top Two\nCharacters", fill= "Top Pair Genders") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))




# taken from output of topchar.R (check variable "top_char_output")
topr2a <- data.frame(group=c("both white", "both not white","white, not white","white, not white"), 
                     type=c("all","all","fw","fnw"),
                     count=c(633, 25, 59, 44))

topr2a %>% #read_table() %>%
  filter(type!="all") %>%
  mutate(group = as.factor(group) %>% fct_reorder(count, sum)) %>%
  arrange(group, count) %>%
  mutate(type = as.factor(type) %>% fct_reorder2(group, count))

lvl0 <- tibble(group = "Parent", count = 0, level = 0, fill = NA)
lvl1 <- topr2a %>%
  group_by(group) %>%
  summarise(count = sum(count)) %>% 
  ungroup() %>%
  mutate(level= 1) %>%
  mutate(fill = group)

lvl2 <- topr2a %>%
  select(group = type, count, fill = group) %>%
  mutate(level = 2)

bind_rows(lvl0, lvl1, lvl2) %>%
  mutate(group = as.factor(group) %>% fct_reorder2(fill, count)) %>%
  arrange(fill, group) %>%
  mutate(level = as.factor(level)) %>%
  ggplot(aes(x = level, y = count, fill = fill, alpha = level)) +
  geom_col(width = 1, color = "gray90", size = 0.5, position = position_stack()) +
  #geom_text(aes(label = group), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = .8), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values = custom2, na.translate = F) +
  labs(x = NULL, y = NULL) +
  labs(fill = "Top Pair Races", title = "Who Speaks the Most? Race of Top Two Characters")+
  theme_minimal() +
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 10, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 10))




#script <- read.csv("graphtier_script.csv")
#script$GENRE <- gsub("romcom","comedy",script$GENRE)

g4 <- ggplot(script, aes(fill=GENRE, x = YEAR)) + 
#g4<-ggplot(script, aes(x = YEAR, ..count..)) +
  geom_histogram(binwidth = 10, col = "white") +
  #geom_bar(width = 0.5, position="stack",  fill = GENRE)+
  theme_bw() + 
  scale_fill_manual(values = custom1) +
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "Number of Films", y = "Years", title = "Genre Distribution of Data by 10 Years", fill = "Genre")
plot(g4)



vicky <- read.csv("overall.race.word.char.csv", header = T, stringsAsFactors = F)
chars_perc<-vicky$per.char
pop_perc<-vicky$per.pop

v2 <- vicky[,1:4]

g5 <- ggplot(v2, aes(x = 1, y = per.char, fill = X)) +
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) +
  #geom_text(aes(label = group), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = .5), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values= custom, na.translate = F) +
  labs(fill = "Race", title = "Percentage of Characters by Race")+
  labs(x = NULL, y = NULL) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))
plot(g5) # char


g6 <- ggplot(vicky, aes(x = 1, y = per.pop, fill = X)) +
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) +
  #geom_text(aes(label = group), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = .5), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_manual(values= custom, na.translate = F) +
  labs(x = NULL, y = NULL) +
  labs(fill = "Race", title = "Percentage of Population by Race") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))
plot(g6) # pop

g7 <- ggplot(vicky, aes(x = 1, y = per.words, fill = X)) +
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) +
  #geom_text(aes(label = group), size = 2.5, position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  #scale_alpha_manual(values = c("0" = 0, "1" = 1, "2" = .5), guide = F) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  #scale_fill_brewer(palette = "Dark2", na.translate = F) +
  scale_fill_manual(values = custom, na.translate = F) +
  labs(x = NULL, y = NULL, title = "Percentage of Words Spoken by Race") +
  labs(fill="Race") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11))
plot(g7) # words

g7.2a <- ggplot(a, aes(x="", y = a, fill = cats)) + 
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) + 
  coord_polar(theta = "y") + 
  scale_x_discrete(breaks=NULL) +
  scale_fill_manual(values = pi_colors, na.translate = F) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()
plot(g7.2a)
a2 <- data.frame(c("asian"=new[5,], "other"=100-new[5,]))
a2 <- cbind(a2, c("asian","other"))
colnames(a2) <- c("vals", "cats")
g7.2a2 <- ggplot(a2, aes(x="", y = vals, fill = cats)) + 
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) + 
  coord_polar(theta = "y") + 
  scale_x_discrete(breaks=NULL) +
  scale_fill_manual(values = pi_colors, na.translate = F) +
  scale_y_continuous(breaks=NULL) +
  theme_minimal()
plot(g7.2a2)







gw <- read.csv("genre.by.words.csv", header = T, stringsAsFactors = F)
nrow(gw)
wg <- data.frame(t(gw))



nw <- col_sums(gw[1:5,2:7])
genre <- names(nw)
nw_g <- data.frame(t(rbind(nw,genre)))
nw_g$nw <- as.numeric(as.character(nw_g$nw))
nw_g$nw <- round(nw_g$nw * 100, 2)

g8 <- ggplot(genres.df, aes(x = genre, y = perc_nw, fill = genre)) +
  geom_bar(stat="identity", width = 0.5) +
  theme_bw() +
  labs(x = "Genre", y = "Percent", 
       title = "Percentage of Not-White Characters by Genre") +
  #scale_fill_brewer(palette = "Spectral")
  coord_cartesian(ylim=c(0,100))+
  scale_fill_manual(values = custom1) + 
  theme(plot.title = element_text(size = 13, family = "Georgia", face = "bold"),
        text = element_text(size = 11, family = "Georgia"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11), legend.position= "none")
plot(g8)


new <- c("latinx" = 12.5, "white"= 69.1, "black"=12.1, "indig"=0.7, "asian" = 3.6, "other"= 2)
# based on the year 2000 census. 