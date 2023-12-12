setwd("~/Desktop/txtLAB/R&F2")
acts <- read.csv("actors_metadata.csv", header=T)

#subset by group
w.sub<-subset(acts, acts$CHARACTER_RACE == "w")
ea.sub<-subset(acts, acts$CHARACTER_RACE == "a")
b.sub<-subset(acts, acts$CHARACTER_RACE == "b")
i.sub<-subset(acts, acts$CHARACTER_RACE == "i")
l.sub<-subset(acts, acts$CHARACTER_RACE == "l")
sa.sub<-subset(acts, acts$CHARACTER_RACE == "s")
a.sub <- rbind(ea.sub, sa.sub) # group East Asian and South Asian for census purposes

#population counts per group (from 2000 census)
us.pop <- 282200000
w.pop <- .691*us.pop
a.pop <- .036*us.pop
b.pop <- .121*us.pop
i.pop <- .0078*us.pop
l.pop <- .124*us.pop

# REPRESENTATION BY # OF CHARACTERS
#build dataframe to use for contingency table loop 
char.rep.df<-data.frame(matrix(c(nrow(a.sub), a.pop, nrow(b.sub), b.pop, nrow(i.sub), i.pop, nrow(l.sub), l.pop, nrow(w.sub), w.pop), ncol=2, byrow=T)) 

#run loop to get representation odds (character)
rep.df<-NULL
for (n in 1:nrow(char.rep.df)){
  char<-char.rep.df$X1[n]
  not.char<-(nrow(acts)-char.rep.df$X1[n])
  race<-char.rep.df$X2[n]
  not.race<-(us.pop-char.rep.df$X2[n])
  # build contingency table
  temp.race.df<-data.frame(matrix(c(char, not.char, race, not.race), ncol=2, byrow=T))
  # run fisher's odds
  model.race<-fisher.test(temp.race.df)
  pv<-model.race$p.value
  odds<-model.race$estimate
  temp.df<-rbind(c(odds, pv))
  rep.df<-rbind(rep.df, temp.df)
}
row.names(rep.df) <- c("asian", "black", "indigenous", "latinx", "white")
char.rep <- rep.df

# REPRESENTATION BY # OF WORDS
#build dataframe to use for contingency table loop 
word.rep.df<-data.frame(matrix(c(sum(a.sub$EXTRACTED_WORDS), a.pop, sum(b.sub$EXTRACTED_WORDS), b.pop, sum(i.sub$EXTRACTED_WORDS), i.pop, sum(l.sub$EXTRACTED_WORDS), l.pop, sum(w.sub$EXTRACTED_WORDS), w.pop), ncol=2, byrow=T)) 

#run loop to get representation odds (words)
rep.df<-NULL
for (n in 1:nrow(word.rep.df)){
  word<-word.rep.df$X1[n]
  not.word<-sum(acts$EXTRACTED_WORDS)-word.rep.df$X1[n]
  race<-word.rep.df$X2[n]
  not.race<-us.pop-word.rep.df$X2[n]
  # build contingency table
  temp.race.df<-data.frame(matrix(c(word, not.word, race, not.race), ncol=2, byrow=T))
  # run fisher's odds
  model.race<-fisher.test(temp.race.df)
  pv<-model.race$p.value
  odds<-model.race$estimate
  temp.df<-rbind(c(odds, pv))
  rep.df<-rbind(rep.df, temp.df)
}
row.names(rep.df) <- c("asian", "black", "indigenous", "latinx", "white")
word.rep <- rep.df
