##### Top 2 Actors in each film! #####
# Author: Anne Meisner
setwd("~/Desktop")
actor.data<-read.csv("actors_metadata.csv", header = T)
script.data<-read.csv("scripts_metadata.csv", header = T, sep = "")


script_list<-levels(actor.data$SCRIPT_ID)
topact<-NULL
secact<-NULL
onlyact<-NULL
bothact<-NULL

for (i in 1:length(script_list)){
  movie<-actor.data[actor.data$SCRIPT_ID==script_list[i],]
  
  if (nrow(movie)>1){
    act1<-movie[movie$EXTRACTED_WORDS==max(movie$EXTRACTED_WORDS),]
    topact<-rbind(topact,act1)
    
    movie<-subset(movie, movie$CHARACTER_ID!=act1$CHARACTER_ID)
    act2<-movie[movie$EXTRACTED_WORDS==max(movie$EXTRACTED_WORDS),]
    secact<-rbind(secact,act2)
  }
  else{
    act1<-movie[movie$EXTRACTED_WORDS==max(movie$EXTRACTED_WORDS),]
    onlyact<-rbind(onlyact,act1)
  }
}
no_char <- 0
for (i in 1:length(script_list)){
  movie<-actor.data[actor.data$SCRIPT_ID==script_list[i],]
  
  if (nrow(movie)>1){
    act1<-movie[movie$EXTRACTED_WORDS==max(movie$EXTRACTED_WORDS),]
    
    movie<-subset(movie, movie$CHARACTER_ID!=act1$CHARACTER_ID)
    act2<-movie[movie$EXTRACTED_WORDS==max(movie$EXTRACTED_WORDS),]
    
    bothact<-rbind(bothact,act1[1,],act2[1,])
  }
  else{no_char<- no_char+1}
}
print(no_char)
analysis<-function(subset){
  # number of roles
  roles<-nrow(subset)
  #number of actors
  subset$ACTOR_NAME<-factor(subset$ACTOR_NAME)
  actors<-nlevels(subset$ACTOR_NAME)
  
  #gender breakdown
  women<-nrow(subset[subset$GENDER=="f",])
  wacts<-(subset[subset$GENDER=="f",])
  wacts$ACTOR_NAME<-factor(wacts$ACTOR_NAME)
  wacts<-nlevels(wacts$ACTOR_NAME)
  
  men<-nrow(subset[subset$GENDER=="m",])
  macts<-(subset[subset$GENDER=="m",])
  macts$ACTOR_NAME<-factor(macts$ACTOR_NAME)
  macts<-nlevels(macts$ACTOR_NAME)
  
  # number of mixed race
  if (subset$CHARACTER_RACE!="w"){
    mix<-subset[subset$PART_WHITE=="y",]
    frac_mix<-nrow(mix)/roles
    mix$ACTOR_NAME<-factor(mix$ACTOR_NAME)
    rept_mix<-nlevels(mix$ACTOR_NAME)
    
    return(c("roles"=roles,"actors"=actors, "men"=men,"unique male"=macts, "women"=women,"unique female"=wacts, 
             "mixed actors"=nrow(mix), "mixed %"=frac_mix, "unique mixed"=rept_mix))
  }
  else{
    return(c("roles"=roles,"actors"=actors, "men"=men, "unique male"=macts,"women"=women,"unique female"=wacts,
             "mixed actors"=0, "mixed %"=0, "unique mixed"=0))
  }
}

both_white<-function(a,b){
  if (a$CHARACTER_RACE==b$CHARACTER_RACE & a$CHARACTER_RACE=="w"){
    return(1)
  }
  else{
    return(0)
  }
}
both_nwhite<-function(a,b){
  if (a$CHARACTER_RACE!="w" & b$CHARACTER_RACE!="w"){
    return(1)
  }
  else{
    return(0)
  }
}
mixed_pair<-function(a,b){
  if ((a$CHARACTER_RACE!=b$CHARACTER_RACE) & (a$CHARACTER_RACE=="w" | b$CHARACTER_RACE=="w")){
    return(1)
  }
  else{
    return(0)
  }
}
first_w<-function(a,b){
  if (a$CHARACTER_RACE=="w"){
    return(1)
  }
  else{
    return(0)
  }
}
first_m<-function(a,b){
  if (a$GENDER=="m"){
    return(1)
  }
  else{
    return(0)
  }
}
first_f<-function(a,b){
  if (a$GENDER=="f"){
    return(1)
  }
  else{
    return(0)
  }
}
both_m<-function(a,b){
  if (a$GENDER=="m" & b$GENDER=="m"){
    return(1)
  }
  else{
    return(0)
  }
}
both_f<-function(a,b){
  if (a$GENDER=="f" & b$GENDER=="f"){
    return(1)
  }
  else{
    return(0)
  }
}
gender_mix<-function(a,b){
  if (a$GENDER!=b$GENDER){
    return(1)
  }
  else{
    return(0)
  }
}

wtop<-topact[topact$CHARACTER_RACE=="w",]
btop<-topact[topact$CHARACTER_RACE=="b",]
ltop<-topact[topact$CHARACTER_RACE=="l",]
itop<-topact[topact$CHARACTER_RACE=="i",]
stop<-topact[topact$CHARACTER_RACE=="s",]
ntop<-topact[topact$CHARACTER_RACE=="n",]
atop<-topact[topact$CHARACTER_RACE=="a",]

wsec<-secact[secact$CHARACTER_RACE=="w",]
bsec<-secact[secact$CHARACTER_RACE=="b",]
lsec<-secact[secact$CHARACTER_RACE=="l",]
isec<-secact[secact$CHARACTER_RACE=="i",]
ssec<-secact[secact$CHARACTER_RACE=="s",]
nsec<-secact[secact$CHARACTER_RACE=="n",]
asec<-secact[secact$CHARACTER_RACE=="a",]

awt<-analysis(wtop)
aws<-analysis(wsec)

abt<-analysis(btop)
abs<-analysis(bsec)

alt<-analysis(ltop)
als<-analysis(lsec)

ait<-analysis(itop)
ais<-analysis(isec)

ant<-analysis(ntop)
ans<-analysis(nsec)

ast<-analysis(stop)
ass<-analysis(ssec)

aat<-analysis(atop)
aas<-analysis(asec)

top_char_race<-rbind("black"=abt, "east asian"=aat,"indigenous"=ait,"latinx"=alt,"near eastern"=ant, "south asian"=ast,"white"=awt)
sec_char_race<-rbind("black"=abs, "east asian"=aas,"indigenous"=ais,"latinx"=als,"near eastern"=ans, "south asian"=ass,"white"=aws)

chi<-NULL
for (i in 1:nrow(top_char_race)){
  chi<-cbind(chi,c(top_char_race[i,1],sec_char_race[i,1]))
}
colnames(chi)<-row.names(top_char_race)
chisq.test(chi)
chi<-round(rbind(chi, chi[1,]/chi[2,]), 3)
row.names(chi)<-c("top","sec","frac")

pairstats<-NULL
#for (i in 1:nlevels(bothact$SCRIPT_ID)){
for (i in 1:(nrow(bothact)/2)){
  a<-bothact[(2*i)-1,]
  b<-bothact[(2*i),]
  #id <- levels(bothact$SCRIPT_ID)[i]
  #sub <- bothact[bothact$SCRIPT_ID==id,]
  #a<-sub[1,]
  #b<-sub[2,]
  bw<-both_white(a,b)
  bnw<-both_nwhite(a,b)
  mp<-mixed_pair(a,b)
  if (mp==1){
    fw<-first_w(a,b)
  }
  else{
    fw<-0
  }
  bm<-both_m(a,b)
  bf<-both_f(a,b)
  gm<-gender_mix(a,b)
  if (gm==1){
    fm<-first_m(a,b)
    ff<-first_f(a,b)
  }
  else{
    fm<-0
    ff<-0
  }
  
  row<-c("script"=as.numeric(as.character(a$SCRIPT_ID)),"both w"=bw,
         "both nw"=bnw,"mix pair"=mp,"1st white"=fw, "both m"=bm, "both f"=bf,
         "mix gender"=gm, "first m"=fm, "first f"=ff)
  pairstats<-rbind(pairstats,row)
}

top_char_output<-colSums(pairstats)
# counts below in top_char_output :-) 
a<-data.frame(c(633,767-633),c(31,767-31), row.names = c("top", "other"))
colnames(a)<-c("white","not white")
fisher.test(a)
fisher.test(data.frame(c(59,103-59),c(44,103-44)))

