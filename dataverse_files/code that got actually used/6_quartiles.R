####### Quartile Analysis ####### 
# Author: Anne Meisner
library("stats")

setwd("~/Desktop")
actor.data<-read.csv("actors_metadata.csv", header = T)
script.data<-read.csv("scripts_metadata.csv", header = T, sep = "")


actor.data$SCRIPT_ID<-factor(actor.data$SCRIPT_ID)
script_list<-levels(actor.data$SCRIPT_ID)

all.q25<-NULL
all.q50<-NULL
all.q75<-NULL
all.q100<-NULL

race_quartiles<-function(race, invert=FALSE){
  if (!invert){
    rq25<-all.q25[all.q25$CHARACTER_RACE==race,]
    nq25<-nrow(all.q25)
    nrq25<-nrow(rq25)
    q25.frac<-nrq25/nq25
  
    rq50<-all.q50[all.q25$CHARACTER_RACE==race,]
    nq50<-nrow(all.q50)
    nrq50<-nrow(rq50)
    q50.frac<-nrq50/nq50
  
    rq75<-all.q75[all.q75$CHARACTER_RACE==race,]
    nq75<-nrow(all.q75)
    nrq75<-nrow(rq75)
    q75.frac<-nrq75/nq75
  
    rq100<-all.q100[all.q100$CHARACTER_RACE==race,]
    nq100<-nrow(all.q100)
    nrq100<-nrow(rq100)
    q100.frac<-nrq100/nq100
  
    return(c("25%"=q25.frac, "50%"=q50.frac, "75%"=q75.frac, "100%"=q100.frac))
  }
  else{
    rq25<-all.q25[all.q25$CHARACTER_RACE!=race,]
    nq25<-nrow(all.q25)
    nrq25<-nrow(rq25)
    q25.frac<-nrq25/nq25
    
    rq50<-all.q50[all.q50$CHARACTER_RACE!=race,]
    nq50<-nrow(all.q50)
    nrq50<-nrow(rq50)
    q50.frac<-nrq50/nq50
    
    rq75<-all.q75[all.q75$CHARACTER_RACE!=race,]
    nq75<-nrow(all.q75)
    nrq75<-nrow(rq75)
    q75.frac<-nrq75/nq75
    
    rq100<-all.q100[all.q100$CHARACTER_RACE!=race,]
    nq100<-nrow(all.q100)
    nrq100<-nrow(rq100)
    q100.frac<-nrq100/nq100
    
    return(c("25%"=q25.frac, "50%"=q50.frac, "75%"=q75.frac, "100%"=q100.frac))
  }
}

race_gender_quartiles<-function(race, gender, invert=FALSE){
  if(!invert){
    rq25<-all.q25[all.q25$CHARACTER_RACE==race & all.q25$GENDER==gender,]
    nq25<-nrow(all.q25)
    nrq25<-nrow(rq25)
    q25.frac<-nrq25/nq25
  
    rq50<-all.q50[all.q50$CHARACTER_RACE==race & all.q25$GENDER==gender,]
    nq50<-nrow(all.q50)
    nrq50<-nrow(rq50)
    q50.frac<-nrq50/nq50
  
    rq75<-all.q75[all.q75$CHARACTER_RACE==race & all.q25$GENDER==gender,]
    nq75<-nrow(all.q75)
    nrq75<-nrow(rq75)
    q75.frac<-nrq75/nq75
  
    rq100<-all.q100[all.q100$CHARACTER_RACE==race & all.q25$GENDER==gender,]
    nq100<-nrow(all.q100)
    nrq100<-nrow(rq100)
    q100.frac<-nrq100/nq100
  
    return(c("25%"=q25.frac, "50%"=q50.frac, "75%"=q75.frac, "100%"=q100.frac))
  }
  else{
    rq25<-all.q25[all.q25$CHARACTER_RACE!=race & all.q25$GENDER==gender,]
    nq25<-nrow(all.q25)
    nrq25<-nrow(rq25)
    q25.frac<-nrq25/nq25
    
    rq50<-all.q50[all.q50$CHARACTER_RACE!=race & all.q50$GENDER==gender,]
    nq50<-nrow(all.q50)
    nrq50<-nrow(rq50)
    q50.frac<-nrq50/nq50
    
    rq75<-all.q75[all.q75$CHARACTER_RACE!=race & all.q75$GENDER==gender,]
    nq75<-nrow(all.q75)
    nrq75<-nrow(rq75)
    q75.frac<-nrq75/nq75
    
    rq100<-all.q100[all.q100$CHARACTER_RACE!=race & all.q100$GENDER==gender,]
    nq100<-nrow(all.q100)
    nrq100<-nrow(rq100)
    q100.frac<-nrq100/nq100
    
    return(c("25%"=q25.frac, "50%"=q50.frac, "75%"=q75.frac, "100%"=q100.frac))
  }
}

race_quartiles_raw<-function(race, invert=FALSE){
  if (!invert){
    rq25<-all.q25[all.q25$CHARACTER_RACE==race,]
    nrq25<-nrow(rq25)
  
    rq50<-all.q50[all.q50$CHARACTER_RACE==race,]
    nrq50<-nrow(rq50)
  
    rq75<-all.q75[all.q75$CHARACTER_RACE==race,]
    nrq75<-nrow(rq75)
  
    rq100<-all.q100[all.q100$CHARACTER_RACE==race,]
    nrq100<-nrow(rq100)
    
    return(c("25%"=nrq25, "50%"=nrq50, "75%"=nrq75, "100%"=nrq100))
  }
  else{
    rq25<-all.q25[all.q25$CHARACTER_RACE!=race,]
    nrq25<-nrow(rq25)
    
    rq50<-all.q50[all.q50$CHARACTER_RACE!=race,]
    nrq50<-nrow(rq50)
    
    rq75<-all.q75[all.q75$CHARACTER_RACE!=race,]
    nrq75<-nrow(rq75)
    
    rq100<-all.q100[all.q100$CHARACTER_RACE!=race,]
    nrq100<-nrow(rq100)
    
    return(c("25%"=nrq25, "50%"=nrq50, "75%"=nrq75, "100%"=nrq100))
  }
}

race_gender_quartiles_raw<-function(race, gender,invert=FALSE){
  if (!invert){
    rq25<-all.q25[all.q25$CHARACTER_RACE==race & all.q25$GENDER==gender,]
    nrq25<-nrow(rq25)
  
    rq50<-all.q50[all.q50$CHARACTER_RACE==race & all.q50$GENDER==gender,]
    nrq50<-nrow(rq50)
  
    rq75<-all.q75[all.q75$CHARACTER_RACE==race & all.q75$GENDER==gender,]
    nrq75<-nrow(rq75)
  
    rq100<-all.q100[all.q100$CHARACTER_RACE==race & all.q100$GENDER==gender,]
    nrq100<-nrow(rq100)
  
    return(c("25%"=nrq25, "50%"=nrq50, "75%"=nrq75, "100%"=nrq100))
  }
  else{
    rq25<-all.q25[all.q25$CHARACTER_RACE!=race & all.q25$GENDER==gender,]
    nrq25<-nrow(rq25)
    
    rq50<-all.q50[all.q50$CHARACTER_RACE!=race & all.q50$GENDER==gender,]
    nrq50<-nrow(rq50)
    
    rq75<-all.q75[all.q75$CHARACTER_RACE!=race & all.q75$GENDER==gender,]
    nrq75<-nrow(rq75)
    
    rq100<-all.q100[all.q100$CHARACTER_RACE!=race & all.q100$GENDER==gender,]
    nrq100<-nrow(rq100)
    
    return(c("25%"=nrq25, "50%"=nrq50, "75%"=nrq75, "100%"=nrq100))
  }
}

build_chisq<-function(race, gender="all"){
  if (gender=="all"){
    set<-race_quartiles_raw(race)
    nonset<-race_quartiles_raw(race, invert=TRUE)
    return(rbind(set,nonset))
  }
  else{
    set<-race_gender_quartiles_raw(race, gender)
    nonset<-race_gender_quartiles_raw(race, gender, invert=TRUE)
    return(rbind(set,nonset))
  }
}

quartile_var<-function(r.quarts){
  total<-sum(r.quarts)
  exp_q<-total/4 # the predicted number of characters given a completely even distr. of character roles
  q.25<-r.quarts[1]-exp_q
  q.50<-r.quarts[2]-exp_q
  q.75<-r.quarts[3]-exp_q
  q.100<-r.quarts[4]-exp_q
  return(c(q.25,q.50,q.75,q.100)/exp_q)
}


for (i in 1:length(script_list)){
  movie<-actor.data[actor.data$SCRIPT_ID==script_list[i],]
  qw<-quantile(movie$EXTRACTED_WORDS)
  q25<-unname(qw[2])
  q50<-unname(qw[3])
  q75<-unname(qw[4])
  
  movie.q25<-subset(movie, movie$EXTRACTED_WORDS <= q25)
  movie.q50<-subset(movie, movie$EXTRACTED_WORDS <= q50 & movie$EXTRACTED_WORDS > q25)
  movie.q75<-subset(movie, movie$EXTRACTED_WORDS <= q75 & movie$EXTRACTED_WORDS > q50)
  movie.q100<-subset(movie, movie$EXTRACTED_WORDS > q75)
  
  all.q25<-rbind(all.q25, movie.q25)
  all.q50<-rbind(all.q50, movie.q50)
  all.q75<-rbind(all.q75, movie.q75)
  all.q100<-rbind(all.q100, movie.q100)
}

a.quarts<-race_quartiles_raw("a")
b.quarts<-race_quartiles_raw("b")
i.quarts<-race_quartiles_raw("i")
l.quarts<-race_quartiles_raw("l")
n.quarts<-race_quartiles_raw("n")
s.quarts<-race_quartiles_raw("s")
w.quarts<-race_quartiles_raw("w")

a.var<-quartile_var(a.quarts)
b.var<-quartile_var(b.quarts)
i.var<-quartile_var(i.quarts)
l.var<-quartile_var(l.quarts)
n.var<-quartile_var(n.quarts)
s.var<-quartile_var(s.quarts)
w.var<-quartile_var(w.quarts)





############## Relevant ####################
var_output<-rbind(a.var,b.var,i.var,l.var,n.var,s.var,w.var)
row.names(var_output)<-c("e asian","black","indig","latinx","n east","s asian", "white")




##### dealing w the quartiles -- bootstrappin assuming an even distrib is what we would see. 
test<-rmultinom(10000, nrow(w.sub), c(0.25, 0.25, 0.25, 0.25))

determine_skew<-function(r.quarts){
  total<-sum(r.quarts)
  test<-rmultinom(10000, total, c(0.25, 0.25, 0.25, 0.25))
  qs<-quantile(test[1,],c(.025,0.925))
  bottom<-qs[1]
  top<-qs[2]
  sig<-NULL
  for (i in 1:length(r.quarts)){
    if (r.quarts[i] >= top){
      sig<-c(sig, 1)
    }
    if (r.quarts[i] <= bottom){
      sig<-c(sig, -1)
    }
    if (r.quarts[i] > bottom & r.quarts[i] < top){
      sig<-c(sig, 0)
    }
  }
  return(rbind(r.quarts,sig))
}

a_skew<-determine_skew(a.quarts)
b_skew<-determine_skew(b.quarts)
i_skew<-determine_skew(i.quarts)
l_skew<-determine_skew(l.quarts)
n_skew<-determine_skew(n.quarts)
s_skew<-determine_skew(s.quarts)
w_skew<-determine_skew(w.quarts)

################ THIS SHOWS WHO IS SIGNIF ###################
all_skews<-rbind("e asian skew"=a_skew[2,],"black skew"=b_skew[2,],"indig skew"=i_skew[2,],
                 "latinx skew"=l_skew[2,],"n east"=n_skew[2,],"s asian skew"=s_skew[2,],"white skew"=w_skew[2,])
