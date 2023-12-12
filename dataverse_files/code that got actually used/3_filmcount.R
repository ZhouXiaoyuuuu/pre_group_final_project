### how many films does it take before a group's representation is covered? ###
# Author: Anne Meisner & Victoria Svaikovsky
setwd("~/Desktop/ANNE")
actor.data<-read.csv("actors_metadata.csv", header = T)
script.data<-read.csv("scripts_metadata.csv", header = T, sep = "")

actor.data$SCRIPT_ID<-factor(actor.data$SCRIPT_ID)


film_count<-function(race, gender="all"){
  if (gender=="all"){
    subset<-actor.data[actor.data$CHARACTER_RACE==race,]
    subset$SCRIPT_ID<-factor(subset$SCRIPT_ID)
    return(nlevels(subset$SCRIPT_ID))
  }
  else{
    subset<-actor.data[actor.data$CHARACTER_RACE==race,]
    subset<-subset[subset$GENDER==gender,]
    subset$SCRIPT_ID<-factor(subset$SCRIPT_ID)
    return(nlevels(subset$SCRIPT_ID))
  }
}

race_count<-function(race, gender="all"){
  if (gender=="all"){
    subset<-actor.data[actor.data$CHARACTER_RACE==race,]
    return(nrow(subset))
  }
  else{
    subset<-actor.data[actor.data$CHARACTER_RACE==race,]
    subset<-subset[subset$GENDER==gender,]
    return(nrow(subset))
  }
}

a<-film_count("a")
b<-film_count("b")
i<-film_count("i")
l<-film_count("l")
n<-film_count("n")
s<-film_count("s")
w<-film_count("w")

ar<-race_count("a")
br<-race_count("b")
ir<-race_count("i")
lr<-race_count("l")
nr<-race_count("n")
sr<-race_count("s")
wr<-race_count("w")


## counts used in paper here :) 
film_count_sum<-c("asian"=a,"black"=b,"indig"=i,"latinx"=l,
                  "n east"=n,"s asian"=s, "white"=w)
race_count_sum<-c("asian"=ar,"black"=br,"indig"=ir,"latinx"=lr,
                  "n east"=nr,"s asian"=sr, "white"=wr)

fa<-film_count("a", "f")
fb<-film_count("b", "f")
fi<-film_count("i", "f")
fl<-film_count("l", "f")
fn<-film_count("n", "f")
fs<-film_count("s", "f")
fw<-film_count("w", "f")

far<-race_count("a", "f")
fbr<-race_count("b", "f")
fir<-race_count("i", "f")
flr<-race_count("l", "f")
fnr<-race_count("n", "f")
fsr<-race_count("s", "f")
fwr<-race_count("w", "f")

film_count_fsum<-c("asian"=fa,"black"=fb,"indig"=fi,"latinx"=fl,
                  "n east"=fn,"s asian"=fs, "white"=fw)
race_count_fsum<-c("asian"=far,"black"=fbr,"indig"=fir,"latinx"=flr,
                  "n east"=fnr,"s asian"=fsr, "white"=fwr)

ma<-film_count("a", "m")
mb<-film_count("b", "m")
mi<-film_count("i", "m")
ml<-film_count("l", "m")
mn<-film_count("n", "m")
ms<-film_count("s", "m")
mw<-film_count("w", "m")

mar<-race_count("a", "m")
mbr<-race_count("b", "m")
mir<-race_count("i", "m")
mlr<-race_count("l", "m")
mnr<-race_count("n", "m")
msr<-race_count("s", "m")
mwr<-race_count("w", "m")

film_count_msum<-c("asian"=ma,"black"=mb,"indig"=mi,"latinx"=ml,
                  "n east"=mn,"s asian"=ms, "white"=mw)
race_count_msum<-c("asian"=mar,"black"=mbr,"indig"=mir,"latinx"=mlr,
                  "n east"=mnr,"s asian"=msr, "white"=mwr)

