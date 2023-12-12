#Necessary: extraneous files. 
# Author: Anne Meisner
scripts <- read.csv("scripts_metadata.csv", sep = "\t")

# char check
acts <- read.csv("actors_metadata.csv")

sum(acts$EXTRACTED_WORDS) # 3,136,683
acts <- acts[acts$CHARACTER_RACE != "cf" & acts$CHARACTER_RACE != "p" & acts$CHARACTER_RACE != "nw",]
nrow(acts) # number of actors: 4058

table(acts$CHARACTER_RACE)

length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="a"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="b"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="i"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="l"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="n"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="s"]))
length(unique(acts$ACTOR_NAME[acts$CHARACTER_RACE=="w"]))

bact <- ssubact[ssubact$CHARACTER_RACE == "b",]
eact <- ssubact[ssubact$CHARACTER_RACE == "a",]
iact <- ssubact[ssubact$CHARACTER_RACE == "i",]
lact <- ssubact[ssubact$CHARACTER_RACE == "l",]
nact <- ssubact[ssubact$CHARACTER_RACE == "n",]
sact <- ssubact[ssubact$CHARACTER_RACE == "s",]
wact <- ssubact[ssubact$CHARACTER_RACE == "w",]


length(unique(bact$SCRIPT_ID)) 
length(unique(eact$SCRIPT_ID)) 
length(unique(iact$SCRIPT_ID)) 
length(unique(lact$SCRIPT_ID)) 
length(unique(nact$SCRIPT_ID)) 
length(unique(sact$SCRIPT_ID)) 
length(unique(wact$SCRIPT_ID)) 

