### geotagging ### 
# Author: Anne Meisner
library("plyr")

setwd("~/Desktop/anne/ANNE")
actor.data<-read.csv("actors_metadata.csv", header = T)
script.data<-read.csv("scripts_metadata.csv", header = T, sep = "")

actor.data$SCRIPT_ID<-factor(actor.data$SCRIPT_ID)
script_list<-levels(actor.data$SCRIPT_ID)

#pulled from filmcount.R
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

# Helper fns. generate subsets. 
race_sub<-function(actor.data, race){
  return(actor.data[(actor.data$CHARACTER_RACE == race),])
}
no_race_sub<-function(actor.data, race){
  return(actor.data[!(actor.data$CHARACTER_RACE == race),])
}
gender_sub<-function(actor.data, gender){
  return(actor.data[(actor.data$GENDER == gender),])
}
no_gender_sub<-function(actor.data, gender){
  return(actor.data[!(actor.data$GENDER == gender),])
}
gen_race_sub<-function(actor.data, gender, race){
  sub<-subset(actor.data, actor.data$GENDER == gender)
  return(sub[(sub$CHARACTER_RACE == race),])
}
no_gen_race_sub<-function(actor.data, gender, race){
  no.sub<-actor.data[!(actor.data$GENDER == gender),]
  return(no.sub[!(no.sub$CHARACTER_RACE == race),])
}
build_subs<-function(actor.data, gender, race){
  if (race=="all" & gender=="all"){
    return(NULL)
  }
  if (gender=="all"){
    return(race_sub(actor.data, race))
  }
  if (race=="all"){
    return(gender_sub(actor.data, gender))
  }
  else{
    return(gen_race_sub(actor.data, gender, race))
  }
}

# Sum location references. 
sum_loc_refs<-function(geo.race){
  total_refs<-sum(geo.race[,!(names(geo.race) %in% c("CHAR_ID","North.America.and.Australasia"))])
  return(colSums(geo.race[,!(names(geo.race) %in% c("CHAR_ID","North.America.and.Australasia"))])/total_refs)
}
sum_loc_refs2<-function(geo.race){
  total_refs<-sum(geo.race[,!(names(geo.race) %in% "CHAR_ID")])
  return(colSums(geo.race[,!(names(geo.race) %in% "CHAR_ID")])/total_refs)
}

# Geoscraped dataframe generated from python script: geotag.py
geonums<-read.csv("geonum.csv", stringsAsFactors = T, sep = '\t')
geonums<-geonums[geonums$CHAR_ID %in% actor.data$CHARACTER_ID,]

f.sub<-gender_sub(actor.data, gender="f")
m.sub<-gender_sub(actor.data, gender="m")

w.sub<-race_sub(actor.data, race="w")
b.sub<-race_sub(actor.data, race="b")
l.sub<-race_sub(actor.data, race="l")
a.sub<-race_sub(actor.data, race="a")
s.sub<-race_sub(actor.data, race="s")
n.sub<-race_sub(actor.data, race="n")
i.sub<-race_sub(actor.data, race="i")
# 
# wf.sub<-gen_race_sub(actor.data, gender="f", race="w")
# bf.sub<-gen_race_sub(actor.data, gender="f", race="b")
# lf.sub<-gen_race_sub(actor.data, gender="f", race="l")
# af.sub<-gen_race_sub(actor.data, gender="f", race="a")
# sf.sub<-gen_race_sub(actor.data, gender="f", race="s")
# if.sub<-gen_race_sub(actor.data, gender="f", race="i")
# 
# wm.sub<-gen_race_sub(actor.data, gender="m", race="w")
# bm.sub<-gen_race_sub(actor.data, gender="m", race="b")
# lm.sub<-gen_race_sub(actor.data, gender="m", race="l")
# am.sub<-gen_race_sub(actor.data, gender="m", race="a")
# sm.sub<-gen_race_sub(actor.data, gender="m", race="s")
# nm.sub<-gen_race_sub(actor.data, gender="m", race="n")
# im.sub<-gen_race_sub(actor.data, gender="m", race="i")

ar<-race_count("a")
br<-race_count("b")
ir<-race_count("i")
lr<-race_count("l")
nr<-race_count("n")
sr<-race_count("s")
wr<-race_count("w")

race_count_sum<-c("asian"=ar,"black"=br,"indig"=ir,"latinx"=lr,
                  "n east"=nr,"s asian"=sr, "white"=wr)


geo.a<-geonums[geonums$CHAR_ID %in% a.sub$CHARACTER_ID,]
geo.b<-geonums[geonums$CHAR_ID %in% b.sub$CHARACTER_ID,]
geo.i<-geonums[geonums$CHAR_ID %in% i.sub$CHARACTER_ID,]
geo.l<-geonums[geonums$CHAR_ID %in% l.sub$CHARACTER_ID,]
geo.n<-geonums[geonums$CHAR_ID %in% n.sub$CHARACTER_ID,]
geo.s<-geonums[geonums$CHAR_ID %in% s.sub$CHARACTER_ID,]
geo.w<-geonums[geonums$CHAR_ID %in% w.sub$CHARACTER_ID,]

ref_count_sum<-c("asian"=nrow(geo.a),"black"=nrow(geo.b),"indig"=nrow(geo.i),"latinx"=nrow(geo.l),
                 "n east"=nrow(geo.n),"s asian"=nrow(geo.s), "white"=nrow(geo.w))

ref_count_sum/race_count_sum

#all_races<-sum_loc_refs(geonums)
all_races<-sum_loc_refs(geonums[!(geonums$CHAR_ID %in% geo.w$CHAR_ID),])


# Ultimately, this measure was excluded from publication. 
bayes_predict<-function(geo.race, all_races, geonums){
  output<-NULL
  geonums<-geonums[!(geonums$CHAR_ID %in% geo.w$CHAR_ID),]
  #pB<-nrow(geo.race)/nrow(geonums)
  pA<-sum(geo.race[,!(names(geo.race) %in% c("CHAR_ID","North.America.and.Australasia"))])/sum(geonums[,!(names(geonums) %in% c("CHAR_ID","North.America.and.Australasia"))])
  race_ps<-sum_loc_refs(geo.race)
  for (i in 1:length(all_races)){
    pB<-all_races[i]
    pBA<-race_ps[i]
    
    pAB<-(pBA*pA)/pB
    output<-c(output,pAB)
  }
  return(output)
}

by_movie<-function(geo.race){
  baysian<-NULL
  geo_actors<-actor.data[actor.data$CHARACTER_ID %in% geo.race$CHAR_ID,]
  geo_actors$SCRIPT_ID<-factor(geo_actors$SCRIPT_ID)
  geo_scripts<-levels(geo_actors$SCRIPT_ID)
  
  for (i in 1:length(geo_scripts)){
    script<-geo_scripts[i]
    movie<-actor.data[actor.data$SCRIPT_ID == script,]
    movie_geotagged<-geonums[geonums$CHAR_ID %in% movie$CHARACTER_ID,]
    movie_georace<-geo.race[geo.race$CHAR_ID %in% movie$CHARACTER_ID,]
  
    bayes<-bayes_predict(movie_georace, sum_loc_refs(movie_geotagged), movie)
    baysian<-rbind(baysian,c(bayes,as.numeric(script)))
  }
  return(baysian)
}

nam_distr<-geonums[which(geonums$North.America.and.Australasia>0),]
actors_nam<-actor.data[actor.data$CHARACTER_ID %in% nam_distr$CHAR_ID,]
nam_actor_counts<-count(actors_nam$CHARACTER_RACE)
total_namref<-sum(geonums$North.America.and.Australasia)
totalrefs<-c("a"=0,"b"=0,"i"=0,"l"=0,"n"=0,"s"=0,"w"=0)

for (i in 1:nrow(nam_distr)){
  num<-nam_distr[i,]$North.America.and.Australasia
  char_id<-nam_distr[i,]$CHAR_ID
  race<-actors_nam[actors_nam$CHARACTER_ID==char_id,]
  race<-as.character(race$CHARACTER_RACE)
  totalrefs[race]<-totalrefs[race] + num
}

totalrefs_frac<-totalrefs/total_namref
#pulled comparisons to movie demographics from filmcount.R -- not different p much except for white
#otherwise static. 

a_bayes<-bayes_predict(geo.a, all_races, geonums)
b_bayes<-bayes_predict(geo.b, all_races, geonums)
i_bayes<-bayes_predict(geo.i, all_races, geonums)
l_bayes<-bayes_predict(geo.l, all_races, geonums)
n_bayes<-bayes_predict(geo.n, all_races, geonums)
s_bayes<-bayes_predict(geo.s, all_races, geonums)

# rel_region: the relevant region for the race group as a data frame of only char_ids that ref it.
# drop all other columns but the one we're gonna use so it's under [,2]
against_w_comp<-function(geo.race, rel_region){
  race_reg<-rel_region[rel_region$CHAR_ID %in% geo.race$CHAR_ID,]
  white_reg<-rel_region[rel_region$CHAR_ID %in% geo.w$CHAR_ID,]
  race_sum<-sum(race_reg[,2])
  white_sum<-sum(white_reg[,2])
  total_sum<-sum(rel_region[,2])
  return(c("white"=white_sum, "compared"=race_sum))#/total_sum)
}

carrib<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$Caribbean > 0),]$CHAR_ID,
                         "Caribbean" = geonums[which(geonums$Caribbean > 0),]$Caribbean))
africa<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$Sub.Saharan.Africa > 0),]$CHAR_ID,
                         "Sub.Saharan.Africa" = geonums[which(geonums$Sub.Saharan.Africa > 0),]$Sub.Saharan.Africa))
e_asia<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$East.Asia > 0),]$CHAR_ID, 
                         "East.Asia" = geonums[which(geonums$East.Asia > 0),]$East.Asia))
s_am<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$South.and.Central.America > 0),]$CHAR_ID, 
                       "South.and.Central.America" = geonums[which(geonums$South.and.Central.America > 0),]$South.and.Central.America))
m_east<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$North.Africa.and.Middle.East > 0),]$CHAR_ID, 
                         "North.Africa.and.Middle.East" = geonums[which(geonums$North.Africa.and.Middle.East > 0),]$North.Africa.and.Middle.East))
s_asia<-data.frame(cbind("CHAR_ID" = geonums[which(geonums$South.Asia > 0),]$CHAR_ID, 
                         "South.Asia" = geonums[which(geonums$South.Asia > 0),]$South.Asia))

region_list<-c(africa,e_asia,s_am,m_east,s_asia)
geo_list<-list("asian"=geo.a,"black"=geo.b,"indig"=geo.i,"latinx"=geo.l,
            "n east"=geo.n,"s asian"=geo.s, "white"=geo.w)

against_alt_comp<-function(geo.race, geo.race2, rel_region){
  race_reg<-rel_region[rel_region$CHAR_ID %in% geo.race$CHAR_ID,]
  race2_reg<-rel_region[rel_region$CHAR_ID %in% geo.race2$CHAR_ID,]
  race_sum<-sum(race_reg[,2])
  race2_sum<-sum(race2_reg[,2])
  total_sum<-sum(rel_region[,2])
  return(c("race"=race_sum, "race2"=race2_sum))#/total_sum)
}

# fisher odds now on differences with the raw counts! :) 
build_fish<-function(geo.race, rel_region, index){ # only for white
  white_comp<-against_w_comp(geo.race, rel_region)
  white_raw<-ref_count_sum[7]
  #race_raw<-race_count_sum[index]
  race_raw<-ref_count_sum[index]
  df<-data.frame("race"=c(white_comp[2],race_raw),"white"=c(white_comp[1],white_raw))
  fish<-fisher.test(df)
  return(fish)
}
build_fish2<-function(geo.race, geo.race2, rel_region, index, index2){
  alt_comp<-against_alt_comp(geo.race, geo.race2, rel_region)
  alt_raw<-ref_count_sum[index2]
  race_raw<-ref_count_sum[index]
  df<-data.frame("race1"=c(alt_comp[1],race_raw),"race2"=c(alt_comp[2],alt_raw))
  fish<-fisher.test(df)
  return(fish$estimate)
}
build_var_fish<-function(region){
  totals<-NULL
  for (i in 1:length(geo_list)){
    row<-NULL
    for (j in 1:length(geo_list)){
      fisher<-build_fish2(data.frame(geo_list[i]), data.frame(geo_list[j]), region, i, j)
      row<-c(row,fisher)
    }
    totals<-rbind(totals)
  }
  colnames(totals)<-names(geo_list)
  row.names(totals)<-names(geo_list)
  return(totals)
}

fish_e_asia<-build_fish(geo.a, e_asia, 1)
fish_s_asia<-build_fish(geo.s, s_asia, 6)
fish_n_east<-build_fish(geo.n, m_east, 5)
fish_latinx<-build_fish(geo.l, s_am, 4)


