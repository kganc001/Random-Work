library(statnet)
library(igraph)
library(GGally)
library(sna)
library(ggplot2)
library(tidyverse)

setwd("c:/users/kimberly/desktop/prospectus/ergm")

sl <- read.csv("clean_sl_data.csv")

sl <- sl[which(is.na(sl$latitude)==FALSE),]
sl <- sl[which(is.na(sl$longitude)==FALSE),]


dist <- read.csv("sl_distance.csv")

dist <- dist[which(is.na(dist$latitude_a)==FALSE),]
dist <- dist[which(is.na(dist$longitude_a)==FALSE),]
dist <- dist[which(is.na(dist$latitude_b)==FALSE),]
dist <- dist[which(is.na(dist$longitude_b)==FALSE),]
dist <- dist[which(dist$provstate != "Unknown"),]

sl <- merge(sl,dist)

sl1 <- sl[,12:13]


slNet <- network(sl1, directed = TRUE)
summary(slNet ~ edges + ostar(2) + istar(2) + triangle + m2star + asymmetric + transitive + mutual)


#NULL ERGM
nullErgm <- ergm(slNet ~ edges, eval.loglik=TRUE)
summary(nullErgm) #AIC: 179.8; BIC: 182.7; p < .05




nullGof <- gof(nullErgm, GOF = ~ idegree + odegree + dspartners + espartners)
plot(nullGof)




#ASYMMETRIC ERGM
aErgm <- ergm(slNet ~ edges + asymmetric, eval.loglik=TRUE)
summary(aErgm) #AIC: 119.2; BIC: 125; edges not sig; asymmetric ***

mcmc.diagnostics(aErgm)


aGof <- gof(aErgm, GOF = ~ idegree + odegree + dspartners + espartners)
plot(aGof)




#OUT DEGREE POPULARITY ERGM
odErgm <- ergm(slNet ~ edges + odegreepopularity, eval.loglik=TRUE)
summary(odErgm) #AIC: 174.8; BIC: 180.6; edges ***; odegreepopularity ***

mcmc.diagnostics(odErgm)

odGof <- gof(odErgm, GOF = ~ idegree + odegree + dspartners + espartners)
plot(odGof)




#IN DEGREE POPULARITY ERGM
idErgm <- ergm(slNet ~ edges + idegreepopularity, eval.loglik=TRUE)
summary(idErgm) #AIC: 173.6; BIC: 179.3; edges ***; idegreepopularity ***

mcmc.diagnostics(idErgm)

idGof <- gof(idErgm, GOF = ~ idegree + odegree + dspartners + espartners)
plot(idGof)


#DISTANCE ERGM
dist <- as.matrix(sl$distance)

dErgm <- ergm(slNet ~ edges + edgecov(dist) + asymmetric, eval.loglik=TRUE)
summary(dErgm) #AIC: 170.5; BIC: 176.3; edges .; distance **




dGof <- gof(dErgm ~ model)
dGof

plot(dGof)

globalGof <- gof(dErgm ~ esp + distance)
plot(globalGof)



##




#DISTANCE AND ASYMMETRY
d1Ergm <- ergm(slNet ~ edges + edgecov(dist) +  asymmetric, directed = T)
summary(d1Ergm) #AIC: 113.5; BIC: 122.1; edges ***; distance ***; asymmetric ***

mcmc.diagnostics(d1Ergm)





