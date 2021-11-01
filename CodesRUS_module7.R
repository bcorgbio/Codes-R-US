
#Pagel’s Lambda

library(geiger)
library(dplyr)
library(ggplot2)
library(MuMIn)

m.phy <- read.tree("mammal.tree.pruned.txt")
m.phy$tip.label <- gsub("(\\w+)_(\\w+)","\\1 \\2",m.phy$tip.label)


m.phy0<- rescale(m.phy, model = "lambda", 0)
m.phy5 <- rescale(m.phy, model = "lambda", 0.5)

par(mfcol = c(1, 3))
plot(m.phy)
plot(m.phy5)
plot(m.phy0)

library(ape)
library(phytools)

sim.trait <- runif(Ntip(m.phy))

names(sim.trait) <- m.phy$tip.label

print(sim.trait)

sim.sig <- phylosig(m.phy,sim.trait,method="lambda",test=T)

print(sim.sig)

mammal.temp<-read.csv("mammal.temp2.csv")
mammal.temp <- mammal.temp%>%
  mutate(T.delta = abs(T.high-T.low)
  )

mammal.temp.log <- mammal.temp %>%
  mutate_at(c("mass.g", "T.high", "T.low", "T.delta"), log)

###question 1

##generate graph of body mass vs.temperature difference 
mammal.temp.log%>%
  ggplot(aes(mass.g,T.delta))+geom_point()+geom_smooth(method = "lm")

##generate graph of body mass vs.maximum temperature
mammal.temp.log%>%
  ggplot(aes(mass.g,T.high))+geom_point()+geom_smooth(method = "lm")

##generate graph of body mass vs.minimum temperature 
mammal.temp.log%>%
  ggplot(aes(mass.g,T.low))+geom_point()+geom_smooth(method = "lm")


###question 2

##evaluate lamda value and p-value to find signficant phylogentic signal in the four variables

mass.trait <- mammal.temp$mass.g
names(mass.trait) <- mammal.temp.log$species
print(mass.trait)
mass.trait <- phylosig(m.phy,mass.trait,method="lambda",test=T)
print(mass.trait)

Tdelta.trait <- mammal.temp$T.delta
names(Tdelta.trait) <- mammal.temp.log$species
print(Tdelta.trait)
Tdelta.trait <- phylosig(m.phy,Tdelta.trait,method="lambda",test=T)
print(Tdelta.trait)

T.low.trait <- mammal.temp$T.low
names(T.low.trait) <- mammal.temp.log$species
print(T.low.trait)
T.low.trait <- phylosig(m.phy,T.low.trait,method="lambda",test=T)
print(T.low.trait)

T.high.trait <- mammal.temp$T.high
names(T.high.trait) <- mammal.temp.log$species
print(T.high.trait)
T.high.trait <- phylosig(m.phy,T.high.trait,method="lambda",test=T)
print(T.high.trait)

##           Lamda        p-value
## mass      0.999934     1.83952e-11 
## T.delta   0.580389     0.00776833 
## T.low     0.791644     0.00298335
## T.high    0.869769     0.0409622 


## Mass is most is most significant signal because it has highest Phylogenetic signal lambda and lowest P value. 
