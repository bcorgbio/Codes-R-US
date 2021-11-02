

#Pagel’s Lambda

library(geiger)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(nlme)

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

##           Lambda        p-value
## mass      0.999934     1.83952e-11 
## T.delta   0.580389     0.00776833 
## T.low     0.791644     0.00298335
## T.high    0.869769     0.0409622 


## Mass is most the significant signal because it has highest phylogenetic signal lambda and lowest P value. 


### Question 3

##After we find mass is the most significant signal, we use BM and OU model to test which variable is the best covariance with mass.(T.high?T.low?or T.delta?)
pgls.BM1 <- gls(mass.g~ T.high, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.BM2 <- gls(mass.g~ T.low, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.BM3 <- gls(mass.g~T.delta, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.log.BM1 <- gls(mass.g~ T.high, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
pgls.log.BM2 <- gls(mass.g~ T.low, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
pgls.log.BM3 <- gls(mass.g~T.delta, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")

treenew<-m.phy
treenew$edge.length<-treenew$edge.length*1000

pgls.OU1 <- gls(mass.g~ T.high, correlation = corMartins(0, phy = treenew, form=~species), data = mammal.temp, method = "ML")
pgls.OU2<- gls(mass.g~ T.low , correlation= corMartins(0, phy=treenew, form = ~species), data = mammal.temp, method = "ML")
pgls.OU3 <- gls(mass.g~T.delta, correlation = corMartins(0, phy = treenew, form=~species), data = mammal.temp, method = "ML")
pgls.log.OU1 <- gls(mass.g~ T.high, correlation = corMartins(0, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")
pgls.log.OU2<- gls(mass.g~ T.low , correlation= corMartins(0, phy=treenew, form = ~species), data = mammal.temp.log, method = "ML")
pgls.log.OU3 <- gls(mass.g~T.delta, correlation = corMartins(0, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")

##perform AIC test
mammal.temp.phylo.aic<-AICc(pgls.log.BM1,pgls.log.BM2,pgls.log.BM3,pgls.log.OU1,pgls.log.OU2,pgls.log.OU3)
aicw(mammal.temp.phylo.aic$AICc)

##    fit     delta            w
##1 215.9847  6.087645 4.110737e-02
##2 214.4116  4.514543 9.026394e-02
##3 209.8970  0.000000 8.626505e-01
##4 221.9914 12.094348 2.039767e-03
##5 220.6755 10.778479 3.938384e-03
##6 255.4238 45.526747 1.121573e-10

##Based on our AIC test, the T.delta has the lowest value. The T.delta is the most significant one with the mass. 


### Question 4

##perform ANOVA test for the most significant variable 

anova(pgls.log.BM3)


