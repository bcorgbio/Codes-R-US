#Pagel’s Lambda

library(geiger)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(nlme)
library(ape)
library(phytools)

m.phy <- read.tree("mammal.tree.pruned.txt")
m.phy$tip.label <- gsub("(\\w+)_(\\w+)","\\1 \\2",m.phy$tip.label)


m.phy0<- rescale(m.phy, model = "lambda", 0)
m.phy5 <- rescale(m.phy, model = "lambda", 0.5)

par(mfcol = c(1, 3))
plot(m.phy)
plot(m.phy5)
plot(m.phy0)


sim.trait <- runif(Ntip(m.phy))
names(sim.trait) <- m.phy$tip.label
print(sim.trait)
sim.sig <- phylosig(m.phy,sim.trait,method="lambda",test=T)
print(sim.sig)

mammal.temp <- read.csv("mammal.temp2.csv")
mammal.temp <- mammal.temp%>%
  mutate(T.delta = abs(T.high-T.low))
##print(mammal.temp)
mammal.temp.log <- mammal.temp %>%
  mutate_at(c("mass.g", "T.high", "T.low", "T.delta"), log)
##print(mammal.temp.log)


###question 1

##generate log-log graph of body mass vs.temperature difference 
mammal.temp.log%>%
  ggplot(aes(mass.g,T.delta))+geom_point()+geom_smooth(method = "lm", se = F)

##generate log-log graph of body mass vs.maximum temperature
mammal.temp.log%>%
  ggplot(aes(mass.g,T.high))+geom_point()+geom_smooth(method = "lm", se = F)

##generate log-log graph of body mass vs.minimum temperature 
mammal.temp.log%>%
  ggplot(aes(mass.g,T.low))+geom_point()+geom_smooth(method = "lm", se= F)


## linear models
mammal.diff.lm <- lm(T.delta ~ mass.g, mammal.temp.log)
mammal.high.lm <- lm(T.high ~ mass.g, mammal.temp.log)
mammal.low.lm <- lm(T.low ~ mass.g, mammal.temp.log)

mammal.temp.aic <- AICc(mammal.diff.lm, mammal.high.lm, mammal.low.lm)
mammal.temp.aicw <- aicw(mammal.temp.aic$AICc)
print(mammal.temp.aicw)
# linear model of mammal.high.lm is the best fit given lowest value in chart
summary(mammal.diff.lm)
summary(mammal.high.lm)
summary(mammal.low.lm)

mammal.lm.table <- matrix(c(1.07127, -0.11518, 3.6480114, -0.0010265, 3.577075, 0.003091), ncol=2, byrow = TRUE)
rownames(mammal.lm.table) <- c("Delta", "High", "Low")
colnames(mammal.lm.table) <- c("Intercept", "mass.g")
mammal.lm.table <- as.table(mammal.lm.table)
print(mammal.lm.table)

##Summary for Slope and Intercept values of Linear Models
#Temp Delta
## (Intercept)  1.07127
## mass.g      -0.11518
#Temp High
## (Intercept)  3.6480114
## mass.g      -0.0010265
#Temp Low
## (Intercept) 3.577075
## mass.g      0.003091


###question 2

##evaluate lambda value and p-value to find significant phylogenetic signal in the four variables

mass.trait <- mammal.temp$mass.g
names(mass.trait) <- mammal.temp.log$species
print(mass.trait)
mass.trait <- phylosig(m.phy,mass.trait,method="lambda",test=T)
print(mass.trait)

T.delta.trait <- mammal.temp$T.delta
names(T.delta.trait) <- mammal.temp.log$species
print(T.delta.trait)
T.delta.trait <- phylosig(m.phy,T.delta.trait,method="lambda",test=T)
print(T.delta.trait)

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


### Make an Actual Table

phylo.sig.table <- matrix(c(0.999934, 1.83952e-11, 0.580389, 0.00776833, 0.791644, 0.00298335, 0.869769, 0.0409622), ncol=2, byrow = TRUE)
rownames(phylo.sig.table) <- c("Mass", "T.delta", "T.low", "T.high")
colnames(phylo.sig.table) <- c("Lamba", "p-value")
phylo.sig.table <- as.table(phylo.sig.table)
print(phylo.sig.table)

##           Lambda        p-value
## mass      0.999934     1.83952e-11 
## T.delta   0.580389     0.00776833 
## T.low     0.791644     0.00298335
## T.high    0.869769     0.0409622 


## The mass has the highest Lambda value and lowest p-value. So the most signification phylogentic signal is mass. 
##Other signals are also significant because their lambda values are high and p-values are below 0.05.

### Question 3

##After we find mass is the most significant signal, we use BM and OU model to test which variable is the best covariance with mass.(T.high?T.low?or T.delta?)
## We apply PGLS analysis and BM/OU model to find the best fit
#without PGLS analysis, but use BM model
log.BM1 <- gls(T.high~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
log.BM2 <- gls(T.low~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
log.BM3 <- gls(T.delta~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")

#with PGLS analysis and BM model
pgls.BM1 <- gls(T.high~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.BM2 <- gls(T.low~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.BM3 <- gls(T.delta~mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp, method = "ML")
pgls.log.BM1 <- gls(T.high~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
pgls.log.BM2 <- gls(T.low~ mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")
pgls.log.BM3 <- gls(T.delta~mass.g, correlation = corBrownian(1, phy = m.phy, form =~species), data = mammal.temp.log, method = "ML")

treenew<-m.phy
treenew$edge.length<-treenew$edge.length*1000

#without PGLS analysis,but use OU model
log.OU1 <- gls(T.high~ mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")
log.OU2<- gls(T.low~ mass.g, correlation= corMartins(1, phy=treenew, form = ~species), data = mammal.temp.log, method = "ML")
log.OU3 <- gls(T.delta~mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")

#with PGLS analysis and OU model
pgls.OU1 <- gls(T.high~ mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp, method = "ML")
pgls.OU2<- gls(T.low~ mass.g, correlation= corMartins(1, phy=treenew, form = ~species), data = mammal.temp, method = "ML")
pgls.OU3 <- gls(T.delta~mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp, method = "ML")
pgls.log.OU1 <- gls(T.high~mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")
pgls.log.OU2<- gls(T.low~mass.g, correlation= corMartins(1, phy=treenew, form = ~species), data = mammal.temp.log, method = "ML")
pgls.log.OU3 <- gls(T.delta~mass.g, correlation = corMartins(1, phy = treenew, form=~species), data = mammal.temp.log, method = "ML")

## predictions from PGLS
mammal.temp.log$phy.pred.T.high <- predict(pgls.log.OU1)
mammal.temp.log$phy.pred.T.low <- predict(pgls.log.OU2)
mammal.temp.log$phy.pred.T.delta <- predict(pgls.log.OU3)
##print(mammal.temp.log)

##perform AIC test
mammal.temp.phylo.aic<-AICc(pgls.log.BM1,pgls.log.BM2,pgls.log.BM3,pgls.log.OU1,pgls.log.OU2,pgls.log.OU3)
aicw(mammal.temp.phylo.aic$AICc)

### Make an Actual Table
phylo.aic.table <- matrix(c(-240.80123, 0.988891, 3.788469e-01, -201.22363, 40.566492, 9.644881e-10, 104.08271, 345.872838, 4.873783e-76, -241.79012, 0.000000, 6.211531e-01, -207.29113, 34.498994, 2.003721e-08, 94.73602, 336.526139, 5.217663e-74), ncol=3, byrow = TRUE)
colnames(phylo.aic.table) <- c("fit", "delta", "w")
rownames(phylo.aic.table) <- c("Log.BM.High", "Log.BM.Low", "Log.Bm.Delta", "Log.OU.High", "Log.OU.Low", "Log.OU.Delta")
phylo.aic.table <- as.table(phylo.aic.table)
print(phylo.aic.table)

##fit      delta            w
##1 -240.80123   0.988891 3.788469e-01
##2 -201.22363  40.566492 9.644881e-10
##3  104.08271 345.872838 4.873783e-76
##4 -241.79012   0.000000 6.211531e-01
##5 -207.29113  34.498994 2.003721e-08
##6   94.73602 336.526139 5.217663e-74

##Based on our AIC test, the T.high has the lowest value given the log transformed data under OU. The T.high is the most significant one with the mass. 


### Question 4

##perform ANOVA test for the most significant variable 

anova(pgls.log.OU1)
anova(pgls.log.OU2)
anova(pgls.log.OU3)

summary(pgls.log.OU1)
summary(pgls.log.OU2)
summary(pgls.log.OU3)

coef(pgls.log.OU1)
coef(pgls.log.OU2)
coef(pgls.log.OU3)

mammal.temp.log%>%
  ggplot(aes(mass.g, T.delta)) + geom_point() + geom_smooth(method = "lm", se = F) + geom_line(aes(y=phy.pred.T.delta))
mammal.temp.log%>%
  ggplot(aes(mass.g, T.high)) + geom_point() + geom_smooth(method = "lm", se = F) + geom_line(aes(y=phy.pred.T.high))
mammal.temp.log%>%
  ggplot(aes(mass.g, T.low)) + geom_point() + geom_smooth(method = "lm", se = F) + geom_line(aes(y=phy.pred.T.low))


T.high.phylo.aic <- AICc(pgls.BM1, pgls.OU1, pgls.log.BM1, pgls.log.OU1)
aicw(T.high.phylo.aic$AICc)
print(T.high.phylo.aic)
#Log OU model fits best
anova(pgls.log.OU1)

# Mortola's model
anova(pgls.OU1)

T.low.phylo.aic <- AICc(pgls.BM2, pgls.OU2, pgls.log.BM2, pgls.log.OU2)
aicw(T.low.phylo.aic$AICc)
print(T.low.phylo.aic)
#Log OU model fits the best
anova(pgls.log.OU2)
#

T.delta.phylo.aic <- AICc(pgls.BM3, pgls.OU3, pgls.log.BM3, pgls.log.OU3)
aicw(T.delta.phylo.aic$AICc)
print(T.delta.phylo.aic)
#log OU model fits the best
anova(pgls.log.OU3)
#