###Project Code

#1

#load libraries
library(tidyverse)
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#load data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

# merging anole.eco to anole data set
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>% ##omitted any rows of tibble that had missing values
  print()

#mutated collective tibble containing morphological and ecological data
anole.log <- anole2 %>%
  mutate_at(c("SVL","HTotal","PH","ArbPD"), log)


#redefined anole.log as a tibble containing this new column
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

# mutating and redefining anole.log data to include a column for phylogenetically corrected residuals
anole.log <- anole.log %>%
  mutate(phylo.res=residuals(pgls.BM2))
p.eco.phylo <- anole.log %>%
  ggplot(aes(x=Ecomorph2, y=phylo.res))+geom_boxplot()+stat_summary(fun=mean, geom="point", size=3)
print(p.eco.phylo)


#establish covariation matrix with tree & assume the characters evolved under evolutionary model
anole.tree <- read.tree("anole.tre")
plot(anole.tree, cex=0.4)


#2

#Constructing two simple linear models that assesses effect of PH and ArbPD by including them as covariates in the models
anole.log.arbpd.lm <- lm(HTotal~SVL+ArbPD, anole.log)
summary(anole.log.arbpd.lm)
anova(anole.log.arbpd.lm)

anole.log.ph.lm <- lm(HTotal~SVL+PH, anole.log)
summary(anole.log.ph.lm)
anova(anole.log.ph.lm)


#3

#Exploring how both ArbPD and PH effect the hindlimb-SVL relationship
#Plotting the residuals of the simple linear models against covariates

#computing the residuals based on global anole.log.arpd.lm and anole.log.ph.lm model
#used mutate to establish a new res column that contains the residuals
#redefined anole.log as a tibble containing this new column

#before
anole.log
anole.log <- anole.log %>%
  mutate(pd.res=residuals(anole.log.arbpd.lm))
#after adding ArbPD residuals
anole.log
anole.log <- anole.log %>%
  mutate(ph.res=residuals(anole.log.ph.lm))
#after adding PH residuals
anole.log

#plotting the residuals against covariates
p.pd <- anole.log %>%
  ggplot(aes(x=Ecomorph2,y=pd.res)) + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)
print(p.pd)

p.ph <- anole.log %>%
  ggplot(aes(x=Ecomorph2,y=ph.res)) + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)
print(p.ph)

#4
#Constructing phylogenetic least squares models of the hindlimb-SVL relationships that include the unique combinations of these two covariates

#PGLS model with the hindlimb-SVL relationship + perch height (under BM & OU)
pgls.BM1 <- gls(HTotal~SVL+PH, correlation=corBrownian(1, phy=anole.tree, form=~Species), data=anole.log, method="ML")
pgls.OU1 <- gls(HTotal~SVL+PH, correlation=corMartins(0, phy=anole.tree, form=~Species), data=anole.log, method="ML")
#PGLS model with the hindlimb-SVL relationship + perch diameter (under BM & OU)
pgls.BM2 <- gls(HTotal~SVL+ArbPD, correlation=corBrownian(1, phy=anole.tree, form=~Species), data=anole.log, method="ML")
pgls.OU2 <- gls(HTotal~SVL+ArbPD, correlation=corMartins(0, phy=anole.tree, form=~Species), data=anole.log, method="ML")
#PGLS model with the hindlimb-SVL relationship + perch height & diameter (under BM & OU)
pgls.BM3 <- gls(HTotal~SVL+PH+ArbPD, correlation=corBrownian(1, phy=anole.tree, form=~Species), data=anole.log, method="ML")
pgls.OU3 <- gls(HTotal~SVL+PH+ArbPD, correlation=corMartins(0, phy=anole.tree, form=~Species), data=anole.log, method="ML")

#5
#Assessing the fit of each model using AICc and AICw 
#Comment whether one or both of the covariates is a significant predictor of hindlimb length in a phylogenetic context

#perform AIC operations to see which models fit that data best
anole.phylo.aic <- AICc(pgls.BM1, pgls.BM2, pgls.BM3, pgls.OU1, pgls.OU2, pgls.OU3)
aicw(anole.phylo.aic$AICc)
# results of the AIC indicate that the phylogenetically corrected regression model that includes the hindlimb-SVL relationship w/ perch height & diameter with traits evolving under BM is the best fit
# lowest values = best fit
# both covariates are significant predictors of hindlimb length in a phylogenetic context

#6 

#Plot that visualizes the differences in residual values of the covariates on the hindlimb residuals
anole.log %>%
  dplyr::select(Ecomorph2, res, pd.res, ph.res) %>%
  pivot_longer(cols=c("res", "pd.res", "ph.res")) %>%
  print %>%
  ggplot(aes(x=Ecomorph2, y=value)) +geom_boxplot() + stat_summary(fun=mean, geom="point", size=2) + facet_grid(name~., scales="free_y") + ylab("residual")

#residuals of hindlimb-SVL relationship considering effects of covariates (BM3)
#mutating and redefining anole.log data to include a column for residuals relating to the best fitting PGLS model
#plotting them against Ecomorph2
anole.log <- anole.log %>%
  mutate(PHPD.res=residuals(pgls.BM3))
anole.log
p.eco.phpd <- anole.log %>%
  ggplot(aes(x=Ecomorph2, y=PHPD.res)) + geom_boxplot() + stat_summary(fun=mean, geom="point", size=2)
print(p.eco.phpd)
