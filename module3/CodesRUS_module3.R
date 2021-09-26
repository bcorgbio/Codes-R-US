





#PROJECT REPORT CODE

#2

#Constructing two simple linear models that assesses effect of PH and ArbPD by including them as covariates in the models
anole.log.arbpd.lm <- lm(HTotal~SVL+ArbPD, anole.log)
summary(anole.log.arbpd.lm)
anova(anole.log.arbpd.lm)

anole.log.lm <- lm(HTotal~SVL, anole.log)

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
#after 1
anole.log <- anole.log %>%
  mutate(pd.res=residuals(anole.log.arbpd.lm))
anole.log
#after 2
anole.log <- anole.log %>%
  mutate(ph.res=residuals(anole.log.ph.lm))
anole.log

#plotting the residuals against covariates
p.pd <- anole.log %>%
  ggplot(aes(x=Ecomorph2,y=pd.res)) + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)
print(p.pd)

p.ph <- anole.log %>%
  ggplot(aes(x=Ecomorph2,y=ph.res)) + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)
print(p.ph)