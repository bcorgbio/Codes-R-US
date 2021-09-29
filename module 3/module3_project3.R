#   SET UP

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
## filtered the data to remove any rows when Ecomorph value was U or CH
### these values relate to species w very few observations
## filtered for with filter then used "!" to keep those not in filter
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>% ##omitted any rows of tibble that had missing values
  print()

#mutated collective tibble containing morphological and ecological data
## changed columns to log transformations of their values
## mutate columns identified with c() by the second argument given log
anole.log <- anole2 %>%
  mutate_at(c("SVL","HTotal","PH","ArbPD"), log)


#   VISUALIZING DATA, FORMING & REFINING HYPOTHESES

#plotting hind limb length (y-value) vs. size in anoles (x-values)
## added 2 geometries: points & smooth line built on linear model
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")

#establishing simple linear model using lm() function
## ~ = variable to the left is predicted by one or more variables to the right
## built linear model with HTotal predicted by SVL
## second parameter indicates what data the variables are coming from
anole.lm <- lm(HTotal~SVL, anole2)

#gives values of intercept and slope
coef(anole.lm)

#regraphing using the intercept and slope found with the linear model
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2], intercept=coef(anole.lm)[1], col="blue")

#predicting HTotal for a range of SVL rather than using geom_smooth to do it
## create tibble with prediction (pred.lm)
SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <- tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm, newdata = data.frame(SVL=SVL2))
)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm, aes(SVL,H.pred), col="blue")
#retrieves important information about the model
summary(anole.lm)

#nls() = non-linear least squares
## least squares = approach in regression modeling which approximates the relationship by minimizing the sum of the squares of the residuals
### how the model in our basic lm model is fit
## nls = the model is fit based on a non-linear relationship 
### an equation with the paramters of alpha and beta

#fitting allometric model with nls
## need to specify starting values for parameters in our model w list 
## nls conducts search of model parameter values to find those that reduce the sum of squares of the residuals
## starting points are estimated on scaling coefficent (beta; approximately 1)
## & intercept (alpha; close to but not 0)
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1), data=anole2)
# calling summary 
## see that the parameters a & b are significant = both are likely explanatory parameters
## doesn't produce an r squared value
summary(anole.allo)

#how well does this data fit the model ? is it better than the lm() model?
##    p-values = if low reject null, if high dont reject null
## we've assessed the null hyp that SVL has no effect on HTotal & rejected it through linear and allometric tests
### obvious: limbs get longer as individual lizards grow longer

# rest of the question: does hind-limb length vary linearly or allometrically
## could perform likelihood ratio tests
### compare the likelihood of two nested models
#### likelihood = probability, given a model and set of parameter values, of obtaining a particular set of data
### right approach for us - find the model, linear or allometric, with a higher likelihood of describing our morphological data better therefore is better approximation of hindlimb-size relationship
### can only be used in limited cases - when comparing two models when one is a special case of the other
#### one model is the dame as the other except is more complex (one or more parameters = means other independent variables)
## we don't have nested models but different model families: one linear, one allometric

##alternative = information theory approach
### use the AIC = compares the lieklihood of model against number of parameters estimated
### can be described as favoring the model that provides the best fit to the data with as few parameters as possible
### sensitive to sample size; use AICc which penalizes models that have small sample sizes relative to number of parameters

#Computing AICc of both models then compare their fit
#Calculating the relative support of each model using AIC weights
#AICc from MuMIn package
anole.aic <- AICc(anole.lm, anole.allo)

#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

print(anole.aicw)

#Can see that anole.allo (2) has a lower AIC score 
## lower score indicates better fit



#MORE COMPLEX VISUALIZATIONS & MODELS

#consider the effect of ecomorph on the hindlimb-SVL relationships
#use log-transformed data 

#visualizing hindlimb-SVL relationships for each ecomorph in ggplot
##passed log-transformed data to ggplot, specifying SVL for x-values and HTotal for y, then added point and line
## added another argument which colors all the added geometries according to the column values Ecomorph2
anole.log%>%
  ggplot(aes(HTotal, SVL, col=Ecomorph2))+geom_point()+geom_smooth(method="lm")

#need to consider ecomorph as important variable explaining the hindlimb-SVL relationship
#constructing model to include new variable
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2, anole.log)
summary(anole.log.eco.lm)

#performing ANOVA test on model (two-way analysis of covariance)
## assessing the effect of a categorical variable (Ecomorph2) in context of how HTotal covaries with SVL
# results = reject the null hyp that ecomorph groups do not have separate hindlimb-SVL relationships
## established ecomorph has significant effect on hindlimb-SVL relationship
anova(anole.log.eco.lm)
#really want to know if adding Ecomorph2 parameter results in a better fit
#establishing model with log-transformed data and compare with AIC and AICw
anole.log.lm <- lm(HTotal~SVL, anole.log)
anova(anole.log.lm)
anole.log.aic <- AICc(anole.log.lm, anole.log.eco.lm)
aicw(anole.log.aic$AICc)

#Residuals = how much any one data point varies from the prediction
#given global model that predicts hindlimb length based on SVL --> can determine how much each species deviates from prediction
#take residual for each species and look for patterns by plotting them versus ecomorph

#computing the residuals based on global anole.log.lm model
#used mutate to establish a new res column that contains the residuals
#redefined anole.log as a tibble containing this new column
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

#plotting the residuals against Ecomorph2
anole.log %>%
  ggplot(aes(Ecomorph2,res)) + geom_point()
### can see that the deviations are greatest in the twig and trunk-ground ecomorphs

#better summarization of the data --> including median residual for each ecomorph
#use ggplot do it through geom_boxplot()
p.eco <- anole.log %>%
  ggplot(aes(x=Ecomorph2, y=res)) + geom_boxplot()
print(p.eco)
### summary of hindlimb-SVL residual data according to ecomorph

#desire to visualize the mean of values in groups rather than median
p.eco + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)
### added point with stat_summary geom
### applies a summarizing function to already established group & plots the value of the summary with specifies geom
### applied mean function and point with size 3



#NOT SO FAST: PHYLOGENY MATTERS

#Ecomorph is an important explanatory variable in highlimb-SVL relationship
#Need to account for evolutionary history
## some species are more closely related --> cannot be seen as independent samples
## have to observe the covariance with phylogenetic position

#need to use phylogenetic comparative methods (PCMs)
#to perform phylogenetic generalized least squares (PGLS) --> establish covariation matrix with tree & assume the characters evolved under evolutionary model
anole.tree <- read.tree("anole.tre")
plot(anole.tree, cex=0.4)
###cex stands for character expansion factor - how size of the text should be adjusted

# have tree to use our PGLS regression analysis
# need to see what we mean by an evolutionary model
## under what process or mode does a trait evolve over a tree

#performing PGLS under two different models: both with simple regression models that doesn't include ecomorph and models that do
#PGLS under BM, no ecomorph
pgls.BM1 <- gls(HTotal ~SVL, correlation=corBrownian(1,phy=anole.tree, form=~Species), data=anole.log, method="ML")

#PGLS under BM, w ecomorph
pgls.BM2 <- gls(HTotal ~SVL *Ecomorph2, correlation=corBrownian(1, phy=anole.tree, form=~Species), data=anole.log, method="ML")

#PGLS under OU, no ecomorph
pgls.OU1 <- gls(HTotal ~SVL, correlation=corMartins(0, phy=anole.tree, form=~Species), data=anole.log, method="ML")

#PGLS under OU, w ecomorph
pgls.OU2 <- gls(HTotal~SVL *Ecomorph2, correlation=corMartins(0, phy=anole.tree, form=~Species), data=anole.log, method="ML")

### set up generalized least squares models that establish a correlation matrix based on BM and OU models of evolution
## corBrownian() from ape package for BM models & corMartins from ape for OU models
## parameters estimated under maximum likelihood with ML
## have to specify the tree and the column names that will form the covariation matrix species

#perform AIC operations to see which models fit that data best
anole.phylo.aic <- AICc(pgls.BM1, pgls.BM2, pgls.OU1, pgls.OU2)
aicw(anole.phylo.aic$AICc)
### can say that phylogenetically corrected regression model that includes Ecomorph2 with traits evolving under BM is the best fit
## lowest values = best fit
### OU models specify a pull to some global optimum --> rejected

#The traits have evolved randomly, but randomly within each lineage

#consider whether Ecomorph is significant factor in predicting the hindlimb-SVL relationship in phylogenetic context by performing an ANOVA on best fitting model
anova(pgls.BM2)
### ecomorph2 has significant effect on relationship with p<0.0001
### isn't as strong as a factor when considering phylogeny

#coming back to residuals of hindlimb-SVL relationship considering BM evolution
## mutating and redefining anole.log data to include a column for phylogenetically corrected residuals
## plotting them against ecomorph 
anole.log <- anole.log %>%
  mutate(phylo.res=residuals(pgls.BM2))
p.eco.phylo <- anole.log %>%
  ggplot(aes(x=Ecomorph2, y=phylo.res))+geom_boxplot()+stat_summary(fun=mean, geom="point", size=3)
print(p.eco.phylo)



#PLOTTING MORE WITH LESS

#comparing phylogenetically corrected residuals with the uncorrected residuals
## make anole.log tibble longer with respect to the two types of residuals along with another identifying which type of residual the value belongs to
anole.log %>%
  dplyr::select(Ecomorph2, res, phylo.res) %>%
  pivot_longer(cols=c("res", "phylo.res")) %>%
  print %>%
  ggplot(aes(x=Ecomorph2, y=value)) +geom_boxplot() + stat_summary(fun=mean, geom="point", size=3) + facet_grid(name~., scales="free_y") + ylab("residual")
### selected 3 columns of interest and the phylogenetically corrected residual in anole.log and passed that to a pivoting function
### specified the columns that will make tibble longer
### constructed two new columns: one for values other for names of column from which it came
## ecomorph2 column remains & there's a single column for the residual values and another identifying the values by name
#For each entry of an individual ecomorph in the tibble, there's two types of residuals identified by name, along with their values in value
## plot a boxplot of both residuals for each ecomorph using boxplot code from above and modifying to produce the facets
### rather than having the y values equal res or phylo.res, have it equal value in our tibble which reflects the values for both types of residuals in our new tibble
## added facet_grid to plot with name = get each type of residual plotted as boxplot against ecomorph in a separate row
## added scales=free_y which allows the scales on the y axis to be different in each row


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
