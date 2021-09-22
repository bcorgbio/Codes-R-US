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
#constructing model to incude new variable
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2, anole.log)
summary(anole.log.eco.lm)

#performing ANOVA test on model (two-way analysis of covariance)
## assessing the effect of a categorical variable (Ecomorph2) in context of how HTotal covaries with SVL
# results = reject the null hyp that ecomorph groups do not have separate hindlimb-SVL relationships
## 
anova(anole.log.eco.lm)
