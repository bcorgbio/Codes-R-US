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
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1), data=anole2)
summary(anole.allo)
