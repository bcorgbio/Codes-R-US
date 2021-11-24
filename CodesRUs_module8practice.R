library(tidyverse)
library(MuMIn)
library(ggplot2)


ang <- seq(45,157.5,length.out = 11)

k <- list.files("./Project8Data", full.names = T)

print(k)
k.l <- list()
for(i in k){
  k.i <- read_delim(i,delim = " ", col_names = c("Reading","Force","Unit"))
  met.dat <- unlist(strsplit(i,"_"))
  group <- met.dat[1]
  subject <- met.dat[2]
  angle <- as.numeric(met.dat[3])
  experiment <- gsub(".csv","",met.dat[4])
  k.l[[i]] <- k.i %>%
    mutate(group=group,subject=subject,angle=angle,experiment=experiment)
}

print(k.l)
#generate normalized force

force.data<-do.call(rbind,k.l)

force.data$Force <- as.numeric(force.data$Force)

data.force.max.each <- force.data%>%
  group_by(experiment,angle)%>%
  dplyr::summarise(maxf=max(abs(Force), na.rm=TRUE), n=n())

data.force.max.tot <- data.force.max.each%>%
  group_by(experiment)%>%
  dplyr::summarise(maxf2=max(maxf))

data.force.max.each.joined<-data.force.max.each%>%
  left_join(data.force.max.tot)%>%
  mutate(normF=maxf/maxf2)

#find the angle with maximum force
ang <- seq(45,157.5,length.out = 11)

data.force.max.each.joined%>%
  ggplot(aes(angle,normF))+geom_point()+geom_point(aes(x=angle[which.max(normF)],y=normF[which.max(normF)]),col="red",size=4)+facet_wrap(.~experiment,ncol=5)

data.force.max.each.joined$angle[which.max(data.force.max.each.joined$normF)] #112.5


force.max.each <- force.data%>%
  group_by(experiment,angle,subject)%>%
  dplyr::summarise(maxf=max(abs(Force), na.rm=TRUE), n=n())

force.max.tot <- force.max.each%>%
  group_by(experiment,subject)%>%
  dplyr::summarise(maxf2=max(maxf))

force.max.each.joined <- force.max.each%>%
  left_join(force.max.tot)%>%
  mutate(normF=maxf/maxf2)

print(force.max.each.joined)

dfj.control <- force.max.each.joined[which(force.max.each.joined$experiment == "control"),]
dfj.fatigue <- force.max.each.joined[which(force.max.each.joined$experiment == "fatigue"),]
print(dfj.fatigue)

#polynomial model(not finished)
poly.m2.contr <- lm(normF~poly(angle,2), dfj.control) #second order
poly.m3.contr <- lm(normF~poly(angle,3), dfj.control) #third order
poly.m4.contr <- lm(normF~poly(angle,4), dfj.control) #fourth order

AICc(poly.m2.contr, poly.m3.contr, poly.m4.contr)
#Second Order is lowest = best

poly.m2.fat <- lm(normF~poly(angle,2), dfj.fatigue) #second order
poly.m3.fat <- lm(normF~poly(angle,3), dfj.fatigue) #third order
poly.m4.fat <- lm(normF~poly(angle,4), dfj.fatigue) #fourth order

AICc(poly.m2.fat, poly.m3.fat, poly.m4.fat)
#Second Order is lowest = best

x.pred <- seq(45, 157.5, length.out=1000)

#normF.pred <- predict()
normF.pred.contr <- predict(poly.m2.contr, newdata = data.frame(angle=x.pred))
normF.pred.fat <- predict(poly.m2.fat, newdata = data.frame(angle=x.pred))
