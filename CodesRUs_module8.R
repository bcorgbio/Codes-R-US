library(tidyverse)
library(MuMIn)
library(ggplot2)


ang <- seq(45,157.5,length.out = 11)

k <- list.files(pattern = "control|fatigue")

print(k)
k.l <- list()
for(i in k){
  met.dat <- unlist(strsplit(i,"_"))
  group <- met.dat[1]
  subject <- met.dat[2]
  angle <- as.numeric(met.dat[3])
  experiment <- gsub("\\..+","",met.dat[4])
  k.l[[i]] <- read_delim(i,delim = " ", col_names = c("Reading","Force","Unit"), id="Experiment") %>%
    mutate(group=group,subject=subject,angle=angle,experiment=experiment)
}


#generate normalized force

force.data<-do.call(rbind,k.l)

force.data$Force <- as.numeric(force.data$Force)

data.force.max.each <- force.data%>%
  group_by(experiment,angle)%>%
  dplyr::summarise(maxf=max(abs(Force)))

data.force.max.tot <- data.force.max.each%>%
  group_by(experiment)%>%
  dplyr::summarise(maxf2=max(maxf))

data.force.max.each.joined<-data.force.max.each%>%
  left_join(data.force.max.tot)%>%
  mutate(normF=maxf/maxf2)

#find the angle with maximum force
library(ggplot2)
ang <- seq(45,157.5,length.out = 11)
data.force.max.each.joined%>%
  ggplot(aes(angle,normF))+geom_point()+geom_point(aes(x=angle[which.max(normF)],y=normF[which.max(normF)]),col="red",size=4)+facet_wrap(.~experiment,ncol=5)

data.force.max.each.joined$angle[which.max(data.force.max.each.joined$normF)] #112.5

#polynomial model(not finished)
poly.m2 <- lm(normF~poly(angle,2)) #second order
poly.m3 <- lm(normF~poly(angle,3)) #third order
poly.m4 <- lm(normF~poly(angle,4)) #fourth order

AICc(poly.m2, poly.m3, poly.m4)

x.pred <- seq(45, 157.5, length.out=1000)

#normF.pred <- predict()