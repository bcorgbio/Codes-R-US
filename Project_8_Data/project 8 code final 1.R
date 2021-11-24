library(tidyverse)
library(MuMIn)
library(ggplot2)


ang <- seq(45,157.5,length.out = 11)

k <- list.files("./Project8Data", full.names = T)

k.l <- list()
for(i in k){
  met.dat <- unlist(strsplit(i,"_"))
  sub <- met.dat[2]
  ang <- as.numeric(met.dat[3])
  exp <- gsub("\\..+","",met.dat[4])
  k.l[[i]] <- read_delim(i,delim = " ", col_names = c("Reading","Force","Unit"), id="Experiment") %>%
    mutate(sub=sub,ang=ang,exp=exp)
}


#data organization and clean up

data<-do.call(rbind,k.l)%>%
  group_by(sub,exp,ang)%>%
  summarise(max.force=max(abs(Force)))%>%
  pivot_wider(names_from = exp, values_from = max.force)
print(data) 

##remove all nas

data<-na.omit(data)

##normalize force

by.con<-group_by(data,sub)%>%
summarize(fmax.ang.con=max(control))

data<-data %>%
  left_join(by.con,by="sub")

data<-mutate(data,norm.con=control/fmax.ang.con)


by.fat<-group_by(data,sub)%>%
  summarize(fmax.ang.fat=max(fatigue))

data<-data%>%
  left_join(by.fat,by="sub")

data<-mutate(data,norm.fat=fatigue/fmax.ang.fat)

##produce graph for each experiment 

ang <- seq(45,157.5,length.out = 11)
data%>%
  ggplot()+geom_point(aes(x=ang,y=norm.con))+facet_wrap(.~sub, ncol=5)
data%>%
  ggplot()+geom_point(aes(x=ang,y=norm.fat))+facet_wrap(.~sub,ncol=5)

data$ang[which.max(data$norm.con)] #Following the analysis, we find the angle with max. force is at 111 degree. So, we have to choose the closest angle-112.5 degree.Also, 112.5 degree has the second highest force value. 


##producing graphs for each experiment ignoring subject sets
data.set <- do.call(rbind, k.l)
data.set$Force <- as.numeric(data.set$Force)

data.max.each <- data.set%>%
  group_by(exp, ang)%>%
  dplyr::summarise(maxf=max(abs(Force), na.rm=TRUE), n=n())

data.max.tot <- data.max.each%>%
  group_by(exp)%>%
  dplyr::summarise(maxf2=max(maxf))

data.max.joined <- data.max.each%>%
  left_join(data.max.tot)%>%
  mutate(normF=maxf/maxf2)

data.max.joined%>%
  ggplot(aes(ang,normF))+geom_point()+geom_point(aes(x=ang[which.max(normF)], y=normF[which.max(normF)]), col="red", size=4)+facet_wrap(.~exp, ncol=5)

#preform AIC test on second,third and fourth order for control and fatigue
ang.by.con<-group_by(data,sub)%>%
  summarize(theta_max.con=ang[which.max(norm.con)])
print(ang.by.con)
ang.by.fat<-group_by(data,sub)%>%
  summarize(theta_max.fat=ang[which.max(norm.fat)])
print(ang.by.fat)


poly.con<- data%>%
  group_by(sub)%>%
  summarize(
    m2=AICc(lm(norm.con~poly(ang,2))),
    m3=AICc(lm(norm.con~poly(ang,3))),
    m4=AICc(lm(norm.con~poly(ang,4)))
  )%>%
  pivot_longer(m2:m4,names_to="model",values_to="AICc")%>%
  print()

poly.fat<-group_by(data,sub)%>%
  summarize(
    m2=AICc(lm(norm.fat~poly(ang,2))),
    m3=AICc(lm(norm.fat~poly(ang,3))),
    m4=AICc(lm(norm.fat~poly(ang,4)))
  )%>%
  pivot_longer(m2:m4,names_to="model",values_to="AICc")%>%
  print()



##prediction of 1000 discrete angles
x.pred <- seq(45,157.5,length.out = 1000)
fits.con<-data%>%
  group_by(sub)%>%
  summarize(
    m2=predict(lm(norm.con~poly(ang,2)),newdata=data.frame(ang=x.pred)),
    m3=predict(lm(norm.con~poly(ang,3)),newdata=data.frame(ang=x.pred)),
    m4=predict(lm(norm.con~poly(ang,4)),newdata=data.frame(ang=x.pred))
  )%>%
  pivot_longer(m2:m4,names_to="model")%>%
  group_by(sub,model)%>%
  summarize(theta_max=x.pred[which.max(value)])



fits.fat<-data%>%
  group_by(sub)%>%
  summarize(
    m2=predict(lm(norm.fat~poly(ang,2)),newdata=data.frame(ang=x.pred)),
    m3=predict(lm(norm.fat~poly(ang,3)),newdata=data.frame(ang=x.pred)),
    m4=predict(lm(norm.fat~poly(ang,4)),newdata=data.frame(ang=x.pred))
  )%>%
  pivot_longer(m2:m4,names_to="model")%>%
  group_by(sub,model)%>%
  summarize(theta_max=x.pred[which.max(value)])%>%
  print()


best.models.con <- fits.con%>%
  left_join(poly.con)%>%
  group_by(sub)%>%
  mutate(best=poly.con==min(poly.con))%>% 
  filter(best==TRUE)%>%
  select(-best)%>%
  print()

best.models.fat <- fits.fat%>%
  left_join(poly.fat)%>%
  group_by(sub)%>%
  mutate(best=poly.fat==min(poly.fat))%>%
  filter(best==TRUE)%>%
  select(-best)%>%
  print()










