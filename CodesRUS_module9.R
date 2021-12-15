#final project

install.packages("rgbif")
install.packages("ggmap")
install.packages("usmap")
install.packages("magick")
install.packages("cowplot")
install.packages("lme4")
install.packages("car")
install.packages("rnoaa")
install.packages("rgbif")
install.packages("rgdal")
install.packages("knitr")
install.packages("kableExtra", dependencies = TRUE)
install.packages("systemfonts")


library(tidyverse)
library(MuMIn)
library(rnoaa)
library(data.table)
library(ggmap)
library(usmap)
library(magick)
library(cowplot)
library(lme4) 
library(car) 
library(data.table) 
library(rgbif)
library(rgdal)
library(knitr)
library(kableExtra)

### choice of species 
#Chimney Swift, Chaetura pelagica
#Ruby-throated Hummingbird, Archilochus colubris
#Belted Kingfisher, Megaceryle alcyon
#Yellow-bellied Sapsucker, Sphyrapicus varius
#Common Yellowthroat,Geothlypis trichas


swift <- occ_data(scientificName = "Chaetura pelagica", stateProvince = "Massachusetts", limit = 200, year = 2018)
humming<- occ_data(scientificName = "Archilochus colubris", stateProvince = "Massachusetts", limit = 200, year = 2018)
kingfisher<- occ_data(scientificName = "Megaceryle alcyon", stateProvince = "Massachusetts", limit = 200, year = 2018)
sapsucker<- occ_data(scientificName = "Sphyrapicus varius", stateProvince = "Massachusetts", limit = 200, year = 2018)
yellowthroat<- occ_data(scientificName = "Geothlypis trichas", stateProvince = "Massachusetts", limit = 200, year = 2018)

MA <- map_data('state', 'massachusetts')

swift.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=swift[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
humming.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=humming[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
kingfisher.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=kingfisher[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
sapsucker.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=sapsucker[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
yellowthroat.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=yellowthroat[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()

swift.p2 <- ggdraw() + draw_image("swift.png", scale =0.3, halign=0, valign=1) + draw_plot(swift.p)
print(swift.p2)
humming.p2 <- ggdraw() + draw_image("humming.png", scale =0.3, halign=0, valign=1) + draw_plot(humming.p)
print(humming.p2)
kingfisher.p2 <- ggdraw() + draw_image("kingfisher.png", scale =0.3, halign=0, valign=1) + draw_plot(kingfisher.p)
print(kingfisher.p2)
sapsucker.p2 <- ggdraw() + draw_image("sapsucker.png", scale =0.3, halign=0, valign=1) + draw_plot(sapsucker.p)
print(sapsucker.p2)
yellowthroat.p2 <- ggdraw() + draw_image("yellowthroat.png", scale =0.3, halign=0, valign=1) + draw_plot(yellowthroat.p)
print(yellowthroat.p2)


species <- c("Chaetura pelagica","Archilochus colubris","Megaceryle alcyon","Sphyrapicus varius","Geothlypis trichas")

y <- paste0("2000",",","2019")
m <- paste0("4",",","5")


dat.l <-list()
for(s in species){
  n.obs <-  occ_data(scientificName = s,year=y,month=m,limit=0,country="US",basisOfRecord = "HUMAN_OBSERVATION",stateProvince="Massachusetts")$meta$count 
  
  print(n.obs)
  
  
  dat.l[[paste0(s)]] <- occ_data(scientificName = s,year=y,month=m,
                                 limit=n.obs,country="US",
                                 basisOfRecord = "HUMAN_OBSERVATION",
                                 stateProvince="Massachusetts")[[2]]
  
  
}

dat <- rbindlist(dat.l,fill=T)

head(dat)

saveRDS(data,"massbird.data.RDS")

library(tidyverse)
dat <- readRDS("massbird.data.RDS")

dat.l%>%
  bind_rows(.id = "species") %>% 
  group_by(year,species)%>%
  summarise(count=sum(individualCount,na.rm = T))%>%
  ggplot(aes(x=year,y=count,col=species))+geom_point()

#It looks like there’s not much data before 2005 for all species

###Querying NOAA's NCDC API

options(noaakey = "VYpNYgvfxWWfUIWmcjVZxUUJwebzTmLB")

sts <- c(
  "GHCND:USW00013894", #Mobible, AL 2k away about 10 days away @200 km/day
  "GHCND:USW00013881", #Charlotte, NC 1000 km away about 6 days away @200 km/day
  "GHCND:USW00014739" #Boston
)

bos <- ncdc_stations(stationid = "GHCND:USW00014739")
print(bos)

cha <- ncdc_stations(stationid = "GHCND:USW00013881")
print(cha)

mob <- ncdc_stations(stationid = "GHCND:USW00013894")
print(mob)


sta.d <- bind_rows( #bind the rows
  lapply(sts,function(x) ncdc_stations(stationid = x)$data ) 
)%>%
  left_join(usmap_transform(.[,c("longitude","latitude")]))%>% 
  mutate(name=str_sub(name, -5,-4))%>%
  mutate(migr.day=c(10,5,0))%>% 
  separate(id,into = c("station.type","id"))%>%
  print()

plot_usmap(
  include = c(.northeast_region,.south_region,.east_north_central)
)+geom_point(data=sta.d,aes(x=longitude.1,y=latitude.1,col=name),size=5)+geom_label(data=sta.d,aes(x=longitude.1,y=latitude.1,col=name,label=name),size=5,nudge_x = 1e6*0.25)+theme(legend.position = "none")

weather.d <- meteo_pull_monitors(sta.d$id,date_min = "2005-01-01")#since 2005 we see an uptick in eBird data

head(weather.d)

#Preparing eBird Data

#Chimney Swift, Chaetura pelagica(refered by cs)

cs<- dat.l%>%
  bind_rows(.id = "species") %>% 
  filter(species=="Chaetura pelagica")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>2004)

cs%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

cs.pred <- cs%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))), 
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cs%>%select(j.day,date)) 

cs%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=rh.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

cs.arrive.date <-cs.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

cs.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Ruby-throated Hummingbird, Archilochus colubris(refered by rh)

rh<- dat.l%>%
  bind_rows(.id = "species") %>% 
  filter(species=="Archilochus colubris")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>2004)

rh%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

rh.pred <- rh%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))), 
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(rh%>%select(j.day,date)) 

rh%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=rh.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

rh.arrive.date <-rh.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

rh.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Belted Kingfisher, Megaceryle alcyon(refered by bk)

bk<- dat.l%>%
  bind_rows(.id = "species") %>% 
  filter(species=="Megaceryle alcyon")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>2004)

bk%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

bk.pred <- bk%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))), 
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(bk%>%select(j.day,date)) 

bk%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=rh.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

bk.arrive.date <-bk.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

bk.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Yellow-bellied Sapsucker, Sphyrapicus varius(refered by ys)

ys<- dat.l%>%
  bind_rows(.id = "species") %>% 
  filter(species=="Sphyrapicus varius")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>2004)

ys%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

ys.pred <- ys%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))), 
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(ys%>%select(j.day,date)) 

ys%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=rh.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

ys.arrive.date <-ys.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

ys.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Common Yellowthroat,Geothlypis trichas(referred by cn)
cn<- dat.l%>%
  bind_rows(.id = "species") %>% 
  filter(species=="Geothlypis trichas")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>2004)

cn%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)


cn.pred <- cn%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cn%>%select(j.day,date))

cn%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=cn.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

cn.arrive.date <-cn.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

cn.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()


##Preparing weather Data

weather.d <- weather.d%>%
  dplyr::mutate(year=as.integer(str_sub(date,1,4)), 
                date=as.Date(date))%>%
  group_by(year)%>% 
  dplyr::mutate(j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01"))), 
                date2=date,
                wdir.rad=(180-abs(wdf2-180))*pi/180, 
                wvec=cos(wdir.rad)*-1*awnd 
  )%>% 
  select(id,year,date2,j.day,tmin,tmax,wvec)%>% 
  left_join(sta.d%>%select(id,name,migr.day))%>% 
  mutate(j.day=j.day+migr.day)

#mean arrival time for five species
cs.arr.weath <- cs.arrive.date%>%
  left_join(weather.d)%>%
  left_join(cs%>%select(year,date,j.day))

head(cs.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

cs.arr.weath2 <- cs.arrive.date%>%
  left_join(weather.wk)

head(cs.arr.weath2)



rh.arr.weath <- rh.arrive.date%>%
  left_join(weather.d)%>%
  left_join(rh%>%select(year,date,j.day))

head(rh.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

rh.arr.weath2 <- rh.arrive.date%>%
  left_join(weather.wk)

head(rh.arr.weath2)



bk.arr.weath <- bk.arrive.date%>%
  left_join(weather.d)%>%
  left_join(bk%>%select(year,date,j.day))

head(bk.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

bk.arr.weath2 <- bk.arrive.date%>%
  left_join(weather.wk)

head(bk.arr.weath2)



ys.arr.weath <- ys.arrive.date%>%
  left_join(weather.d)%>%
  left_join(ys%>%select(year,date,j.day))

head(ys.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

ys.arr.weath2 <- ys.arrive.date%>%
  left_join(weather.wk)

head(ys.arr.weath2)


cn.arr.weath <- cn.arrive.date%>%
  left_join(weather.d)%>%
  left_join(cs%>%select(year,date,j.day))

head(cn.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

cn.arr.weath2 <- cn.arrive.date%>%
  left_join(weather.wk)

head(cn.arr.weath2)


##Linear Mixed-effect Modeling for five species

#cs
cs.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),cs.arr.weath,na.action = "na.fail")
Anova(cs.lmer)
cs.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),cs.arr.weath2,na.action = "na.fail")
Anova(cs.lmer2)


cs.arr.aic <- dredge(cs.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

cs.kb <- kable(cs.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")
kable_styling(cs.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),cs.arr.weath2,na.action = "na.fail")

Anova(best.lmer)

#rh
rh.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),rh.arr.weath,na.action = "na.fail")
Anova(rh.lmer)
rh.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),rh.arr.weath2,na.action = "na.fail")
Anova(rh.lmer2)


rh.arr.aic <- dredge(rh.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

rh.kb <- kable(rh.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")
kable_styling(rh.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),rh.arr.weath2,na.action = "na.fail")

Anova(best.lmer)

#bk

bk.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),bk.arr.weath,na.action = "na.fail")
Anova(bk.lmer)
bk.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),bk.arr.weath2,na.action = "na.fail")
Anova(bk.lmer2)


bk.arr.aic <- dredge(bk.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

bk.kb <- kable(bk.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")
kable_styling(bk.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),bk.arr.weath2,na.action = "na.fail")

Anova(best.lmer)

#ys
ys.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),ys.arr.weath,na.action = "na.fail")
Anova(ys.lmer)
ys.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),ys.arr.weath2,na.action = "na.fail")
Anova(ys.lmer2)


ys.arr.aic <- dredge(ys.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

ys.kb <- kable(ys.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")
kable_styling(ys.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),bk.arr.weath2,na.action = "na.fail")

Anova(best.lmer)

#cn

cn.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),cn.arr.weath,na.action = "na.fail")
Anova(cn.lmer)
cn.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),cn.arr.weath2,na.action = "na.fail")
Anova(cn.lmer2) 

cn.arr.aic <- dredge(cn.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

cn.kb <- kable(cn.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")
kable_styling(cn.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),bk.arr.weath2,na.action = "na.fail")

Anova(best.lmer)
