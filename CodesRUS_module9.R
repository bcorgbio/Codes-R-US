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

library(rgbif)
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

### Choice of species 
#Chimney Swift, Chaetura pelagica
#humming-throated Hummingbird, Archilochus colubris
#Belted Kingfisher, Megaceryle alcyon
#Yellow-bellied Sapsucker, Sphyrapicus varius
#Common Nighthawk, Chordeiles minor

swift <- occ_data(scientificName = "Chaetura pelagica", stateProvince = "Massachusetts", limit = 200, year = 2018)
humming<- occ_data(scientificName = "Archilochus colubris", stateProvince = "Massachusetts", limit = 200, year = 2018)
kingfisher<- occ_data(scientificName = "Megaceryle alcyon", stateProvince = "Massachusetts", limit = 200, year = 2018)
sapsucker<- occ_data(scientificName = "Sphyrapicus varius", stateProvince = "Massachusetts", limit = 200, year = 2018)
nighthawk<- occ_data(scientificName = "Chordeiles minor", stateProvince = "Massachusetts", limit = 200, year = 2018)

MA <- map_data('state', 'massachusetts')

swift.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=swift[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
humming.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=humming[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
kingfisher.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=kingfisher[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
sapsucker.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=sapsucker[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()
nighthawk.p <- ggplot(MA, aes(long,lat,group=subregion) )+
  geom_polygon(colour = "gray",fill="gray90")+geom_point(data=nighthawk[[2]],aes(x=decimalLongitude,y=decimalLatitude,size=individualCount),alpha=0.3,inherit.aes = F)+ coord_quickmap()+theme_void()

swift.p2 <- ggdraw() + draw_image("swift.png", scale =0.3, halign=0, valign=1) + draw_plot(swift.p)
print(swift.p2)
humming.p2 <- ggdraw() + draw_image("humming.png", scale =0.3, halign=0, valign=1) + draw_plot(humming.p)
print(humming.p2)
kingfisher.p2 <- ggdraw() + draw_image("kingfisher.png", scale =0.3, halign=0, valign=1) + draw_plot(kingfisher.p)
print(kingfisher.p2)
sapsucker.p2 <- ggdraw() + draw_image("sapsucker.png", scale =0.3, halign=0, valign=1) + draw_plot(sapsucker.p)
print(sapsucker.p2)
nighthawk.p2 <- ggdraw() + draw_image("nighthawk.png", scale =0.3, halign=0, valign=1) + draw_plot(nighthawk.p)
print(nighthawk.p2)

species <- c("Chaetura pelagica","Archilochus colubris","Megaceryle alcyon","Sphyrapicus varius","Chordeiles minor")
y <- paste0("1990",",","2019")
m <- paste0("3",",","6")

dat.l <-list()

for(s in species){
  
  ## setting the limit=0 returns no records but can give you the number of observations if you access the meta data
  n.obs <-  occ_data(scientificName = s,year=y,month=m,limit=0,country="US",basisOfRecord = "HUMAN_OBSERVATION",stateProvince="Massachusetts")$meta$count 
  print(n.obs)
  
  dat.l[[paste0(s)]] <- occ_data(scientificName = s,year=y,month=m,
                                 limit=n.obs,country="US",
                                 basisOfRecord = "HUMAN_OBSERVATION",
                                 stateProvince="Massachusetts")[[2]]
}

dat <- rbindlist(dat.l,fill=T)

head(dat)
saveRDS(dat, "massbird.data.RDS")
library(tidyverse)
dat <- readRDS("massbird.data.RDS")

dat.l%>%
  bind_rows(.id = "species")%>%
  group_by(year,species)%>%
  summarise(count=sum(individualCount,na.rm = T))%>%
  ggplot(aes(x=year,y=count,col=species))+geom_point()


###Querying NOAA's NCDC API
options(noaakey = "token")
sts <- c(
  "GHCND:USW00013894", #Mobible, AL 2k away about 10 days away @200 km/day
  "GHCND:USW00013881", #Charlotte, NC 1000 km away about 6 days away @200 km/day
  "GHCND:USW00014739" #Boston
)

bos <- ncdc_stations(stationid = "GHCND:USW00014739")
print(bos)

sta.d <- bind_rows( lapply(sts,function(x) ncdc_stations(stationid = x)$data )%>%
  left_join(usmap_transform(.[,c("longitude","latitude")]))%>%
  mutate(name=str_sub(name, -5,-4))%>%
  mutate(migr.day=c(10,5,0))%>% 
  separate(id,into = c("station.type","id"))%>%
  print())

plot_usmap(
  include = c(.northeast_region,.south_region,.east_north_central)+
  geom_point(data=sta.d, aes(x=longitude.1, y=latitude.1, col=name),size=5)+
  geom_label(data=st.d, aes(x=longitude.1, y=latitude.1, col=name, label=name), size=5, nudge_x=1e6*0.25)+
  theme(legend.position="none")
)

weather.d <- meteo_pull_monitors(sta.d$id, data_min = "2000-01-01")
head(weather.d)


###Data Analysis

##Chimney Swift
cp <- dat%>%
  filter(species=="Chaetura pelagica")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

cp%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

cp.pred <- cp%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cp%>%select(j.day,date)) ## add date back to tibble

cp%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=cp.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

cp.arrive.date <-cp.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

cp.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()


##Ruby-throated Hummingbird
rh <- dat%>%
  filter(species=="Archilochus colubris")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

rh%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

rh.pred <- rh%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cp%>%select(j.day,date)) ## add date back to tibble

rh%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=rh.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

rh.arrive.date <-rh.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

rh.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Belted Kingfisher
bk <- dat%>%
  filter(species=="Megaceryle alcyon")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

bk%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

bk.pred <- bk%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cp%>%select(j.day,date)) ## add date back to tibble

bk%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=bk.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

bk.arrive.date <-bk.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

bk.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Yellow-bellied Sapsucker
ys <- dat%>%
  filter(species=="Sphyrapicus varius")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

ys%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

ys.pred <- ys%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cp%>%select(j.day,date)) ## add date back to tibble

ys%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=ys.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

ys.arrive.date <-ys.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

ys.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()

#Common Nighthawk
ch <- dat%>%
  filter(species=="Chordeiles minor")%>%
  group_by(year)%>%
  mutate(date=as.Date(paste0(year,"-",month,"-",day)),
         j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01")))
  )%>%
  group_by(species,year,j.day,date)%>%
  summarise(day.tot=sum(individualCount,na.rm=T))%>%
  group_by(species,year)%>%
  mutate(prop=cumsum(day.tot/sum(day.tot,na.rm = T)))%>%
  filter(year>1999)

ch%>%
  ggplot(aes(j.day,prop))+geom_point()+facet_wrap(year~.)

ch.pred <- ch%>%
  group_by(year)%>%
  summarize(
    pred=predict(nls(prop~SSlogis(j.day,Asym, xmid, scal)),newdata=data.frame(j.day=min(j.day):max(j.day))),#predict the logistic curve for each species
    j.day=min(j.day):max(j.day),
  )%>%
  left_join(cp%>%select(j.day,date)) ## add date back to tibble

ch%>%
  ggplot(aes(j.day,prop))+geom_point(aes=0.3)+geom_line(data=ch.pred,aes(x=j.day,y=pred),col="blue",size=2)+facet_wrap(year~.)

ch.arrive.date <-ch.pred%>%
  group_by(year)%>%
  filter(j.day==j.day[which.min(abs(pred-0.25))])

ch.arrive.date%>%
  ggplot(aes(year,j.day))+geom_point()




 
###Preparing weather Data
weather.d <- weather.d%>%
  dplyr::mutate(year=as.integer(str_sub(date,1,4)), #add year
                date=as.Date(date))%>%
  group_by(year)%>% #group by year so we can compute julian day
  dplyr::mutate(j.day=julian(date,origin=as.Date(paste0(unique(year),"-01-01"))), #add julian day
                date2=date,
                wdir.rad=(180-abs(wdf2-180))*pi/180, #radians so we can use a trig function to compute wind vector, scale degrees first to 180 scale to 2x pi and subtract from 180 (wind comes out of a direction)
                wvec=cos(wdir.rad)*-1*awnd # we want a negative value for positive value for 2x pi
  )%>% #store day in new column
  select(id,year,date2,j.day,tmin,tmax,wvec)%>% #select the rows we need
  left_join(sta.d%>%select(id,name,migr.day))%>% #add the station id info (ie. name)
  mutate(j.day=j.day+migr.day)#make j.day ahead of BOS according to the migration days away so we can join weather along path

mc.arr.weath <- mc.arrive.date%>%
  left_join(weather.d)%>%
  left_join(mc%>%select(year,date,j.day))

head(mc.arr.weath)

weather.wk <-weather.d %>% 
  group_by(year,name) %>% 
  mutate(wk.tmin = frollmean(tmin, n=14,align="right"),
         wk.tmax = frollmean(tmax, n=14,align="right"),
         wk.wvec = frollmean(wvec, n=14,align="right")
  )%>%
  select(j.day,date2,name,wk.tmin,wk.tmax,wk.wvec)

mc.arr.weath2 <- mc.arrive.date%>%
  left_join(weather.wk)

head(mc.arr.weath2)

###Linear Mixed-effect Modeling
#weather at 0, 5, and 10 days away from arrival
mc.lmer <- lmer(j.day~tmin*tmax*wvec+(1|name),mc.arr.weath,na.action = "na.fail")

Anova(mc.lmer) #Anova from the car package

#0Mean two week weather preceding arrival
mc.lmer2 <- lmer(j.day~wk.tmin*wk.tmax*wk.wvec+(1|name),mc.arr.weath2,na.action = "na.fail")

Anova(mc.lmer2)

mc.arr.aic <- dredge(mc.lmer2,fixed = c("wk.tmin","wk.tmax","wk.wvec"),)

mc.kb <- kable(mc.arr.aic[1:4,],caption = "Fit values for nested models of the most complicated lme model")

kable_styling(mc.kb)

best.lmer <-  lmer(j.day~wk.tmin+wk.tmax+wk.wvec+(1|name),mc.arr.weath2,na.action = "na.fail")

Anova(best.lmer)