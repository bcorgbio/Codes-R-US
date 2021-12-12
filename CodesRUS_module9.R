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
saveRDS(data, "massbird.data.RDS")
dat <- readrDS("massbird.data.RDS")

dat%>%
  bind_rows(.id = "species")%>%
  group_by(year,species)%>%
  summarise(count=sum(individualCount,na.rm = T))%>%
  ggplot(aes(x=year,y=count,col=species))+geom_point()

print(dat)


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
