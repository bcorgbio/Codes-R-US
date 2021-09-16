# load library
library(tidyverse) 

# load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.b1 <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

# joining relational data
pseed2 <- pseed%>%
  left_join(speeds, by=c("speed" = "vol"))%>%
  print()

# Adding BL to results table
pseed.b1%>%
  print()
pseed2%>%
  select(fish)%>%
  unique()
## joining BL table w/ Results table
pseed2 <- pseed2%>%
  left_join(pseed.b1, by="fish")%>%
  print()
## updating bl column for correct units
pseed2 <- pseed2%>%
  mutate(bl.s = cm.s/bl)%>%
  print()

# using ggplot to plot specific fin amp vs. speed
pseed2%>%
  ggplot(aes(x=bl.s, y=amp.bl)) + geom_point()
## cleaning up plot
pseed2%>%
  ggplot(aes(x=bl.s, y=amp.bl)) + geom_point(alpha=0.01)

# plotting left pelvic fin
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame, y=amp.bl))+geom_point()

#Installing and loading package
install.packages("features")
library(features)

#Exploring features() function
exp1 <- pseed2 %>%
  filter(date=="2019-06-17-151149", fin=="L")
f1 <- features(x =exp1$frame, y=exp1$amp.bl) -> f1
fget(f1)

#plotting vertical lines corresponding to critical points
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  ggplot(aes(x=frame, y=amp.bl))+geom_point()+geom_vline(xintercept = fget(f1)$crit.pts)

#amplifying amplitude by 100
f2 <-  features(x = exp1$frame,y=exp1$amp.bl*100)
fget(f2)

#creating tibble for the peaks of the data
f.tib <- fget(f2)[2:3]%>%
  as_tibble()%>%
  filter(curvature<0)%>%
  mutate(peaks=round(crit.pts,0))%>%
  print()

#plot of amplitude data for first experiment
pseed2%>%
  filter(date=="2019-06-17-151149", fin=="L")%>%
  mutate(peak=frame %in% f.tib$peaks)%>%
  ggplot(aes(x=frame,y=amp.bl,col=peak))+geom_point()

#finding how many experiments are in the set
pseed2%>%
  summarize(n=length(unique(date)))

#function for finding the peaks in each experiment and fins
find.peaks <- function(x,y,mult=100){ #define the functions parameter/inputs:x,y, and how much we won't to multiple y by (remember the rounding issue)
  f <- fget(features(x = x,y=y*mult))[2:3]%>% #store results in `f` and compute the features for the x-y relationship, wrap in in fget to retrieve the important features, subset the results to take the 2nd and 3rd and  items, the critical points and curvature, then pass it to a tibble
    as_tibble()%>% # pass in through a filter that returns curvatures <0
    filter(curvature<0)%>% #add a column that rounds the critical point to an integer that represents the frame
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) # return the peaks from tibble
}

#filter for the first 3 experiments
pseed2%>%
  filter(date%in%unique(date)[1:3])%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  ggplot(aes(x=frame,y=amp.bl,alpha=peak,col=peak))+geom_point()+facet_grid(date~fin)

#repeating for all data but filtering to only return the peaks
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.bl))%>%
  filter(peak==T) #new filter

#plotting amplitude against speed through linear regression model
pseed.max%>%
  ggplot(aes(x=bl.s,y=amp.bl))+geom_point()+geom_smooth(method="lm")

#running a simple statistical test on the model to confirm results
amp.aov <-  aov(amp.bl~bl.s,pseed.max)
summary(amp.aov)

#computing the means for each speed and for each fish
##plotting means vs. speed and linear regression line for each fish
pseed.max %>%
  group_by(fish, bl.s) %>%
  summarize(mean.max=mean(amp.bl)) %>%
  ggplot(aes(x=bl.s,y=mean.max,col=fish))+geom_point()+geom_smooth(method="lm")

#viewing our data again
pseed2

#attempting to add column --> violates tidy principle
pseed2 <- pseed2 %>%
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl))

#attempting to delete one row --> lose half our data
pseed2 %>%
  filter(fin=="R")

#widening data set --> giving amplitude for right and left fins & summing values
#getting sum amplitude & printing results
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#finding peaks for all 
pseed.max <- pseed2%>%
  group_by(date,fin)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)

pseed.max%>%
  ggplot(aes(x=bl.s, y=amp.sum))+ geom_point() + geom_smooth(method="lm")

pseed.max %>%
  group_by(fish,bl.s) %>%
  summarize(mean.max=mean(amp.sum))%>%
  ggplot(aes(x=bl.s, y=mean.max, col=fish)) + geom_point() + geom_smooth(methos = "lm")

