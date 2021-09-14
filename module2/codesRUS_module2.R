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