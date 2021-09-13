#load library
library(tidyverse) 

#load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.b1 <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

# joining relational data
pseed2 <- pseed%>%
  left_join(speeds, by=c("speed" = "vol"))%>%
  print()