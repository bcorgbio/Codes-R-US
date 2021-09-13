library(tidyverse) # Remember to load your libraries!

#Remember to set your working directory, too!

#load data
pseed <- read_csv("pseed.fin.amps.csv")
pseed.b1 <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")