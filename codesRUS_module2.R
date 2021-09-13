library(tidyverse) # Remember to load your libraries!

#Remember to set your working directory, too!

#load data
pseed <- read_csv("pseed.fin.amps.csv")
## Parsed with column specification:
## cols(
##  fish = col_character(),
##  speed = col_double(),
##  frame = col_double(),
##  date = col_character(),
##  amp = col_double(),
##  fin = col_character(),
##  amp.b1 = col_double()
## )

pseed.b1 <- read_csv("pseed.lengths.csv")
## Parsed with column specification:
## cols(
##  fish = col_character(),
##  b1 = col_double()
## )

speeds <- read_csv("pseed.calibration.csv")
## Parsed with column specification:
## cols(
##    vol = col_double(),
##    m.s = col_double(),
##    cm.s = col_double()
## )