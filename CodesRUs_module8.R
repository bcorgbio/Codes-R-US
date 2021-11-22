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
  experiment <- gsub(".csv","",met.dat[4])
  k.l[[i]] <- read_delim(i,delim = " ", col_names = c("Reading","Force","Unit"), id="Experiment") %>%
    mutate(group=group,subject=subject,angle=angle,experiment=experiment)
}

