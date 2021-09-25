library (ggplot2)
library(tidyverse)

#My comments begin with [CPK]

# [CPK] I can't run this code to access scales.csv file if the directory is set to a folder on your computer!!!!  What you could do is just use the default: the wd is the project directory. But, if data are stored in subdir with the default dir, you'll have to add this deeper path to the file name with "./scales1".  . . 

setwd("~/Desktop/Codes-R-Us/Codes-R-US/scales1")

# dat variable containing the scales dataset
#dat <- read.csv("scales.csv")

#[cpk] see difference VVVV

dat <- read.csv("./scales1/scales.csv")


# [CPK] from here on, some lines you don't need.  I know the dim of the data and what the first few lines look like, etc. Please stick to addressing the tasks.


dim(dat)
head(dat)

# reports the class of each column in the dataset
class(dat$N)
class(dat$quadrant)
class(dat$species)
class(dat$specimen)


# reports the dimensions of the dataset
class(dat[,1])
class(dat[,2])
class(dat[,3])
class(dat[,4])

#summary of scales and specimens samples for each species
mean(dat$N)
mean(dat$quadrant)
sapply(dat,class)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
length(species)
dat$species==species[1]
A.rup<-length(dat$species[dat$species==species[1]])
L.gib<-length(dat$species[dat$species==species[2]])
L.mac<-length(dat$species[dat$species==species[3]])
M.sal<-length(dat$species[dat$species==species[4]])
M.sax<-length(dat$species[dat$species==species[5]])
P.fla<-length(dat$species[dat$species==species[6]])
species.obs <- data.frame(sp=species,n=c(A.rup,L.gib,L.mac,M.sal,M.sax,P.fla))
species.obs
dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")
for(i in 1:10) print(i)
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

# produces a pdf containing the figures 
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")


#Grade=10/10 ("Spot on" for both rubric items)
#Well done.


#test push sep13