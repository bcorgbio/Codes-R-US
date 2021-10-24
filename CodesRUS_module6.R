library(tidyverse)

f <- list.files("./proj6data",pattern=".csv",full.names=T)

f.l <- list()
for(i in f){
  f.i <- read_csv(i)
  if(ncol(f.i)==1) coln <- 1
  if(ncol(f.i)==2) coln <- 2
  m <- strsplit(i,"_")%>%unlist
  print(m)
  sub <- m[2]%>%tolower()
  time <- m[3]%>%tolower()
  mass <- as.numeric(gsub(".csv","",m[4]))
  f.i <- f.i[,coln]#take only one column
  colnames(f.i) <- "Temp"
  f.l[[i]] <- f.i%>%
    mutate(N=1:n(),Temp=as.numeric(Temp),subject=sub,Time=time,mass=mass)%>%
    print()
}

dat <- do.call(rbind,f.l)%>%
  print()

dat2 <- dat%>%
  group_by(subject,Time,mass)%>%
  na.omit()%>%
  filter(N>0.95*max(N))%>%
  summarize(m.temp=mean(Temp), n=n())%>%
  pivot_wider(names_from=Time, values_from = c(m.temp,n))%>%
  mutate(td=abs(m.temp_day-m.temp_night))%>%
  print()

dat2%>%
  ggplot(aes(log(mass),log(td)))+geom_point()+geom_smooth(method="lm")


#Inter-specific Scaling
mean.mass = colMeans(dat2[2], na.rm=TRUE)%>%
  print()
mean.h.temp= colMeans(dat2[3], na.rm=TRUE)%>%
  print()
mean.l.temp = colMeans(dat2[4], na.rm=TRUE)%>%
  print()

Human.Data <- data.frame("Primata","Homo sapiens", mean.mass, mean.h.temp, mean.l.temp)%>%
  print()
names(Human.Data) <- c("Order", "species","mass.g", "T.high", "T.low")%>%
  print()
print(Human.Data)

mammals <- read.csv("mammal.temp.csv")%>%
  print()
mammals.df <- rbind(mammals, Human.Data)
print(mammals.df)

temp.change <- mammals.df%>%
  mutate(td=abs(T.high-T.low))

mammals.dat <- temp.change%>%

mammals.dat %>%
  ggplot(aes(log(mass.g),log(td)))+geom_point()+geom_smooth(method="lm")
