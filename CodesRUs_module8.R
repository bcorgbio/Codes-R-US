library(tidyverse)
library(MuMIn)
library(ggplot2)


ang <- seq(45,157.5,length.out = 11)

k <- list.files("./Project 8 Data", full.names = T)

print(k)
k.l <- list()
for(i in k){
  k.i <- read_delim(i,delim = " ", col_names = c("Reading","Force","Unit"))
  met.dat <- unlist(strsplit(i,"_"))
  group <- met.dat[1]
  subject <- met.dat[2]
  angle <- as.numeric(met.dat[3])
  experiment <- gsub(".csv","",met.dat[4])
  k.l[[i]] <- k.i %>%
    mutate(group=group,subject=subject,angle=angle,experiment=experiment)
}

print(k.l)

#generate normalized force

force.data<-do.call(rbind,k.l)

force.data$Force <- as.numeric(force.data$Force)

data.force.max.each <- force.data%>%
  group_by(experiment,angle)%>%
  dplyr::summarise(maxf=max(abs(Force)))

data.force.max.tot <- data.force.max.each%>%
  group_by(experiment)%>%
  dplyr::summarise(maxf2=max(maxf))

data.force.max.each.joined<-data.force.max.each%>%
  left_join(data.force.max.tot)%>%
  mutate(normF=maxf/maxf2)

#find the angle with maximum force

ang <- seq(45,157.5,length.out = 11)

data.force.max.each.joined%>%
  ggplot(aes(angle,normF))+geom_point()+geom_point(aes(x=angle[which.max(normF)],y=normF[which.max(normF)]),col="red",size=4)+facet_wrap(.~experiment,ncol=5)

data.force.max.each.joined$angle[which.max(data.force.max.each.joined$normF)] #112.5

normF <- c(2.496081e-05, 9.054897e-01, 2.259461e-01, 2.027892e-01, 7.102198e-05, 
          2.332564e-01, 9.612406e-01, 2.390605e-01, 9.764806e-01, 2.422511e-01, 
          9.844063e-01, 2.433209e-01, 9.968981e-01, 2.447969e-01, 6.578655e-05, 
          3.992673e-05, 1.000000e+00, 2.423130e-01, 2.162966e-01, 2.426354e-01, 
          9.920919e-01, 2.426354e-01, 2.360261e-01, 2.386711e-01, 6.330105e-05,
          2.386711e-01, 2.535860e-01, 2.386711e-01, 2.300790e-01, 2.349558e-01,
          2.349558e-01, 9.569234e-01, 9.627388e-01, 7.757856e-01, 2.580260e-01,
          2.118115e-01, 2.741498e-01, 8.959674e-01, 2.848843e-01, 9.279043e-01,
          2.968872e-01, 9.662953e-01, 3.013313e-01, 9.818046e-01, 3.054637e-01,
          3.348147e-05, 9.931716e-01, 3.078371e-01, 2.526909e-01, 3.072117e-01,
          1.000000e+00, 3.072117e-01, 3.014826e-01, 3.023780e-01, 7.270829e-05,
          3.023780e-01, 2.510978e-01, 3.023780e-01, 2.933047e-01, 2.993180e-01,
          2.993180e-01, 2.311344e-05, 9.761718e-01)

#polynomial model(not finished)
poly.m2 <- lm(normF~poly(angle,2)) #second order
poly.m3 <- lm(normF~poly(angle,3)) #third order
poly.m4 <- lm(normF~poly(angle,4)) #fourth order

AICc(poly.m2, poly.m3, poly.m4)

#normF.pred <- predict()
x.pred <- seq(45, 157.5, length.out=1000)

normF.pred <- predict(poly.m, newdata = data.force.max.each.joined(angle=x.pred))

ggplot(angle, normF)+geom_point(aes(x=x.pred, y=normF.pred), col="red") + geom_point(aes(x=x.pred[which.max(normF.pred)], y=normF.pred[which.max(normF.pred)]), size=5, col="blue")

x.pred[which.max(normF.pred)]

normF.fat