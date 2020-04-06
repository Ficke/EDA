#week 4 practice
#set WD
setwd("/Users/ficke/Coursera/EDA/Week4/")

#install libraries

#Read Data
pm0 <-
  read.table(
    "RD_501_88101_1999-0.txt",
    comment.char = "#",
    header = FALSE,
    sep = "|",
    na.strings = ""
  )
#check data
dim(pm0)
head(pm0)

#get field names
cnames <- readLines("RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm0) <- make.names(cnames[[1]])

#extract pm25 levels
x0 <- pm0$Sample.Value
summary(x0)
#what percent are missing
mean(is.na(x0))

#read in 2012 data
pm1 <- read.table(
  "RD_501_88101_2012-0.txt",
  comment.char = "#",
  header = FALSE,
  sep = "|",
  na.strings = ""
)

dim(pm1)

names(pm1) <- make.names(cnames[[1]])

head(pm1)

#grab sample values for pm1
x1 <- pm1$Sample.Value

#compare 1999 & 2012

boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

#investigate negative pm2.5 values
negative <- x1 < 0
dates <- pm1$Date
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates[negative], "month")

#looking at unique observation sites over time

pm1sub <- subset(pm1,State.Code ==36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0,State.Code ==36 & County.Code == 63 & Site.ID == 2008)

dates1<-pm1sub$Date
x1sub<-pm1sub$Sample.Value
dates1<- as.Date(as.character(dates1), "%Y%m%d")

plot(dates1,x1sub)

dates0<-pm0sub$Date
x0sub<-pm0sub$Sample.Value
dates0<- as.Date(as.character(dates0), "%Y%m%d")

plot(dates0,x0sub)

#plot both 1999 & 2012 on same panel 

par(mfrow = c(1,2), mar=c(4,4,2,1))

#plot  1999 values, plus median line
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm=T))

plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm=T))

#put both plots on same range

#calc range of total data 
rng<- range(x0sub,x1sub,na.rm = T)

plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm=T))

plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm=T))

#let's do this at the state level, rather than the site level 

mn0<-with(pm0, tapply(Sample.Value,State.Code, mean,na.rm=T))
mn1<-with(pm1, tapply(Sample.Value,State.Code, mean,na.rm=T))

d0<- data.frame(state=names(mn0),mean=mn0)
d1<- data.frame(state=names(mn1),mean=mn1)

#merge them together 
mrg <- merge(d0, d1, by = "state")

#reset par to be 1 plot, rather than panel plot

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2],xlim = c(1998,2013)))
with(mrg, points(rep(2012, 52), mrg[,3]))

#connet the dots 
segments(rep(1999,52), mrg[,2], rep(2012,52),mrg[,3])