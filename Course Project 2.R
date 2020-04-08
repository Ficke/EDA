#libraries
#install.packages("ggplot2")
require(ggplot2)

#data import

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#merge detailed names
mrg <-
  merge(NEI, SCC[, c(
    "SCC",
    "Short.Name",
    "EI.Sector",
    "SCC.Level.One",
    "SCC.Level.Two",
    "SCC.Level.Three",
    "SCC.Level.Four"
  )], by = "SCC")

#question 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources
#for each of the years 1999, 2002, 2005, and 2008.

par(mfrow = c(1, 1))

#ggplot option
#ggplot(meg1, aes(x=factor(year), y=Emissions)) + stat_summary(fun.y="sum", geom="line")


#get dates out
dates <- mrg1$year
plot(dates, mrg1$Emissions)
SumYear <- with(mrg1, tapply(Emissions, year, sum, na.rm = T))
dim(SumYear)
d0 <- data.frame(Year = as.Date(names(SumYear), "%Y"), Sum = SumYear)
plot(d0$Year, d0$Sum)

#if you need to reformat the x axis:
#,xlim = as.Date(c("1998-04-07","2009-04-07")))


#Question 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#|fips == "24510" from 1999 to 2008?
#Use the base plotting system to make a plot answering this question.

mrgb<-subset(mrg, fips == 24510)


SumYearBalt <-
  with(mrgb, tapply(Emissions, year, sum, na.rm = T))
d0b <- data.frame(Year = as.Date(names(SumYearBalt), "%Y"), Sum = SumYearBalt)
plot(d0b$Year, d0b$Sum)

#question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
#variable, which of these four sources have seen decreases in emissions from 1999–2008 
#for Baltimore City? Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

#create data frame for each type

#color scale
#install.packages("wesanderson")
#install.packages("RColorBrewer")
install.packages("scles")
library(RColorBrewer)
library(wesanderson)
library(scales)
library(plyr)


myColors <- wes_palette("Rushmore1", n = 4,type ="discrete")
names(myColors) <- levels(mrgb$type)
colScale <- scale_colour_manual(name = "Type",values = myColors)

B <-
  ggplot(data = mrgb, aes(
    x = year,
    y = Emissions,
    group = type,
    color = type
  )) + 
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(
    panel.background = element_rect(fill = "gray100")
  )
B


#question 4
# Across the United States, how have emissions from coal combustion-related sources 
#changed from 1999–2008?

coal <-
  subset(
    mrg,
    EI.Sector == "Fuel Comb - Electric Generation - Coal" |
      EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal"
  )


B <-
  ggplot(data = coal, aes(
    x = year,
    y = Emissions,
  )) + 
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(
    panel.background = element_rect(fill = "gray100")
  )
B

#question 5: How have emissions from motor vehicle sources changed from 1999–2008 
#in Baltimore City?

mv <-
  subset(
    mrgb,
    EI.Sector == "Fuel Comb - Electric Generation - Coal" |
      EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal"
  )


B <-
  ggplot(data = coal, aes(
    x = year,
    y = Emissions,
  )) + 
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(
    panel.background = element_rect(fill = "gray100")
  )
B
