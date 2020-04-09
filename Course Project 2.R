#libraries

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wesanderson)
library(scales)

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

SumYear <- with(mrg, tapply(Emissions, year, sum, na.rm = T))
summary(SumYear)
d0 <- data.frame(Year = as.integer(names(SumYear)), Sum = SumYear)

png("Plot 1.png")
plot(d0$Year, d0$Sum, main = "CW Total Emissions")
dev.off()

#Question 2:
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#|fips == "24510" from 1999 to 2008?
#Use the base plotting system to make a plot answering this question.

mrgb <- subset(mrg, fips == 24510)

#summarize data by year, for baltimore
SumYearBalt <-
  with(mrgb, tapply(Emissions, year, sum, na.rm = T))
d0b <-
  data.frame(Year = as.Date(names(SumYearBalt), "%Y"), Sum = SumYearBalt)

png("Plot 2.png")
plot(d0b$Year, d0b$Sum, main = "Emissions in Baltimore City")
dev.off()


#question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
#variable, which of these four sources have seen decreases in emissions from 1999–2008
#for Baltimore City? Which have seen increases in emissions from 1999–2008?
#Use the ggplot2 plotting system to make a plot answer this question.

myColors <- wes_palette("Rushmore1", n = 4, type = "discrete")
names(myColors) <- levels(mrgb$type)
colScale <- scale_colour_manual(name = "Type", values = myColors)
mrgb <- subset(mrg, fips == 24510)

B <-
  ggplot(data = mrgb, aes(
    x = year,
    y = Emissions,
    group = type,
    color = type
  )) +
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(panel.background = element_rect(fill = "gray100")) +
  ggtitle("Longitudinal Emissions by Source Type")

png("Plot 3.png", height = 500, width = 500)
B
dev.off()


#question 4
# Across the United States, how have emissions from coal combustion-related sources
#changed from 1999–2008?

coal <-
  subset(
    mrg,
    EI.Sector == "Fuel Comb - Electric Generation - Coal" |
      EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal"
  )


Co <-
  ggplot(data = coal, aes(x = year,
                          y = Emissions, )) +
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(panel.background = element_rect(fill = "gray100")) +
  ggtitle("Longitudinal Change in Coal Emissions")

png("Plot 4.png", height = 500, width = 500)
Co
dev.off()

#question 5: How have emissions from motor vehicle sources changed from 1999–2008
#in Baltimore City?
mrgb <- subset(mrg, fips == 24510)
mv <-
  subset(
    mrgb,
    EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
      EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
      EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
      EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
  )

MoV <-
  ggplot(data = mv, aes(x = year,
                        y = Emissions, )) +
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  colScale +
  theme(panel.background = element_rect(fill = "gray100")) +
  ggtitle("Motor Vehicle Emissions in Baltimore")

png("Plot 5.png", height = 500, width = 500)
MoV
dev.off()


#question 6
#Compare emissions from motor vehicle sources in Baltimore City with
#emissions from motor vehicle sources in Los Angeles County, California
#fips=="06037"). Which city has seen greater changes over time in motor vehicle emissions?

#build two dataframes, one for Baltimore & one for LA

mrgb <- subset(mrg, fips == "24510")
mrgla <- subset(mrg, fips == "06037")

#subset to motor vehicle emissions
dfb <- subset(
  mrgb,
  EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
)

dfla <- subset(
  mrgla,
  EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
    EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles"
)

#combine dataframes with dplyr

dfboth <- dfb %>%  mutate(Location = 'Baltimore City') %>%
  bind_rows(dfla %>%
              mutate(Location = 'LA County'))
# plot combo graph

png("Plot 6.png", height = 500, width = 500)

ggplot(dfboth, aes(y = Emissions, x = year, color = Location)) +
  stat_summary(fun = sum, na.rm = T, geom = "line") +
  ggtitle("Comparison in Motor Vehicle Emissions: Baltimore vs LA")

dev.off()
