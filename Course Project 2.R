#libraries 
install.packages("ggplot")


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

par(mfrow = c(1,1))

ggplot(meg1, aes(x=factor(year), y=Emissions)) + stat_summary(fun.y="sum", geom="line")




with(NEI,plot(as.Date(NEI$year),NEI$Emissions)

