data <- read.csv("C:/Users/prakhar/Desktop/file/forestfires.csv")
data

# Scatterplot matrix of DMC,DC,wind,rain,temp

pairs(~DMC+DC+wind+rain+temp,data=data, 
      main="Scatterplot Matrix")

# 3D Scatterplot of wind,rain,area
#install package.
install.packages("scatterplot3d")
library(scatterplot3d)
attach(data)
scatterplot3d(wind,rain,area, main="3D Scatterplot")

dev.print(pdf, '3D_Scatterplot.pdf')

# Interactive 3D Scatterplot of wind,rain,area

library(rgl)
plot3d(wind, rain, area, col="Blue", size=3)
dev.print(pdf, 'interactive_Scatterplot.pdf')

# Boxplot of X and Y

boxplot(X~Y,data=data, main="Boxplot", 
        xlab="X", ylab="Y")
dev.print(pdf, 'Box_Plot.pdf')

# Simple bar plot of temp, wind, rain [horizontal and vertical]

# Vertical Bar Plot
counts <- table(data$temp)
barplot(counts, main="Temperature Distribution", 
        xlab="Temp")
dev.print(pdf, 'Temp_Distribution.pdf')

counts <- table(data$wind)
barplot(counts, main="Wind Distribution", 
        xlab="Wind")

counts <- table(data$rain)
barplot(counts, main="Rain Distribution", 
        xlab="Rain")

# Horizontal Bar Plot
counts <- table(data$temp)
barplot(counts, main="Temperature Distribution", horiz=TRUE)
dev.print(pdf, 'Horizontal_Temperatue_Distribution.pdf')

counts <- table(data$wind)
barplot(counts, main="Wind Distribution", horiz=TRUE)
dev.print(pdf,'horizontal_wind_distribution.pdf')

counts <- table(data$rain)
barplot(counts, main="Rain Distribution", horiz=TRUE)
dev.print(pdf,'Horizontal_Rain_Distribution.pdf')

# Grouped bar plot of X and Y

counts <- table(data$X, data$Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
dev.print(pdf, 'grouped_Barplot.pdf')

# Histogram of probability distribution of X, Y, wind, temp, area along with line density

hist(data$X, 
     main="Probability Distribution of X", 
     xlab="X", 
     border="Black", 
     col="Blue", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data$X))
dev.print(pdf, 'Probability_Distribution.pdf')

dev.print(pdf, 'dist_X_distribution.pdf')

#probability distribution of y.
hist(data$Y, 
     main="Probability Distribution of Y", 
     xlab="Y", 
     border="black", 
     col="Red", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data$Y))
dev.print(pdf,'dist_y_distribution.pdf')

#probability distribution of wind.
hist(data$wind, 
     main="Probability Distribution of Wind", 
     xlab="Wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(df$wind))
dev.print(pdf,'dist_wind_distribution.pdf')

#probability distribution of temp
hist(data$temp, 
     main="Probability Distribution of Temp", 
     xlab="Temp", 
     border="black", 
     col="Brown", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data$temp))
dev.print(pdf,'dist_temp_distribution.pdf')

#probability distribution of area
hist(data$area, 
     main="Probability Distribution of Temp", 
     xlab="Area", 
     border="black", 
     col="Orange", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(data$area))
dev.print(pdf,'dist_area_distribution.pdf')

# Histogram of frequency distribution of X, Y, wind, temp, area

hist(data$X, 
     main="Frequency Distribution of X", 
     xlab="X", 
     border="Black", 
     col="Blue", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Distribution_x.pdf')


hist(data$Y, 
     main="Frequency Distribution of Y", 
     xlab="Y", 
     border="black", 
     col="Red", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Distribution_y.pdf')

hist(data$wind, 
     main="Frequency Distribution of Wind", 
     xlab="Wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Distribution_wind.pdf')

hist(data$temp, 
     main="Frequency Distribution of Temp", 
     xlab="Temp", 
     border="black", 
     col="Brown", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Distribution_temp.pdf')

hist(data$area, 
     main="Frequency Distribution of Temp", 
     xlab="Area", 
     border="black", 
     col="Orange", 
     las=1, 
     breaks=5, 
     prob = TRUE)
dev.print(pdf, 'Freq_Distribution_area.pdf')


# Pie Chart of area, wind, rain, temp by month
install.packages("dplyr")
library(dplyr)

data_pivot <- summarize(group_by(data,month),wind=sum(wind))
slices <- data_pivot[["wind"]] 
pie(slices, labels=data[["month"]], main="Pie Chart of wind")
dev.print(pdf, 'Pie_Chart.pdf')

data_pivot <- summarize(group_by(data,month),area=sum(area))
slices <- data_pivot[["area"]] 
pie(slices, labels=data[["month"]], main="Pie Chart of area")


data_pivot <- summarize(group_by(data,month),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=data[["month"]], main="Pie Chart of rain")

data_pivot <- summarize(group_by(data,month),temp=sum(temp))
slices <- data_pivot[["temp"]] 
pie(slices, labels=data[["month"]], main="Pie Chart of temp")

# Pie Chart of area, wind, rain, temp by day


data_pivot <- summarize(group_by(data,day),wind=sum(wind))
slices <- data_pivot[["wind"]] 
pie(slices, labels=data[["day"]], main="Pie Chart of wind")
dev.print(pdf, 'Pie_Chart_Day.pdf')

data_pivot <- summarize(group_by(data,day),area=sum(area))
slices <- data_pivot[["area"]] 
pie(slices, labels=data[["day"]], main="Pie Chart of area")


data_pivot <- summarize(group_by(data,day),rain=sum(rain))
slices <- data_pivot[["rain"]] 
pie(slices, labels=data[["day"]], main="Pie Chart of rain")

data_pivot <- summarize(group_by(data,day),temp=sum(temp))
slices <- data_pivot[["temp"]] 
pie(slices, labels=data[["day"]], main="Pie Chart of temp")

# Map Plot of source_Airport_ID

airport <- read.csv("C:/Users/prakhar/Desktop/file/airports.dat")
airport
head(airport)
colnames(airport) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
head(airport)

route <- read.csv("C:/Users/prakhar/Desktop/file/routes.dat")
route
colnames(route) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
head(route)

install.packages("plyr")
library(plyr)
departures <- ddply(route, .(sourceAirportID), "nrow")
names(departures)[2] <- "flight"
arrivals <- ddply(route, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flight"

airport_1 <- merge(airport, departures, by.x = "ID", by.y = "sourceAirportID")

install.packages("ggmap")
library(ggmap)
map <- get_map(location = 'World', zoom = 5)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airport_1, alpha = .6)

mapPoints
dev.print(pdf, 'Map_Plot.pdf')

# Map Plot of destinationAirportID

airport_2 <- merge(airport, arrivals, by.x = "ID", by.y = "destinationAirportID")

library(ggmap)
map <- get_map(location = 'World', zoom = 5)

mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(flights)), data = airport_2, alpha = .6)

mapPoints
dev.print(pdf, 'Map_Plot1.pdf')