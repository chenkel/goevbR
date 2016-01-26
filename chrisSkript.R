library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(ggmap)

# ## Reading in the data
goevb = read.csv("/Users/chenkel/Dropbox/2.\ Studium/AAA\ Master/AA\ LETZTES\ SEMESTER/4a\ Introduction\ to\ R/alles.csv", stringsAsFactors = FALSE)

# ## Converting the date to a recognizable format
goevb$datetime <- strptime(goevb$datetime, format = '%d/%m/%Y:%H:%M:%S')

# ## Getting the day and hour of each trip
goevb$day <- weekdays(goevb$datetime, abbreviate = FALSE)
goevb$hour <- goevb$datetime$hour

# ## Sorting the weekdays
dailyTrips <- as.data.frame(table(goevb$day, goevb$hour))
names(dailyTrips) <- c('day', 'hour', 'freq')
dailyTrips$hour <- as.numeric(as.character(dailyTrips$hour))
dailyTrips$day <- factor(dailyTrips$day, 
                         ordered = TRUE,
                         levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))

# ## Plotting the number of trips each day (line graph)
png(filename = 'dailyTrips', width = 800, height = 600, units = 'px')
ggplot(dailyTrips, aes(x = hour, y = freq)) + 
  geom_line(aes(group = day, color = day)) + 
  xlab('Hour') + 
  ylab('Number of trips') + 
  ggtitle('Daily number of Bus Trips')
dev.off()

# ## Plotting the number of trip each day (heat map)
png(filename = 'heatmap.png', width = 800, height = 600, units = 'px')
ggplot(dailyTrips, aes(x = hour, y = day)) + 
  geom_tile(aes(fill = freq)) + 
  scale_fill_gradient(name = 'Total Bus Trips', 
                      low = 'black', 
                      high = 'red') +
  theme(axis.title.y = element_blank())
dev.off()

# ## Get Goettingen map
goeMap <- ggmap::get_googlemap(center = c(lon = 9.925, lat = 51.54), 
                                   zoom = 12,
                                   size = c(640,400),
                                   scale = 2,
                                   format = c("png8"),
                                   maptype = c("roadmap"), sensor = FALSE, 
                                   messaging = FALSE,
                                   urlonly = FALSE,
                                   crop = TRUE, 
                                   color = c("color"), 
                                   language = ("en-EN"))

dev.off()

# ## Get trip locations
table(goevb$origin_lon, goevb$origin_lat)
locationTrips <- as.data.frame(table(goevb$origin_lon, goevb$origin_lat))
names(locationTrips) <- c('origin_lon', 'origin_lat', 'Frequency')
locationTrips$origin_lon <- as.numeric(as.character(locationTrips$origin_lon))
locationTrips$origin_lat <- as.numeric(as.character(locationTrips$origin_lat))
locationTrips <- subset(locationTrips, Frequency > 0)
 

ggmap::ggmap(goeMap, extent = "device") +
  geom_point(aes(x = origin_lon, y = origin_lat, alpha = Frequency), 
             colour = "red", 
             size = 6, 
             data = locationTrips)

