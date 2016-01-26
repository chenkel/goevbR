mydata=read.csv("C:/Users/Maike/Desktop/MASTER/trips.csv")


library(ggplot2)
library(ggmap)


goettingen<- ggmap::get_googlemap(center = c(lon = 9.925, lat = 51.54), 
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
ggmap::ggmap(goettingen, extent = "device")+ geom_point(aes(x = destination_lon, y = destination_lat), colour = "red", alpha = 0.1, size = 2, data = mydata) 

                                                 