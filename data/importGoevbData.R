# ## Import script
goevb = read.csv(
  "data/goevb.csv",
  sep = ",",
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# ## Converting the date to a recognizable format
goevb$datetime <-
  strptime(goevb$datetime, format = '%d/%m/%Y:%H:%M:%S')

# ## Get the day and hour of each trip
goevb$day <- goevb$datetime$wday
# start week by Monday(1) and end by Sunday(7)
goevb$day[goevb$day == 0] = 7
goevb$day_f <-
  factor(goevb$day, labels = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))
goevb$hour <- goevb$datetime$hour

# delete not needed variables
goevb$datetime <- goevb$line <- NULL

saveRDS(goevb, 'data/goevb.rds')