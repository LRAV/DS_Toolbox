#load data
library(tidyverse)
library(lubridate)
data = read.csv("../household_power_consumption.txt", header = TRUE, sep= ";", na.strings = "?")
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
data <- data %>% filter(Date == "2007-02-01" | Date == "2007-02-02")
data["tmp"] <- data$Date + data$Time

#plot data (png default is 480x480)
png(file = "plot3.png")
with(data, plot(tmp, Sub_metering_1, 
                ylab = "Energy sub metering", 
                xlab = "",
                type = "n"))
with(data, lines(tmp, Sub_metering_1))
with(data, lines(tmp, Sub_metering_2, col = "red"))
with(data, lines(tmp, Sub_metering_3, col = "blue"))
legend("topright", 
       col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       lty = c(1,1))

dev.copy(png, "plot3.png")
dev.off()