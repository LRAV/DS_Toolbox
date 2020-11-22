#load data
library(tidyverse)
library(lubridate)
data = read.csv("../household_power_consumption.txt", header = TRUE, sep= ";", na.strings = "?")
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
data <- data %>% filter(Date == "2007-02-01" | Date == "2007-02-02")
data["tmp"] <- data$Date + data$Time

#plot data (png default is 480x480)
png(file = "plot4.png")
par(mfrow=c(2,2))

#plot 1
with(data, plot(tmp, Global_active_power, 
                ylab = "Global Active Power", 
                xlab = "",
                type = "n"))
with(data, lines(tmp, Global_active_power))

#plot 2
with(data, plot(tmp, Voltage, 
                ylab = "Voltage", 
                xlab = "datetime",
                type = "n"))
with(data, lines(tmp, Voltage))

#plot 3
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
       lty = c(1,1),
       bty="n",
       cex=.5) 

#plot 4
with(data, plot(tmp, Global_reactive_power, type = "n", xlab = "datetime"))
with(data, lines(tmp, Global_reactive_power))

dev.copy(png, "plot4.png")
dev.off()