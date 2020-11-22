#load data
library(tidyverse)
library(lubridate)
data = read.csv("../household_power_consumption.txt", header = TRUE, sep= ";", na.strings = "?")
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
data <- data %>% filter(Date == "2007-02-01" | Date == "2007-02-02")
data["tmp"] <- data$Date + data$Time

#plot data (png default is 480x480)
png(file = "plot2.png")
with(data, plot(tmp, Global_active_power, 
                ylab = "Global Active Power (kilowatts)", 
                xlab = "",
                type = "n"))
with(data, lines(tmp, Global_active_power))

dev.copy(png, "plot2.png")
dev.off()