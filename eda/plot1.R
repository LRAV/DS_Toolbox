#load data
library(tidyverse)
library(lubridate)
data = read.csv("../household_power_consumption.txt", header = TRUE, sep= ";", na.strings = "?")
data$Date <- dmy(data$Date)
data$Time <- hms(data$Time)
data <- data %>% filter(Date == "2007-02-01" | Date == "2007-02-02")
data["tmp"] <- data$Date + data$Time

#plot data (png default is 480x480)
png(file = "plot1.png")
with(data, hist(Global_active_power, 
                col = "red", 
                main = "Global Active Power",
                xlab = "Global Active Power (kilowatts)"))

dev.copy(png, "plot1.png")
dev.off()