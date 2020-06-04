#Exploratory Analysis Week 1 - Assignment Project 1
#Analytic Graphing & Base Plotting

#Load required packages
library(dplyr)
library(data.table)
library(ggplot2)
library(lubridate)

#Set working Directory
dir <- "C:\\Users\\frost\\OneDrive\\Desktop\\Online Courses\\Data Science\\1. Modules\\Module 4 - Exploratory Analysis\\2. Assignments\\Week 1 - Analytic Graphing & Base Plotting"
setwd(dir)

#Checking for .zip file then downloading Data file
filename <- "Electric_Power_Consumption.zip"

if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(fileURL, filename, method="curl")
}  

#Checking if folder exists and extracting data from .zip file
if(!file.exists("household_power_consumption.txt")) {
        unzip(filename)
}

#Read in the data and subset for required dates
power <- read.table("./household_power_consumption.txt",
                      sep = ";",
                      skip = 1,
                      col.names = c("Date",
                                    "Time",
                                    "Global_active_power",
                                    "Global_reactive_power",
                                    "Voltage",
                                    "Global_intensity",
                                    "Sub_metering_1",
                                    "Sub_metering_2",
                                    "Sub_metering_3")
)
sub_power <- subset(power, Date %in% c("1/2/2007","2/2/2007"))

#Change Date and Time Character Variables to POSIXct
sub_power$Date <- dmy(sub_power$Date)
datetime <- paste(as.Date(sub_power$Date), sub_power$Time)
sub_power$datetime <- as.POSIXct(datetime)

#Creating Plot 1 as a .png file
png("plot1.png", width = 480, height = 480)
hist(as.numeric(sub_power$Global_active_power), 
     col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)")
dev.off()

#Creating Plot 2 as a .png file
png("plot2.png", width = 480, height = 480)
with(sub_power, {
        plot(Global_active_power~datetime,
             type = "l",
             xlab = "",
             ylab = "Global Active Power (kilowatts)",
             main = "Global Active Power vs. Time"
        )
})
dev.off()

#Creating Plot 3 as a .png file
png("plot3.png", width = 480, height = 480)
with(sub_power, {
        plot(Sub_metering_1~datetime,
             type = "l",
             xlab = "",
             ylab = "Energy sub metering",
             main = "Energy Sub Metering vs. Time"
        )
        lines(Sub_metering_2~datetime, col = "Red")
        lines(Sub_metering_3~datetime, col = "Blue")
        legend("topright", col=c("black", "red", "blue"), lty = 1, lwd = 2,
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
})

dev.off()

#Creating Plot 4 as a .png file
png("plot4.png", width = 480, height = 480)
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(sub_power, {
        plot(Global_active_power~datetime,
             type = "l",
             xlab = "",
             ylab = "Global Active Power (kilowatts)",
             main = "Global Active Power vs. Time"
        )
        plot(Voltage~datetime,
             type = "l",
             xlab = "",
             ylab = "Voltage (volts)",
             main = "Voltage vs. Time"
        )
        plot(Sub_metering_1~datetime,
             type = "l",
             xlab = "",
             ylab = "Energy Sub Metering (watt-hour)",
             main = "Energy Sub Metering vs. Time"
        )
        lines(Sub_metering_2~datetime, col = "Red")
        lines(Sub_metering_3~datetime, col = "Blue")
        legend("topright", col=c("black", "red", "blue"), lty = 1, lwd = 2,
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Global_reactive_power~datetime,
             type = "l",
             xlab = "",
             ylab = "Global Reactive Power (kilowatts)",
             main = "Global Reactive Power vs. Time")
})
dev.off()
