
# This function reads household power consumption data from file
# and returns a data frame containing the data.
# Only data from February 1, 2007 and February 2, 2007 will be
# included in the returned data frame.
# The file containing the data set is assumed to be in the
# current working directory.
readEnergyData <- function(filename="household_power_consumption.txt") {
	data <- read.table("household_power_consumption.txt", sep=";", header=T, colClasses = "character")
    data <- data[(data$Date == "1/2/2007") | (data$Date == "2/2/2007"),]
    
    date.time.vec <- strptime(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")
    data$date.time <- date.time.vec
    rm(date.time.vec)

    data$Global_active_power <- as.numeric(data$Global_active_power)
    data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
    data$Voltage <- as.numeric(data$Voltage)
    data$Global_intensity <- as.numeric(data$Global_intensity)
    data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
    data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
    data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
    
    return(data) 
}

# This function creates plot3 for this assignment and
# stores it as a png file named "plot3.png".  The file will be
# saved in the current working directory.
makePlot3 <- function() {
	data <- readEnergyData()
    plot3.dev <- png(filename="plot3.png", width = 480, height = 480)
    with(data, {
	    plot(date.time,Sub_metering_1, type="n", xlab="", ylab="Energy sub metering")
	    lines(date.time,Sub_metering_1,col="black")
	    lines(date.time,Sub_metering_2,col="red")
	    lines(date.time,Sub_metering_3,col="blue")
	    legend("topright", lwd=1, col=c("black","red","blue"), 
	        legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    })
    dev.off()
    
    rm(plot3.dev)
    rm(data)
}