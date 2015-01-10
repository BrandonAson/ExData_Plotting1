Plot4 <- function()  
  
{
  ############### LOAD PACKAGES ################
  
  library(data.table)

  
  ########## LOAD DATASET ##########
  
  dataSet <- read.table("./household_power_consumption.txt",header = TRUE, sep = ";", na.strings = c("?", ""))
  
  
  ####### RECONFIGURE DATE AS CLASS & SELECT DATE RANGE 2007-02-01 to 2007-02-02#########
  
  #Combine date & time into a single column named 'DateTime'
  dataSet$DateTime <- paste(dataSet$Date, dataSet$Time)
  dataSet$DateTime <- strptime(dataSet$DateTime, format = "%d/%m/%Y %H:%M:%S")
  
  
  #select date range 2007-02-01 to 2007-02-02
  selectDates <- subset(dataSet, dataSet$DateTime < ("2007-02-03 00:00:00") & dataSet$DateTime > ("2007-02-01 00:00:00"))

  
  ########## GENERATE PLOTS #############
  

  selectDates$Global_active_power <- as.numeric(selectDates$Global_active_power)

  par(mfrow = c(2,2), bg = "white")
  with(selectDates, {
    
    ## UPPER LEFT PLOT ##
    plot(selectDates$DateTime, selectDates$Global_active_power,type="l", ylab="Global Active Power (kilowatts)", xlab= "", bg = "white")
    
    ## UPPER RIGHT PLOT ##
    plot(selectDates$DateTime, selectDates$Voltage,type="l", ylab="Voltage", xlab= "datetime", bg = "white", cex= 0.5)
    
    ## LOWER LEFT PLOT ##
    plot(selectDates$DateTime, selectDates$Sub_metering_1,type="l", col = "black", ylab="Energy sub metering", xlab= "", bg = "white")
    lines(selectDates$DateTime, selectDates$Sub_metering_2,type="l", col = "red", ylab="Energy sub metering", xlab= "", bg = "white")
    lines(selectDates$DateTime, selectDates$Sub_metering_3,type="l", col = "blue", ylab="Energy sub metering", xlab= "", bg = "white")
    legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1, col = c("black", "red", "blue"), bg = "transparent", box.col = "transparent", cex = 0.8)
  
    ## LOWER RIGHT PLOT ##
    plot(selectDates$DateTime, selectDates$Global_reactive_power,type="l", ylab="Global_reactive_power", xlab= "datetime", bg = "white")
    
    dev.copy(png, filename = "plot4.png", width = 480, height = 480, units = "px")
    dev.off()
    
    })
  

}