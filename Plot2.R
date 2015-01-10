Plot2 <- function()  
  
{
  ############### LOAD PACKAGES ################
  
  library(data.table)

  ########## LOAD DATASET ##########
  
  dataSet <- read.table("./household_power_consumption.txt", header = TRUE, sep = ";", na.strings = c("?", ""))
  
  
  ####### RECONFIGURE DATE AS CLASS & SELECT DATE RANGE 2007-02-01 to 2007-02-02#########
  
  #Combine date & time into a single column named 'DateTime'
  dataSet$DateTime <- paste(dataSet$Date, dataSet$Time)
  dataSet$DateTime <- strptime(dataSet$DateTime, format = "%d/%m/%Y %H:%M:%S")
  
  
  #select date range 2007-02-01 to 2007-02-02
  selectDates <- subset(dataSet, dataSet$DateTime < ("2007-02-03 00:00:00") & dataSet$DateTime > ("2007-02-01 00:00:00"))

  
  ########## GENERATE PLOT #############
  
  selectDates$Global_active_power <- as.numeric(selectDates$Global_active_power)
  plot(selectDates$DateTime, selectDates$Global_active_power,type="l", ylab="Global Active Power (kilowatts)", xlab= "", bg = "white")
  dev.copy(png, filename = "plot2.png", width = 480, height = 480, units = "px")
  dev.off()
  
  
}