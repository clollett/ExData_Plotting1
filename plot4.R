# This function takes a character vector for dates and a character vector for time
# it paste date and time together to use strptime with the proper format 
# it outputs a POSIXct
combineDT <- function(d,t) {
  if (length(d)!=length(t)) {
    stop("different lengths in vector")
  }
  mydstring=paste(d,t)
  # reads only from this specific format    
  f <- strptime(mydstring,"%d/%m/%Y %H:%M:%S")
  
  
}
# function GettingTidyData takes the url and it makes a preread to extract the row indexes
# for Feb 1, 2007 and Feb 2, 2007
# A second reading to extract the required columns
GettingTidyData <- function(url) {
  # This allows to create a temporal class to make a transformation in reading
  setClass("PreDate")
  setAs("character","PreDate", function(from) as.Date(from, format="%d/%m/%Y") )
  # read preliminary data
  rawdata<-read.table(url, header=TRUE, colClasses=c(Date="PreDate"),sep=";")
  # it gets the row numbers for the study dates
  myindex <- (rawdata$Date=="2007-02-01" | rawdata$Date=="2007-02-02")
  myseq <- seq_along(myindex)[myindex]
  # full file reading in character mode
  focusdata <- read.table(url, header=TRUE, colClasses=c(rep("character",9)), sep=";")
  # subsetting for study dates
  focusdatafinal <- focusdata[myseq,]
  # Combining Date and Time into a POSIXct
  myDateTime <- as.data.frame(combineDT(focusdatafinal$Date,focusdatafinal$Time))
  # Combining columns
  newfocusdata <- cbind(myDateTime,as.numeric(focusdatafinal[,3]),as.numeric(focusdatafinal[,4]),as.numeric(focusdatafinal[,5]),as.numeric(focusdatafinal[,7]),as.numeric(focusdatafinal[,8]),as.numeric(focusdatafinal[,9]))
  # Remaking the names
  names(newfocusdata) <- c("DateTime",names(focusdatafinal)[c(3,4,5,7,8,9)])
  # Returning tidy dataframe
  newfocusdata
}
# specific file
filename <- "household_power_consumption.txt"
# Getting tidy data
fd <- GettingTidyData(filename)
# Plotting histogram of Global Active Power with proper annotation, I tried to get the same color, but I couldn't find the exact color
# Parameters to set 2x2 plots, cex for font size and pty for maximum display
par(mfrow=c(2,2))
par(cex.lab=0.75, cex.axis=0.7)
par(pty="m")
# default margin 5,5,4,2
par(mar=c(5,5,4,1.75)-0.6)
with(fd,{
  # Upperleft plot
  plot(DateTime,Global_active_power, type="l", main="", ylab="Global Active Power", xlab="")
  # Upperright plot
  plot(DateTime,Voltage, type="l", main="", ylab="Voltage", xlab="datetime")
  # Lowerleft plot
  # Plotting blank first
  plot(DateTime,Sub_metering_1, type="n", main="", ylab="Energy sub metering", xlab="")
  # plotting a line per series in black, red and blue 
  lines(DateTime,Sub_metering_1, col="black")
  lines(DateTime,Sub_metering_2, col="red")
  lines(DateTime,Sub_metering_3, col="blue")
  # adding legend
  legend("topright",c("Sub_metering1","Sub_metering2","Sub_metering3"),lwd = 1,col=c("black","red","blue"), cex=0.75, xjust=0, border="white", bty="n")
  # Lowerright plot
  plot(DateTime,Global_reactive_power, type="l", main="", ylab="Global_reactive_power", xlab="datetime")
  
})

# Copying to png file
dev.copy(png,file="plot4.png",width = 480, height = 480)
dev.off()

