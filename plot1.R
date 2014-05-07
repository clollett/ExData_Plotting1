
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
  newfocusdata <- cbind(myDateTime,as.numeric(focusdatafinal[,3]))
  # Remaking the names
  names(newfocusdata) <- c("DateTime",names(focusdatafinal)[c(3)])
  # Returning tidy dataframe
  newfocusdata
}
# specific file
filename <- "household_power_consumption.txt"
# Getting tidy data
fd <- GettingTidyData(filename)
# Parameters to set 1 plot, cex for font size and pty for maximum display
par(mfrow=c(1,1))
par(cex.lab=0.8, cex.axis=0.8, cex.main=1)
par(pty="s")
# default margin 5,5,4,2
par(mar=c(5,5,4,2))
# Plotting histogram of Global Active Power with proper annotation, I tried to get the same color, but I couldn't find the exact color
hist(as.numeric(fd$Global_active_power),breaks=12, main="Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency",ylim=c(0,1200), col="orangered2")
# Copying to png file
dev.copy(png,file="plot1.png",width = 480, height = 480)
dev.off()


