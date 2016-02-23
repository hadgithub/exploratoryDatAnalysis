plot3 <- function(dataFile){
        library(dplyr)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        cons4DayComp <- complete.cases(cons)
        conSiNas <- cons[cons4DayComp,][,]
        conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        consPlot4DayFinal<-select(consProy1,Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3)
        consPlot4DayFinal <- mutate(consPlot4DayFinal,DateTime=format(as.POSIXct(paste(Date,Time)),na.rm=TRUE))
        plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Sub_metering_1, type="l", col="black", xaxt = "n",xlab="Days", ylab=list("Energy sub_metering"))
        lines(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4Day$Sub_metering_2, type="l", col="red", xaxt = "n")
        lines(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4Day$Sub_metering_3, type="l", col="blue", xaxt = "n")
        DTmin = as.POSIXct(consPlot4DayFinal[1,6])
        DTmax = as.POSIXct(consPlot4DayFinal[2880,6])
        mid= DTmin + (DTmax-DTmin)/2 
        axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
        legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), fill=c("black","red","blue"), horiz=FALSE)
        dev.copy(png, file = "plot3.png", width = 800, height=480)
        dev.off()
}