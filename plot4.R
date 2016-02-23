plot4 <- function(dataFile){
        library(dplyr)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        cons4DayComp <- complete.cases(cons)
        conSiNas <- cons[cons4DayComp,][,]
        conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
        ##Data for second, third and fourth graphs
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        consPlot4DayFinal<-select(consProy1,Date,Time,Global_active_power,Global_reactive_power,Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3)
        consPlot4DayFinal <- mutate(consPlot4DayFinal,DateTime=format(as.POSIXct(paste(Date,Time)),na.rm=TRUE))
        ##consPlot<-filter(consPlot4Day,Global_active_power<=0.5)
        par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
        ##First graph
        plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Global_active_power, type="l", col="black", xaxt = "n",xlab="Days", ylab="Global Active Power")
        DTmin = as.POSIXct(consPlot4DayFinal[1,9])
        DTmax = as.POSIXct(consPlot4DayFinal[2880,9])
        mid= DTmin + (DTmax-DTmin)/2 
        axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
        ##Second graph
        plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Voltage, type="l", col="black", xaxt = "n",xlab="datetime", ylab="Voltage")
        DTmin = as.POSIXct(consPlot4DayFinal[1,9])
        DTmax = as.POSIXct(consPlot4DayFinal[2880,9])
        mid= DTmin + (DTmax-DTmin)/2 
        axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
        ##Third graph
        plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Sub_metering_1, type="l", col="black", xaxt = "n",xlab="Days", ylab=list("Energy sub_metering"))
        lines(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4Day$Sub_metering_2, type="l", col="red", xaxt = "n")
        lines(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4Day$Sub_metering_3, type="l", col="blue", xaxt = "n")
        DTmin = as.POSIXct(consPlot4DayFinal[1,9])
        DTmax = as.POSIXct(consPlot4DayFinal[2880,9])
        mid= DTmin + (DTmax-DTmin)/2 
        axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
        legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), fill=c("black","red","blue"), horiz=FALSE)
        ##Fourth graph
        plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Global_reactive_power, type="l", col="black", xaxt = "n",xlab="datetime", ylab="Global Reactive Power")
        DTmin = as.POSIXct(consPlot4DayFinal[1,9])
        DTmax = as.POSIXct(consPlot4DayFinal[2880,9])
        mid= DTmin + (DTmax-DTmin)/2 
        axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
        dev.copy(png, file = "plot4.png", width = 480, height=480)
        dev.off()
}
        
