plot2 <- function(dataFile){
                library(dplyr)
                cons <- read.csv(dataFile,sep=";",na.strings="?")
                cons4DayComp <- complete.cases(cons)
                conSiNas <- cons[cons4DayComp,][,]
                conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
                consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
                consPlot4DayFinal<-select(consProy1,Date,Time,Global_active_power)
                consPlot4DayFinal <- mutate(consPlot4DayFinal,DateTime=format(as.POSIXct(paste(Date,Time)),na.rm=TRUE))
                plot(as.POSIXct(consPlot4DayFinal$DateTime),consPlot4DayFinal$Global_active_power, type="l", col="black", xaxt = "n",xlab="Days", ylab="Global Active Power (kilowatts)")
                DTmin = as.POSIXct(consPlot4DayFinal[1,4])
                DTmax = as.POSIXct(consPlot4DayFinal[2880,4])
                mid= DTmin + (DTmax-DTmin)/2 
                axis(1, at=c(DTmin,mid,DTmax), labels=c("Thu","Fri","Sat"),tick = TRUE,col.axis="black")
                dev.copy(png, file = "plot2.png", width = 480, height=480)
                dev.off()
}
