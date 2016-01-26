plot3 <- function(dataFile){
        library(dplyr)
        library(ggplot2)
        library(latticeExtra)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        cons4DayComp <- complete.cases(cons)
        conSiNas <- cons[cons4DayComp,][,]
        conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        consPlot4DayFinal<-select(consProy1,Date,Time,Sub_metering_1,Sub_metering_2,Sub_metering_3)
        consPlot4DayFinal <- mutate(consPlot4DayFinal,Date=weekdays(Date))
        consPlot4Day<-group_by(consPlot4DayFinal,Date)
        plot1<-xyplot(Sub_metering_1~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="black",ylab=list("Energy sub_metering"),auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_1"))
        plot2<-xyplot(Sub_metering_2~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="red",ylab=list("Energy sub_metering"),auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_2"))
        plot3<-xyplot(Sub_metering_3~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="blue",ylab=list("Energy sub_metering"),auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_3"))
        plot1+plot2+plot3
        dev.copy(png, file = "plot3.png", width = 480, height=480)
        dev.off()
}