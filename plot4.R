plot4 <- function(dataFile){
        library(dplyr)
        library(ggplot2)
        library(lattice)
        library(gridExtra)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        cons4DayComp <- complete.cases(cons)
        conSiNas <- cons[cons4DayComp,][,]
        conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        ##Data for second, third and fourth graphs
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        consPlot4DayFinal<-select(consProy1,Date,Time,Global_active_power,Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3)
        consPlot4DayFinal <- mutate(consPlot4DayFinal,Date=weekdays(Date))
        consPlot4Day<-group_by(consPlot4DayFinal,Date)
        consPlot<-filter(consPlot4Day,Global_active_power<=0.5)
        par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
        ##First graph
        g1<-xyplot(Global_active_power~Time|Date,data = consPlot, layout = c(4, 1),type="a",col="black",ylab=list("Global_active_power"),xlab=list("datetime"),auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c(""))
        ##Second graph
        g2<-xyplot(Voltage~Time|Date,data = consPlot4Day, layout = c(4, 1),type="a",col="black",ylab=list("Voltage"),xlab=list("datetime"),auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c(""))
        ##Third graph
        plot1<-xyplot(Sub_metering_1~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="black",ylab=list("Energy sub_metering"),xlab="",auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_1"))
        plot2<-xyplot(Sub_metering_2~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="red",ylab=list("Energy sub_metering"),xlab="",auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_2"))
        plot3<-xyplot(Sub_metering_3~ Time | Date, data = consPlot4Day, layout = c(4, 1),type="a",col="blue",ylab=list("Energy sub_metering"),xlab="",auto.key=list(space="top", columns=3, cex.title=1,lines=TRUE, points=FALSE),text=c("Sub_metering_3"))
        g3<-plot1+plot2+plot3
        ##Fourth graph
        g4<-xyplot(Global_active_power~Time | Date, data = consPlot4Day, layout = c(2, 1),auto.key = list(space = "right", points = FALSE, lines = TRUE),plot.points=TRUE,type="a",xlab="datetime")
        grid.arrange(g1, g2, g3, g4, ncol=2, nrow =2)
        dev.copy(png, file = "plot4.png", width = 480, height=480)
        dev.off()
}
        
