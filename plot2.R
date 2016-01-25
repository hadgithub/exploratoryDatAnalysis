plot2 <- function(dataFile){
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        consProy1 <- filter(cons,as.Date(Date) >= as.Date("01/02/2007") & as.Date(Date) <= as.Date("02/02/2007"))
        cons4DayComp <- complete.cases(consProy1)
        consPlot2 <- consProy1[cons4DayComp,][,]
        consPlot4DayFinal<-select(consPlot2,Date,Time,Global_active_power)
        with(consPlot4DayFinal,qplot(Date,Global_active_power,ylim=rng))
        dev.copy(png, file = "plot2.png", width = 480, height=480)
        dev.off()
}