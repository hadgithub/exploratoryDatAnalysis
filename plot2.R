plot2 <- function(dataFile){
                library(dplyr)
                library(ggplot2)
                library(lattice)
                cons <- read.csv(dataFile,sep=";",na.strings="?")
                cons4DayComp <- complete.cases(cons)
                conSiNas <- cons[cons4DayComp,][,]
                conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
                consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
                consPlot4DayFinal<-select(consProy1,Date,Time,Global_active_power)
                consPlot4DayFinal <- mutate(consPlot4DayFinal,Date=weekdays(Date))
                consPlot4Day<-group_by(consPlot4DayFinal,Date)
                xyplot(Global_active_power~Time | Date, data = consPlot4Day, layout = c(2, 1),auto.key = list(space = "right", points = FALSE, lines = TRUE),plot.points=TRUE,type="a")
                dev.copy(png, file = "plot2.png", width = 480, height=480)
                dev.off()
}
