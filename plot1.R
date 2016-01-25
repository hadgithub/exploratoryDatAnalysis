plot1 <- function(dataFile){
        library(dplyr)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        cons4DayComp <- complete.cases(cons)
        conSiNas <- cons[cons4DayComp,][,]
        conSiNas<-mutate(conSiNas,Date = as.Date(Date,format="%d/%m/%Y"))
        consProy1 <- filter(conSiNas, Date >= as.Date("01/02/2007",format="%d/%m/%Y") & Date <= as.Date("02/02/2007",format="%d/%m/%Y"))
        hist(consProy1$Global_active_power,col="red",main="Active Global Power",xlab="Global Active Power(kilowatts)",ylab="Frequency")
        dev.copy(png, file = "plot1.png", width = 480, height=480)
        dev.off()
        }