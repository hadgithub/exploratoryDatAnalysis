plot1 <- function(dataFile){
        library(dplyr)
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        consProy1 <- filter(cons,as.Date(Date) >= as.Date("02/01/2007") & as.Date(Date) <= as.Date("02/02/2007"))
        hist(consProy1$Global_active_power,col="red",main="Active Global Power",xlab="Global Active Power(kilowatts)",ylab="Frequency")
        dev.copy(png, file = "plot1.png", width = 480, height=480)
        dev.off()
        }