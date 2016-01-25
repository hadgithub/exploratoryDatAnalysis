plot1 <- function(dataFile){
        cons <- read.csv(dataFile,sep=";",na.strings="?")
        hist(cons$Global_active_power,col="red",main="Active Global Power",xlab="Global Active Power(kilowatts)",ylab="Frequency")
        dev.copy(png, file = "plot1.png")
        dev.off()
        }