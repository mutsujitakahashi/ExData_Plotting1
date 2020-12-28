library(tidyverse)
library(data.table)
#-----------------------------------------------------------------------
file_read <- function(filename, t_from, t_to) {
    df <- fread(in_filename, sep=";", 
                colClasses=c("Date"="character",
                             "Time"="character",
                             "Global_active_power"="numeric",  
                             "Global_reactive_power"="numeric",
                             "Voltage"="numeric",
                             "Global_intensity"="numeric",
                             "Sub_metering_1"="numeric",
                             "Sub_metering_2"="numeric",
                             "Sub_metering_3"="numeric"),
                na.strings="?")
    cols <- names(df)[3:9]
    tbl <- df %>%
        as_tibble %>%
        mutate(DateTime = strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S", tz="Europe/Paris")) %>%
        select(DateTime, all_of(cols)) %>%    
        filter(t_from <= DateTime & DateTime < t_to)
    return(tbl)
}
#-----------------------------------------------------------------------
plot1 <- function() {
    hist(tbl$Global_active_power,col="red",xlab="Global Active Power(kilowatts)", main="Global Active Power")
}
#-----------------------------------------------------------------------
plot2 <- function() {
    with(tbl, plot(DateTime, Global_active_power, type="l", xlab="", ylab="Global Active Power(kilowatts)"))
}
#-----------------------------------------------------------------------
plot3 <- function(box=T) {
    with(tbl, {
        plot(DateTime, Sub_metering_1, type="l", col="red", ylim=c(0,40), ann=F )
        par(new=T)
        plot(DateTime, Sub_metering_2, type="l", col="magenta1", ylim=c(0,40), ann=F)
        par(new=T)
        plot(DateTime, Sub_metering_3, type="l", col="blue",  ylim=c(0,40), xlab="", ylab="Energy sub metering")
        if (box) {
            legend(x="topright",lty=1, col=c("red","magenta1","blue"),
                 legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        } else {legend(bty="n", x="topright",lty=1, col=c("red","magenta1","blue"),
                 legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        }
        #legend(temp)
        #bty=n
    })
}
#=============== BEGIN_OF_MAIN ========================================================
action <- 2    # 1:plot1, 2:plot2, 3:plot3, 4:plot4, 
#==================================================
out_filename <- c("plt1.png", "plt2.png", "plt3.png", "plt4.png")
in_filename <- "household_power_consumption.txt"
#in_filename <- "x.txt"
setwd("~/cw/data-science/04_Exploratory_Data_Analysis/01_the_basics_of_analytic_graphics/ExData_Plotting1")
loc <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
tbl <- file_read(in_filename,
                 t_from=strptime("2007-02-01 00:00:00", "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"),
                 t_to  =strptime("2007-02-03 00:00:00", "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"))
#--------------------------------------------------------------------
#dev.off()
if (action==1) {
    plot1()
} else if (action==2) {
    plot2()
} else if (action==3) {
    plot3()
} else if (action==4) {
    par(mfrow=c(2, 2), mar=c(4,5,2,1), oma=c(1,1,1,1))    
    with(tbl, {
        plot2()
        plot(DateTime, Voltage, type="l")
        plot3(box=F)
        plot(DateTime, Global_reactive_power, type="l")        
    })
}
dev.copy(png, filename=out_filename[action], width=480, height=480)
dev.off()
#=============== END_OF_MAIN ========================================================
