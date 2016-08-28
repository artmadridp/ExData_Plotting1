Plot4 <-function(filename="household_power_consumption.txt"){
      ##Reads from file only the rows related to Feb 1st and 2nd, 2007
      nc<-max(count.fields(filename,sep = ";"))
      JustDate<-read.table(filename,header=T,sep=";",colClasses = c("character",rep("NULL",times = nc-1)))
      JustDate$Date<-as.Date.character(JustDate$Date,"%d/%m/%Y")
      start<-which.max(JustDate$Date=="2007/02/01")
      nCount<-sum(JustDate$Date=="2007/02/01"|JustDate$Date=="2007/02/02")
      DFright<-read.table(filename,skip = start,nrows=nCount,as.is = T,sep = ";")
      colnames(DFright)<-c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
      ## Sets margins and creates png file
      png(filename = "plot4.png",width = 480,height = 480)
      par(mar = c(4,4,4,4),mfrow = c(2,2))
      tick2<-which.max(DFright$Date=="2/2/2007")
      ## Creates first plot
      plot(DFright$Global_active_power,type = "l",xaxt= 'n',ann=F)
      mtext("Global Active Power",side = 2,line=2)
      ## Creates second plot
      plot(DFright$Voltage,type = "l",xaxt= 'n',ann=F)
      axis(1,at=c(1,tick2,2880),labels = c("Thu","Fri","Sat"))
      mtext("Voltage",side = 2,line=2)
      mtext("datetime",side=1,line = 2)
      ## Creates third plot
      plot(DFright$Sub_metering_1,type = "l",xaxt= 'n',ann=F)
      points(DFright$Sub_metering_2,col="red",type = "l")
      points(DFright$Sub_metering_3,col="blue",type = "l")
      axis(1,at=c(1,tick2,2880),labels = c("Thu","Fri","Sat"))
      mtext("Energy sub metering",side = 2,line=2)
      legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lwd = c(1,1,1),lty = c(1,1,1),pch = c(NA,NA,NA),col = c("black","red","blue"),bty = "n")
      ## Creates fourth plot and closes device
      plot(DFright$Global_reactive_power,type = "l",xaxt= 'n',ann=F)
      axis(1,at=c(1,tick2,2880),labels = c("Thu","Fri","Sat"))
      mtext("Global_reactive_power",side = 2,line=2)
      mtext("datetime",side=1,line = 2)
      dev.off()
}