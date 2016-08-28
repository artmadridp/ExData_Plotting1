Plot2 <-function(filename="household_power_consumption.txt"){
      ##Reads from file only the rows related to Feb 1st and 2nd, 2007
      nc<-max(count.fields(filename,sep = ";"))
      JustDate<-read.table(filename,header=T,sep=";",colClasses = c("character",rep("NULL",times = nc-1)))
      JustDate$Date<-as.Date.character(JustDate$Date,"%d/%m/%Y")
      start<-which.max(JustDate$Date=="2007/02/01")
      nCount<-sum(JustDate$Date=="2007/02/01"|JustDate$Date=="2007/02/02")
      DFright<-read.table(filename,skip = start,nrows=nCount,as.is = T,sep = ";")
      colnames(DFright)<-c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
      ## Sets margins and creates png file
      par(mar = c(6,4,4,4))
      png(filename = "plot2.png",width = 480,height = 480)
      tick2<-which.max(DFright$Date=="2/2/2007")
      plot(DFright$Global_active_power,type = "l",xaxt= 'n',ann=F)
      axis(1,at=c(1,tick2,2880),labels = c("Thu","Fri","Sat"))
      mtext("Global Active Power(kilowatts)",side = 2,line=2)
      dev.off()
}