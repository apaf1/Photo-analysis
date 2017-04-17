
#I extracted the metadata using exiftool, http://www.sno.phy.queensu.ca/~phil/exiftool/
#here is the command I used:
##exiftool -r -csv -model -lensID -cameratemperature -focusdistanceupper -focusdistancelower -datetimeoriginal -createdate -aperture -shutterspeed -shutterspeedvalue -exposureprogram -iso -focallength -colortemperature "folderpath" > out2.txt
# -r = recursive, looks at all subfolders as well
# -csv = export to csv-file
#other "-" commands are the tags I extract from metadata
#"folderpath" = file directory to topmost folder
# > out2.txt = export to file "out.txt"

rm(list = ls()) #remove everything from enviorment

library(dplyr)
cat("\"",getSrcDirectory(function(dummy) {dummy}),"\"",sep="") #get folderpath of script
folderpath<-"/Users/Havard/Documents/Mitt liv i bilder/Volume I/Statistikk"
temp<-read.csv(paste0(folderpath,"/out.txt"),stringsAsFactors = F)
df<-read.csv(paste0(folderpath,"/out2.txt"),stringsAsFactors = F)
df<-rbind(temp,df)

df$date<-as.Date(df$DateTimeOriginal,format="%Y:%m:%d")

#####################
##
## Pictures a month plot
##
######################
df$month<-format(df$date,"%Y.%m")
df$month<-paste0(df$month,".01") #to make as.Date() work
dfMonth<-df%>%
  group_by(month)%>%
  summarize(count=n())
dfMonth$month<-as.Date(dfMonth$month,format="%Y.%m.%d")
dfMonth$season<-"grey"
temp<-as.integer(substr(dfMonth$month,6,7))
dfMonth$season[(3<=temp)&(temp<=5)]<-"green"
dfMonth$season[(6<=temp)&(temp<=8)]<-"darkgreen"
dfMonth$season[(9<=temp)&(temp<=11)]<-"orange"

dfModel<-df %>%
  group_by(Model) %>% #camera model
  summarize(count=n(),
            firstDate=date[order(date)][1],
            lastDate=date[order(date)][length(date)],
            meanDate=mean.Date(date[!is.na(date)]),
            midDate=mean.Date(c(firstDate,lastDate)),
            sdDate=sd(as.numeric(date)[!is.na(as.numeric(date))]))
dfModel<-dfModel[dfModel$count>1000,] #to remove friends cameras
dfModel<-dfModel[!(dfModel$Model==""),] #remove pics without camera metadata
dfModel$lastDate[dfModel$Model=="iPhone 5"]<-"2017-04-14" #NA values caused trouble
dfModel<-dfModel[order(dfModel$firstDate),]
dfModel$epsilon<-(((1:length(dfModel$Model))/2)*750)+1000 #different y-values
dfModel$col<-c("red")
for(i in 1:length(dfModel$Model)){
  dfModel$midDate[i]<-mean.Date(c(dfModel$firstDate[i],dfModel$lastDate[i]))
}

dfLens <- df %>%
  group_by(LensID) %>%
  summarize(count=n(),
            firstDate=date[order(date)][1],
            lastDate=date[order(date)][length(date)],
            meanDate=mean.Date(date[!is.na(date)]),
            midDate=mean.Date(c(firstDate,lastDate)),
            sdDate=sd(as.numeric(date)[!is.na(as.numeric(date))]))
dfLens <- dfLens %>% filter(count>5000) %>% filter(!(LensID==""))
#long lens names cause trouble for plotting, shortening them
dfLens$text<-c("Canon 100mm f2.8L macro","Canon EF-S 17-85mm f/4-5.6","Canon EF-S 18-55mm f/3.5-5.6","Tamron 24-70mm f/2.8")
#a lot of date-trouble, found dates with code below
dfLens$firstDate[dfLens$text=="Tamron 24-70mm f/2.8"]<-"2015-03-17" 
dfLens$firstDate[dfLens$text=="Canon EF-S 17-85mm f/4-5.6"]<-"2011-08-30"
dfLens$lastDate[dfLens$text=="Canon EF-S 18-55mm f/3.5-5.6"]<-"2011-08-01"
for(i in 1:length(dfLens$LensID)){
  dfLens$midDate[i]<-mean.Date(c(dfLens$firstDate[i],dfLens$lastDate[i]))
}
dfLens<-dfLens[order(dfLens$firstDate),]
dfLens$epsilon<-(((1:length(dfLens$LensID))/2)*750)+3000
dfLens$col<-c("blue")

#figure out first date of use for lens
i<-1
temp<-df[df$LensID==dfLens$LensID[i],]
temp$dateNum<-as.numeric(temp$date)
hist(temp$dateNum,main=dfLens$LensID[i],xaxt="n")
axis(1,at=temp$dateNum,labels = temp$date)
temp<-temp[order(temp$dateNum),]
rownames(temp)<-NULL
temp<-temp[52:length(temp$Model),]
hist(temp$dateNum,xaxt="n")
temp$date[1]
temp$date[length(temp$date)]

#Finally plotting!
#Plotting fancy plot:
par(mar=c(6,5,5,5))
dfMonth<-dfMonth[!is.na(dfMonth$month),]
plot(-100,-100,xlim=range(dfMonth$month),ylim=range(dfMonth$count),
     ylab="Number of photos a month",xlab="",xaxt="n",main="Pictures a month")
rect(dfMonth$month+10, 0, dfMonth$month-10, dfMonth$count,col=dfMonth$season)
axis.Date(1,dfDay$day,at=seq(as.Date("2004-01-1","%Y-%m-%d"),as.Date("2017-01-1","%Y-%m-%d"),"years"),format="%d.%m.%y",las=2)
for(i in 1:length(dfModel$Model)){
  lines(c(dfModel$firstDate[i],dfModel$lastDate[i]),rep(dfModel$epsilon[i],2),col=dfModel$col[i])
  lines(c(dfModel$meanDate[i],dfModel$meanDate[i]),c(dfModel$epsilon[i],dfModel$epsilon[i]-75),col=dfModel$col[i],lwd=5)
  #lines(c(dfModel$meanDate[i]-dfModel$sdDate[i],dfModel$meanDate[i]+dfModel$sdDate[i]),c(dfModel$epsilon[i],dfModel$epsilon[i]),col="black",lwd=2)
}
dfModel$Model[dfModel$Model=="K610i"]<-"Sony Ericson K610i"
dfModel$midDate[dfModel$Model=="Canon EOS 5D Mark III"]<- dfModel$midDate[dfModel$Model=="Canon EOS 5D Mark III"]-150
text(dfModel$midDate,dfModel$epsilon+150,dfModel$Model,col=dfModel$col,cex=0.7)
for(i in 1:length(dfLens$LensID)){
  lines(c(dfLens$firstDate[i],dfLens$lastDate[i]),rep(dfLens$epsilon[i],2),col=dfLens$col[i])
  lines(c(dfLens$meanDate[i],dfLens$meanDate[i]),c(dfLens$epsilon[i],dfLens$epsilon[i]-75),col=dfLens$col[i],lwd=5)
  #lines(c(dfModel$meanDate[i]-dfModel$sdDate[i],dfModel$meanDate[i]+dfModel$sdDate[i]),c(dfModel$epsilon[i],dfModel$epsilon[i]),col="black",lwd=2)
}
text(dfLens$midDate,dfLens$epsilon+150,dfLens$text,col=dfLens$col,cex=0.7)
legend("topleft",c("Winter","Spring","Summer","Autumn","Lens w. mean","Camera w. mean"),lwd=2.5,col=c("grey","green","darkgreen","orange","blue","red"),lty = 1,cex=0.8)
dev.print(pdf,paste(folderpath,"/picturesAmonthFancy.pdf",sep=""),width=20) #save plot
dev.off()

#Plot grouped to months
par(mar=c(6,5,5,5))
df$month2<-format(df$date,"%m")
dfMonth2<-df%>%
  group_by(month2)%>%
  summarize(count=n())
dfMonth2<-dfMonth2[!is.na(dfMonth2$month2),]
dfMonth2$month2<-as.integer(dfMonth2$month2)
plot(-100,-100,xlim=c(0.5,12.5),ylim=c(0,max(dfMonth2$count)),
     ylab="Number of photos a month",xlab="",xaxt="n",main="Pictures a month")
rect(dfMonth2$month2+0.45, 0, dfMonth2$month2-0.45, dfMonth2$count,col="grey")
axis(1,at=1:12,labels=c("January","February","March","April","May","June","July","August","September","October","November","December"),las=2)
dev.print(pdf,paste(folderpath,"/picturesAmonth2.pdf",sep=""))
dev.off()

#####################
##
## Plots of camera use
##
######################
dfModel$text<-c("K610i","IXUS860","450D","iPhone 5","5D MarkIII")
dfModel$text<-factor(dfModel$text,dfModel$text)
plot(dfModel$text,dfModel$count-70,ylab="Number of photos",xlab="Camera",main="Shots per camera")
rect(as.numeric(dfModel$text)+0.45, 1,as.numeric(dfModel$text)-0.45, dfModel$count,col="grey")
dev.print(pdf,paste(folderpath,"/camera.pdf",sep=""))
dev.off()

dfLens$text<-factor(dfLens$text,dfLens$text)
plot(dfLens$text,dfLens$count-70,ylab="Number of photos",xlab="Lens",main="Shots per lens")
rect(as.numeric(dfLens$text)+0.45, 1,as.numeric(dfLens$text)-0.45, dfLens$count,col="grey")
dev.print(pdf,paste(folderpath,"/lens.pdf",sep=""))
dev.off()


dfAperture<-df%>%
  group_by(Aperture)%>%
  summarize(count=n() ) %>%
  filter(count>100)
dfAperture$Aperture<-as.factor(as.numeric(dfAperture$Aperture))
plot(dfAperture$Aperture,dfAperture$count-70,ylab="Number of photos",xlab="Aperture",main="What aperture do I shoot at?")
rect(as.numeric(dfAperture$Aperture)+0.45, 0,as.numeric(dfAperture$Aperture)-0.45, dfAperture$count,col="grey")
dev.print(pdf,paste(folderpath,"/aperture.pdf",sep=""))
dev.off()

fixShutterSpeed<-function(vector){
  slash<-grepl("/",vector)
  vector<-gsub("/","",vector)
  vector<-as.numeric(vector)
  vector[slash]<-1/vector[slash]
  return(vector)
}

par(mar=c(6,5,5,5))
dfShutterSpeed<-df%>%
  group_by(ShutterSpeed)%>%
  summarize(count=n()) %>%
  filter(count>20) %>%
  filter(!(ShutterSpeed==""))
dfShutterSpeed$ShutterSpeedNum<-fixShutterSpeed(dfShutterSpeed$ShutterSpeed)
dfShutterSpeed<- dfShutterSpeed %>% filter(!((ShutterSpeedNum<0.7)&(count<500)))
dfShutterSpeed<-dfShutterSpeed[order(dfShutterSpeed$ShutterSpeedNum),]
dfShutterSpeed$ShutterSpeed<-factor(dfShutterSpeed$ShutterSpeed,levels=dfShutterSpeed$ShutterSpeed)
plot(dfShutterSpeed$ShutterSpeed,dfShutterSpeed$count-15,main="What shutterspeed do I shoot at?",ylab="Number of photos",xlab="",xaxt="n")
rect(as.numeric(dfShutterSpeed$ShutterSpeed)+0.45, 0,as.numeric(dfShutterSpeed$ShutterSpeed)-0.45, dfShutterSpeed$count,col="grey")
axis(1,dfShutterSpeed$ShutterSpeed,labels=as.character(dfShutterSpeed$ShutterSpeed),las=2)
dev.print(pdf,paste(folderpath,"/shutterspeed.pdf",sep=""))
dev.off()

dfISO<-df%>%
  group_by(ISO)%>%
  summarize(count=n()) %>%
  filter(count>100)
dfISO$ISO<-as.factor(dfISO$ISO)
plot(dfISO$ISO,dfISO$count-50,main="What ISO do I shoot at?",ylab="Number of photos",xlab="ISO")
rect(as.numeric(dfISO$ISO)+0.45, 0,as.numeric(dfISO$ISO)-0.45, dfISO$count,col="grey")
dev.print(pdf,paste(folderpath,"/iso.pdf",sep=""))
dev.off()

dfExposureProgram<-df%>%
  group_by(ExposureProgram)%>%
  summarize(count=n())
#cat(paste0("\"",dfExposureProgram$ExposureProgram,"\","))
dfExposureProgram$ExposureProgram<-c("other0", "other1", "Av","other2","other3", "M", "other4", "other5","P", "Tv")
temp<-dfExposureProgram[grepl("other",dfExposureProgram$ExposureProgram),]
dfExposureProgram<-dfExposureProgram[!grepl("other",dfExposureProgram$ExposureProgram),]
dfExposureProgram<-rbind(dfExposureProgram,data.frame(ExposureProgram="other",count=sum(temp$count)))
dfExposureProgram$ExposureProgram<-factor(dfExposureProgram$ExposureProgram,levels=c("P","Tv","Av","M","other"))
plot(-100,-100,xlim=c(0.5,max(as.numeric(dfExposureProgram$ExposureProgram))+0.5),ylim=range(dfExposureProgram$count),xlab="Mode",xaxt="n",ylab="Number of pictures",main="Photo mode")
rect(as.numeric(dfExposureProgram$ExposureProgram)+0.45, 0,as.numeric(dfExposureProgram$ExposureProgram)-0.45, dfExposureProgram$count,col="grey")
axis(1,labels=dfExposureProgram$ExposureProgram,at=1:5,las=1)
dev.print(pdf,paste(folderpath,"/photoMode.pdf",sep=""))
dev.off()

df$CameraTemperature<-gsub("C","",df$CameraTemperature)
df$CameraTemperature<-gsub(" ","",df$CameraTemperature)
df$CameraTemperature<-as.numeric(df$CameraTemperature)
hist(df$CameraTemperature,main="Camera Temperature",ylab="Number of photos",xlab="Temperature [C]")
dev.print(pdf,paste(folderpath,"/temp.pdf",sep=""))
dev.off()


