####### THIS FILE READS THE RAW FILES AND SUBSETTED RELEVANT INFORMATION FOR MODELLING ######
#######           Shalima Zalsha, Lei Shi - Southern Methodist University         ############################
############################################################################

library(dplyr)

#################### Specify Paths ###############
Source_Path.PGA2014 <- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/rshot2014.TXT'
Source_Path.PGA2015 <- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/rshot2015.TXT'
Source_Path.PGA2016 <- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/rshot2016.TXT'
Source_Path.PGA2017 <- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/rshot2017.TXT'
Source_Path.PGA2018 <- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/rshot2018.TXT'
DestPath_PGAReady<- 'C:/Users/47227327/Box/Putting/ClientReports_ShiZalsha/Data/PGAReady.rData'

#################### Read file  ###############
PGAData2014<- read.delim(Source_Path.PGA2014, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)
PGAData2015<- read.delim(Source_Path.PGA2015, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)
PGAData2016<- read.delim(Source_Path.PGA2016, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)
PGAData2017<- read.delim(Source_Path.PGA2017, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)
PGAData2018<- read.delim(Source_Path.PGA2018, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)

## Remove '.' in column names
TblHdr <- colnames(PGAData2014)
for (i in 1:length(TblHdr)) {
  TblHdr[i] <- gsub(x = TblHdr[i], pattern =  '[.]', replacement = '')
}

## Replace colnames
colnames(PGAData2014) <- TblHdr
colnames(PGAData2015) <- TblHdr
colnames(PGAData2016) <- TblHdr
colnames(PGAData2017) <- TblHdr
colnames(PGAData2018) <- TblHdr

## Filter From Green and Non Match play Only
PGAData2014<-PGAData2014[PGAData2014$FromLocationScorer=="Green",]
PGAData2015<-PGAData2015[PGAData2015$FromLocationScorer=="Green" & PGAData2015$Tourn !=450,]
PGAData2016<-PGAData2016[PGAData2016$FromLocationScorer=="Green" & PGAData2016$Tourn !=220,]
PGAData2017<-PGAData2017[PGAData2017$FromLocationScorer=="Green" & PGAData2017$Tourn !=230,]
PGAData2018<-PGAData2018[PGAData2018$FromLocationScorer=="Green" & PGAData2018$Tourn !=240,]

PGAData<-rbind(PGAData2014,PGAData2015,PGAData2016,PGAData2017,PGAData2018)
	#Tourn<-as.data.frame(unique(cbind(PGAData$Year,PGAData$Tourn,PGAData$TournamentName)))
	#Tourn<-Tourn[order(Tourn$V1,Tourn$V3),]
	#c(2014, ,'WGC-Accenture Match Play  The Golf Club at Dove Mountain')
	#c(2015,450, 'WGC-Cadillac Match Play. TPC Harding Park')
	#c(2016,220, 'WGC-Dell Match Play.  Austin Country Club')
	#c(2017,230, 'WGC-Dell Technologies Match Play.  Austin Country Club')
	#c(2018,240, 'WGC-Dell Technologies Match Play.  Austin Country Club')

## Player's Shortlist 
PGAData$PlayerName<-paste(trimws(PGAData$PlayerFirstName),trimws(PGAData$PlayerLastName))
Shortlist<-c("Jason Day","Jordan Spieth","Bubba Watson","Rickie Fowler","Justin Rose","Dustin Johnson",
"Sergio Garcia","Zach Johnson","Adam Scott","Brooks Koepka","Phil Mickelson","Tiger Woods","Justin Thomas",
"Bryson DeChambeau","Patrick Reed","Jon Rahm","Tony Finau","Webb Simpson","Xander Schauffele","Henrik Stenson")
PGAData$PlayerName[(PGAData$PlayerName %in% Shortlist)==FALSE]<-"Others"

## Conversion
PGAData$ConversionValue<-PGAData$Shot-PGAData$ParValue
PGAData$Conversion<-"Par"
PGAData$Conversion[PGAData$ConversionValue< -1]<-"Eagle-"
PGAData$Conversion[PGAData$ConversionValue==-1]<-"Birdie"
PGAData$Conversion[PGAData$ConversionValue==1]<-"Bogey"
PGAData$Conversion[PGAData$ConversionValue>1]<-"DoubleBogey+"

## HoleSeq
PGAData<-PGAData[order(PGAData$Year,PGAData$Tourn,PGAData$Course,PGAData$Player,PGAData$Round,PGAData$Time), ]
Start<-PGAData[!duplicated(cbind(PGAData$Year,PGAData$Tourn,PGAData$Course,PGAData$Player,PGAData$Round)),]
Start$FirstHole<-Start$Hole
Start<-cbind(Year=Start$Year,Tourn=Start$Tourn,Course=Start$Course,Player=Start$Player,Round=Start$Round,FirstHole=Start$FirstHole)
PGAData2<-merge(PGAData,Start, by=c("Year","Tourn","Course","Player","Round"),all.x=T)
PGAData2$HoleSeq<-(((PGAData2$Hole-PGAData2$FirstHole))%%18)+1
PGAData2<-PGAData2[order(PGAData2$Year,PGAData2$Tourn,PGAData2$Course,PGAData2$Player,PGAData2$Round,PGAData2$Time), ]

#Baseline And Other Variables
PGAData2$PlayerName<-relevel(as.factor(PGAData2$PlayerName), ref ="Others")
PGAData2$Slope<-relevel(as.factor(PGAData2$Slope), ref ="Level")
PGAData2$ParValue<-relevel(as.factor(PGAData2$ParValue), ref ="3")
PGAData2$Conversion<-relevel(as.factor(PGAData2$Conversion), ref ="Birdie")
PGAData2$In<-(PGAData2$IntheHoleFlag=="Y")*1
PGAData2$Distance2<-PGAData2$DistancetoPin^2
PGAData2<-PGAData2[PGAData2$Slope!="Unknown",]

#Write Clean File for 2014-2018
write.csv(PGAData2,"C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/PutLib/PGAClean.csv")
saveRDS(PGAData2, DestPath_PGAReady)

