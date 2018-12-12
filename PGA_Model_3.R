####### THIS FILE READS CLEANED FILE WITH LB RANKING READY FOR MODELLING  #####
#######           Shalima Zalsha, Lei Shi - Southern Methodist University #####
###############################################################################

#install.packages("pROC")
library(pROC)
library(MASS)

## SPECIFY PATH ##
SourcePath<- 'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/PGAReady_RankUpdated.rData'

# Read Ranked Files
PGAall<- readRDS(SourcePath)
keep<-c("Year","Tourn","Course.x","Round","Player","Hole","Time","IntheHoleFlag","In","HoleSeq","ParValue",
"Distance","Distance2","Slope","Conversion","PlayerName","Rank")
PGA<-PGAall[keep]

# Baseline levels
PGA$PlayerName<-relevel(as.factor(PGA$PlayerName), ref ="Others")
PGA$Slope<-relevel(as.factor(PGA$Slope), ref ="Level")
PGA$ParValue<-relevel(as.factor(PGA$ParValue), ref ="3")
PGA$Conversion<-relevel(as.factor(PGA$Conversion), ref ="Birdie")

# Training and Test Set Round 1,2,3
PGADataTrain<-PGA[PGA$Year <= 2017 & PGA$Round<=3,]
PGAData2018<-PGA[PGA$Year==2018 & PGA$Round<=3,]

# Model Round 1,2,3
m4<-glm(In~ Distance+Distance2+Slope+Conversion+HoleSeq+Rank+PlayerName+ParValue+Distance*Slope+Distance2*Slope, data =PGADataTrain, family = "binomial")

# Model Selection Round 1,2,3
backward<-step(m4,direction="backward")


# Training and Test Set Round 4
PGADataTrain.2<-PGA[PGA$Year <= 2017 & PGA$Round==4,]
PGAData2018.2<-PGA[PGA$Year==2018 & PGA$Round==4,]

# Model Round 4
m4.2<-glm(In~ Distance+Distance2+Slope+Conversion+HoleSeq+Rank+PlayerName+ParValue+Distance*Slope+Distance2*Slope, data =PGADataTrain.2, family = "binomial")
m5.2<-glm(In~ Distance+Distance2+Slope+Conversion+HoleSeq+Rank+PlayerName+ParValue+Distance*Slope, data =PGADataTrain.2, family = "binomial")

# Model Selection Round 4
backward.2<-step(m4.2,direction="backward")

# Final Model Round 1,2,3 & Round 4
write.csv(summary(backward)$coefficients,'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/Result_V1.csv')
write.csv(summary(m5.2)$coefficients,'C:/Users/Shalima/Box Sync/SMU/Fall18------------/Putting/ClientReports_ShiZalsha/Data/Result4_V2.csv')


#PREDICTIONS Round 1,2,3
Prob<- predict(backward, newdata=PGADataTrain, type="response")
Prob.2018 <- predict(backward, newdata=PGAData2018, type="response")
PredPut.2018<-ifelse(Prob.2018 > .5,"pY","pN")
Predict.2018<-cbind(PGAData2018,Prob.2018=round(Prob.2018,3),PP=PredPut.2018)
decision<-table(Predict.2018$IntheHoleFlag, Predict.2018$PP)
c(FP=decision[1,2]/sum(decision[1,]),FN=decision[2,1]/sum(decision[2,]))

#PREDICTIONS Round 4
Prob<- predict(m5.2, newdata=PGADataTrain.2, type="response")
Prob.2018 <- predict(m5.2, newdata=PGAData2018.2, type="response")
PredPut.2018<-ifelse(Prob.2018 > .5,"pY","pN")
Predict.2018<-cbind(PGAData2018.2,Prob.2018=round(Prob.2018,3),PP=PredPut.2018)
decision<-table(Predict.2018$IntheHoleFlag, Predict.2018$PP)
c(FP=decision[1,2]/sum(decision[1,]),FN=decision[2,1]/sum(decision[2,]))

#ROC
par(mfrow=c(1,2))
m1_roc <- roc(PGADataTrain$In~Prob)
plot(m1_roc,main="Model 1: Round 1,2,3")  
m1_roc <- roc(PGADataTrain.2$In~Prob.2)
plot(m1_roc,,main="Model 2: Round 4")   

