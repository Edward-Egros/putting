library(dplyr)

Source_Path.PGA <- 'C:/Users/47227327/Box/Putting/Data/RawData/rshot2014.TXT'
Dest_Path.csv <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2014_AllPutting.csv'
Dest_Path.rdata <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2014_AllPutting.rData'

PGAData <- read.delim(Source_Path.PGA, header = TRUE, sep = ';', as.is = T, stringsAsFactors = F)

## Remove '.' in column names
TblHdr <- colnames(PGAData)
for (i in 1:length(TblHdr)) {
  TblHdr[i] <- gsub(x = TblHdr[i], pattern =  '[.]', replacement = '')
}

colnames(PGAData) <- TblHdr

# Change date from string to Date
PGAData$Date <- as.Date(PGAData$Date, format = '%m/%d/%Y')

sapply(PGAData, class)

## Remove match play
# Match-Play tournament from Edward's email
MatchPlay <- 
  rbind(
    c(2014, 'WGC-Accenture Match Play  The Golf Club at Dove Mountain')
    ,c(2015, 'WGC-Cadillac Match Play. TPC Harding Park')
    ,c(2016, 'WGC-Dell Match Play.  Austin Country Club')
    ,c(2017, 'WGC-Dell Technologies Match Play.  Austin Country Club')
    ,c(2018, 'WGC-Dell Technologies Match Play.  Austin Country Club')
  )
colnames(MatchPlay) <- c('Year', 'TournamentName')

# Specify Match-Play Tournaments
MatchPlay.ThisYear <- unique(PGAData[grep(PGAData$TournamentName, pattern = '*Match Play*',ignore.case = T),]$TournamentName)
# Remove Match-Play Tournaments
PGAData <- subset(PGAData, !(PGAData$TournamentName %in% MatchPlay.ThisYear))

## Create table accounting for score for each putting shot
LastShot <- PGAData[which(PGAData$FromLocationScorer=='Green'), c("Year", "Tourn", "TournamentName", "Player", "PlayerLastName", "Course", "Round", "Hole", "HoleScore", "ParValue" ,"Shot", "Date", "Time", "IntheHoleFlag", "X1stPuttFlag")]

## Calculate score for each hole, it might already be available using HoleScore
LastShot$HoleScore2 <- with(LastShot, Shot - ParValue)
# Might need to refine logic to calculate score for unfinished hole.

## Cumulative sum of  par and score within each player in each tournament, course
LastShot <-
  LastShot %>%
  group_by(Year, Tourn, Player) %>% ## Course is not included as grouping creteria as some tournament runs on multiple courses semutaniously.
  arrange(Year, Tourn, Player, Date, Time) %>%
  #mutate(CumPar = cumsum(as.numeric(ParValue)), CumScore = cumsum(Shot), CumHoleScore = cumsum(as.numeric(HoleScore2)))
  mutate(CumPar = cumsum(as.numeric(ParValue)), CumStrokes = cumsum(Shot), CumHoleScore = cumsum(as.numeric(HoleScore2)))

## Create rolling rank

#TC <- as.data.frame(unique(LastShot[ , c("Tourn", "Course")]))
TC <- unique(LastShot$Tourn)

start_time <- Sys.time()

#AAA <-list(rep(NA, length(TC)))
AAA <-list(rep(NA, length(TC)))
for (j in 1:length(TC)){
  
  Game <- LastShot[which(LastShot$Tourn==TC[j])
                   , c("Year", "Tourn", 'Player', 'PlayerLastName', "Course", "Round", "Date", 'Time'
                       ,"CumPar", "CumStrokes", "CumHoleScore" ) ]
  Game <-
    Game %>%
    group_by(Player) %>%
    mutate(MaxRd = max(Round)) 
  Game <- as.data.frame(Game)
  
  # sapply(Game, class)
  
  Game <- Game[order(Game$Round, Game$Date, Game$Time), ]
  
  nShot <- nrow(Game)
  
  RR <- matrix(0, ncol = 2, nrow = nShot)
  colnames(RR) <- c('Player', 'Rank')
  
  
  for (i in 1:nShot) {
    #if (i == 1) {GameSmall <- rbind(Game[1, ], Game[1, ])}
    #else {GameSmall <- Game[1:i, ]}
    GameSmall <- Game[1:i, ]
    Rd <- GameSmall[i, 'Round']
    Plyr <- GameSmall[i, 'Player']
    # At each time point, create a table of players with most recent score
    Rolling <-
      GameSmall %>%
      filter(MaxRd >= Rd) %>%
      group_by(Tourn, Player) %>%
      arrange(Tourn, Player, Date, Time) %>%
      filter(if (Player == Plyr) {row_number() == max(n()-1, 1)} else {row_number() == n()}) #%>% # If the player is taking the shot, use his/her last score.
    #filter(row_number() == n()) 
    Rolling$Rank <- min_rank(Rolling$CumHoleScore)
    RR[i,]<- unlist(Rolling[which(Rolling$Player == Plyr),c('Player', 'Rank')])
  } 
  
  ## Turn on warnings
  #options(warn=0)
  #warning()
  
  Game$Rank <- RR[, 2]
  
  AAA[[j]] <- Game
}

end_time <- Sys.time()
end_time - start_time

LeaderBoard <- do.call('rbind', AAA)

### Save results
saveRDS(LeaderBoard, Dest_Path.rdata)
write.csv(LeaderBoard, Dest_Path.csv, row.names = F)