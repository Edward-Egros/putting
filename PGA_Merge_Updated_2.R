########      THIS FILE READS THECLEAN FILE AND MERGE IT WITH LEADERBOARD RANKING  ###### ######
#######           Shalima Zalsha, Lei Shi - Southern Methodist University         ############################
############################################################################

library(dplyr)

### Specify Paths
SourcePath_LB2014 <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2014_AllPutting.rData'
SourcePath_LB2015 <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2015_AllPutting.rData'
SourcePath_LB2016 <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2016_AllPutting.rData'
SourcePath_LB2017 <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2017_AllPutting.rData'
SourcePath_LB2018 <- 'C:/Users/47227327/Box/Putting/Data/LeaderBoard2018_AllPutting.rData'
SourcePath_PGAData <- 'C:/Users/47227327/Box/Putting/ClientReports_ShiZalsha/Data/PGAReady.rData'

DestPath_PGAReadyUpdatedRank <- 'C:/Users/47227327/Box/Putting/ClientReports_ShiZalsha/Data/PGAReady_RankUpdated.rData'

### Read in updated rank data
RankData2014 <- readRDS(SourcePath_LB2014)
RankData2015 <- readRDS(SourcePath_LB2015)
RankData2016 <- readRDS(SourcePath_LB2016)
RankData2017 <- readRDS(SourcePath_LB2017)
RankData2018 <- readRDS(SourcePath_LB2018)
RankData <- rbind(RankData2014
                  ,RankData2015
                  ,RankData2016
                  ,RankData2017
                  ,RankData2018)

PGAData <- readRDS(SourcePath_PGAData)

### Remove previous rank columns
aa <- as.character(colnames(PGAData))
PGAData_noRank <- PGAData[, head(aa, 57)]

## Order data and create DupOrder
PGAData_noRank <-
PGAData_noRank %>%
  group_by(Year, Tourn, Player, Round, Date, Time) %>%
    arrange(Year, Tourn, Player, Round, Date, Time, IntheHoleFlag) %>%
      mutate(DupOrder = row_number())

## Order data and create DupOrder
RankData <-
RankData %>%
  group_by(Year, Tourn, Player, Round, Date, Time) %>%
    arrange(Year, Tourn, Player, Round, Date, Time) %>%
      mutate(DupOrder = row_number())

### Join data
RankDataUpdated <- sqldf('select a.*, b.Rank 
                         from PGAData_noRank as a
                          left join RankData as b
                            on a.Year = b.Year
                            and a.Tourn = b.Tourn
                            and a.Player = b.Player
                            and a.Round = b.Round
                            and a.Date = b.Date
                            and a.Time = b.Time
                            and a.DupOrder = b.DupOrder')

### Reorder data for spot check on accuracy
RankDataUpdated <- RankDataUpdated[with(RankDataUpdated, order(Year, Tourn, Round, Date, Time, DupOrder)), ]
PGAData <- PGAData[with(PGAData, order(Year, Tourn, Round, Date, Time)), ]

## Select 2018 Tournaments results
Before <-
PGAData %>%
  filter(Round == 4, Year == 2018) %>%
  group_by(Year, Tourn, Round, Player) %>%
  arrange(Year, Tourn, Round, Date, Time) %>%
  filter(row_number()==n()) %>%
  ungroup %>%
  select(TournamentName, Date, Time, PlayerLastName.y, Rank, LagRank)

After <-
RankDataUpdated %>%
  filter(Round == 4, Year == 2018) %>%
    group_by(Year, Tourn, Round, Player) %>%
      arrange(Year, Tourn, Round, Date, Time, DupOrder) %>%
        filter(row_number()==n()) %>%
          ungroup %>%
          select(TournamentName, Date, Time, PlayerLastName.y, Rank)

### Save updated rank data
saveRDS(RankDataUpdated, DestPath_PGAReadyUpdatedRank)
