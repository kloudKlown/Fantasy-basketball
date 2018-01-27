library(plyr)

NewOffense = read.csv("All_Games_17-18.csv")
NewOffense = subset(NewOffense, NewOffense$G !=0)
NewOffense$MP = as.numeric(gsub(':','.',NewOffense$MP))

NewOffense=unique(NewOffense)

NewOffense[is.na(NewOffense)] = 0
AllTeams = unique(NewOffense$Tm)
OffensiveCumilative =  NewOffense[0,]


# revalue(NewOffense$Tm, c("BRK"="BKN", "CHO"="CHA"))
levels(NewOffense$Tm)[levels(NewOffense$Tm)=="BRK"] <- "BKN"
levels(NewOffense$Tm)[levels(NewOffense$Tm)=="CHO"] <- "CHA"

NewOffense$Date = as.Date(NewOffense$Date)

OffensiveCumilative = NewOffense[0,]
#### Player Progressive####

# OffensiveProgressive = OffensiveCumilative
NewOffense$plusMinus  = 0
AllTeams = unique(NewOffense$Tm)

### Defense
DefensiveProgressiveNew = NewOffense[0,]
for (team in 1:length(AllTeams)) {
  TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team])
  
  # Get DATES for each game and order it in ascending
  maxn = length(unique(TestSubPlayer$Date))
  Dates = unique(TestSubPlayer$Date)
  Dates = Dates[order(Dates)]
  
  Position = unique(TestSubPlayer$position)
  
  # For each position in every game
  for(pos in 1:length(Position))
  {
    TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$position == as.character(Position[pos]) )
      
    # Stats for each game
    for ( games in 1:maxn ) {
      
      Date = Dates[games]
      TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$Date < Dates[games])
      Tm = AllTeams[team]
      
      if (games > 5){
        TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$Date < Dates[games] & 
                                 TestSubPlayer$Date > Dates[games-5]  )
        
        DefensiveProgressiveNew =  rbind(DefensiveProgressiveNew, 
                                         data.frame(Tm = as.character(Tm),
                                                    Date = as.character(Date),
                                                    G. = games,
                                                    Pos = as.character(Position[pos]),
                                                    FG = mean(TestSubPlayer[,c("FG")]) ,
                                                    FGA = mean(TestSubPlayer[,c("FGA")]),
                                                    FG. = mean(TestSubPlayer[,c("FG.")]),
                                                    X3P = mean(TestSubPlayer[,c("X3P")]),
                                                    X3PA = mean(TestSubPlayer[,c("X3PA")]),
                                                    FT = mean(TestSubPlayer[,c("FT")]),
                                                    FTA = mean(TestSubPlayer[,c("FTA")]),
                                                    ORB = mean(TestSubPlayer[,c("ORB")]),
                                                    DRB = mean(TestSubPlayer[,c("DRB")]),
                                                    TRB = mean(TestSubPlayer[,c("TRB")]),
                                                    AST = mean(TestSubPlayer[,c("AST")]),
                                                    STL = mean( TestSubPlayer[,c("STL")] ),
                                                    BLK = mean( TestSubPlayer[,c("BLK")] ),
                                                    TOV = mean(TestSubPlayer[,c("TOV")]),
                                                    PF = mean(TestSubPlayer[,c("PF")]),
                                                    PTS = mean(TestSubPlayer[,c("PTS")]),
                                                    GmSc = mean(TestSubPlayer[,c("GmSc")]),
                                                    plusMinus = mean(TestSubPlayer[,c("plusMinus")])
                                         )
        )
        
      }
      
      if (games <= 5){
        DefensiveProgressiveNew =  rbind(DefensiveProgressiveNew, 
                                         data.frame(Tm = as.character(Tm),
                                                    Date = as.character(Date),
                                                    G. = games,
                                                    Pos = as.character(Position[pos]),
                                                    FG = mean(TestSubPlayer[,c("FG")]) ,
                                                    FGA = mean(TestSubPlayer[,c("FGA")]),
                                                    FG. = mean(TestSubPlayer[,c("FG.")]),
                                                    X3P = mean(TestSubPlayer[,c("X3P")]),
                                                    X3PA = mean(TestSubPlayer[,c("X3PA")]),
                                                    FT = mean(TestSubPlayer[,c("FT")]),
                                                    FTA = mean(TestSubPlayer[,c("FTA")]),
                                                    ORB = mean(TestSubPlayer[,c("ORB")]),
                                                    DRB = mean(TestSubPlayer[,c("DRB")]),
                                                    TRB = mean(TestSubPlayer[,c("TRB")]),
                                                    AST = mean(TestSubPlayer[,c("AST")]),
                                                    STL = mean( TestSubPlayer[,c("STL")] ),
                                                    BLK = mean( TestSubPlayer[,c("BLK")] ),
                                                    TOV = mean(TestSubPlayer[,c("TOV")]),
                                                    PF = mean(TestSubPlayer[,c("PF")]),
                                                    PTS = mean(TestSubPlayer[,c("PTS")]),
                                                    GmSc = mean(TestSubPlayer[,c("GmSc")]),
                                                    plusMinus = mean(TestSubPlayer[,c("plusMinus")])
                                         )
        )
        
      }
      
      # DefensiveCumilativeSub2 = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% AllTeams[team], ]
      TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team] & NewOffense$position == as.character(Position[pos]))
    } 
    
    TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team])
  }

}

DefensiveProgressiveNew[is.na(DefensiveProgressiveNew)] = 0
rm(OffensiveCumilative)

PlayerPosition = as.data.frame(NewOffense$playerName)
PlayerPosition["Pos"] = as.data.frame(NewOffense$position)
PlayerPosition = unique(PlayerPosition)


colnames(DefensiveProgressiveNew) = c("Opp","Date","G.","position","FG","FGA","FGP","X3P","X3PA","FT","FTA",
                   "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","GMSC","plusMinus")


NewOffense_Combined = rbind.fill(NewOffense[0,], DefensiveProgressiveNew[0,]) 

NewOffense_Combined = merge(NewOffense ,DefensiveProgressiveNew, by = c("Date","Opp","position") )




write.csv(DefensiveProgressiveNew, file = 'DVOA.csv')


write.csv(PlayerPosition, file = "PlayerPosition_17.csv")
write.csv(NewOffense_Combined, file ="NewOffense_Combined_17.csv")



