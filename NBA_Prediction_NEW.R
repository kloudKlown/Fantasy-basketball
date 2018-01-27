library(BiodiversityR)
library(ggplot2)
library(e1071)
library(tcltk2)
library(xtable)
library(reshape2)
library(glm2)
library(HH)
library(Hmisc)
library(boot)
library(deepnet)
library(designGG)
library(BiodiversityR)
library(ggplot2)
library(e1071)
library(xtable)
library(caret)
library(randomForest)
library(pROC)
library (ROCR)
library(foreign)
library(ROSE)
library(reshape)
library(ScottKnott)
library(PMCMR)

library(reshape2)
library(NMF)
library(car)
library(Hmisc)
library(rms)
library(plyr) # for mapping vector values
library(psych) # for corr.test()
library(earth)
library(nnet)
library(neuralnet)
library(brnn)
library(frbs)

library(party)
library(bartMachine)
library(rqPen)
library(matrixStats)
# install.packages("neuralnet",repos="http://cran.rstudio.com/", dependencies=TRUE)
# install.packages("brnn",repos="http://cran.rstudio.com/", dependencies=TRUE)
# install.packages("caret",repos="http://cran.rstudio.com/", dependencies=TRUE)
# install.packages("bartMachine",repos="http://cran.rstudio.com/", dependencies=TRUE)
# install.packages("randomForest",repos="http://cran.rstudio.com/", dependencies=TRUE)
# install.packages("rqPen",repos="http://cran.rstudio.com/", dependencies=TRUE)
library(bartMachine)
library(neuralnet)
library(randomForest)
library(brnn)
library(neuralnet)
library(rqPen)
library(h2o)

###########################################################
###########################################################
###########################################################
localH2O <- h2o.init()
# h2o.shutdown()


# OldOffense = read.csv("All_Players2015-16.csv")

Offense2014 = read.csv("2014_TrainingData.csv")
Offense2015 = read.csv("2015_TrainingData.csv")
Offense2016 = read.csv("2016_TrainingData.csv")

Offense2014$G. = Offense2014$G. + 100
Offense2015$G. = Offense2015$G. + 200
Offense2016$G. = Offense2016$G. + 300
Offense2016 = Offense2016[,-c(1)]
Offense2015 = Offense2015[,-c(1)]
Offense2014 = Offense2014[,-c(1)]

ColnamesTraining = function( dataset ){
  
  
  colnames(dataset) = c( "G."   , "playerId" , "Date"   ,       "MP"   ,         "FG P" ,         "FGA P"    ,    
                         "FGx P",   "X3P P" ,"X3PA P", "FT P"   ,       "FTA P" ,        "ORB P",         "DRB P"   ,     
                         "TRB P" ,  "AST P", "STL P",  "BLK P"  ,       "TOV P" ,        "PF P",          "PTS P" ,       
                         "GmSc P" , "plusMinus P","UsageRating",   "DFS P"        , "Pos"     ,      "Name"     ,     "NewTeam",      
                         "Tm P" , "FGx MT"  , "FGA MT", "FG MT" ,        "X3P MT" ,        "X3PA MT"  ,  "FT MT"    ,     
                         "FTA MT" , "ORB MT", "DRB MT", "TRB MT",         "AST MT"  ,       "STL MT"  ,       "BLK MT",        
                         "TOV MT" , "PF MT",  "PTS MT","GmSc MT",        "plusMinus MT" ,  "DFS MT"   ,      "Tm OT",         
                         "FGx OT" ,"FGA OT",  "FG OT" , "X3P OT" ,          "X3PA OT ",          "FT OT" ,           "FTA OT" ,         
                         "ORB OT", "DRB OT", "TRB OT" ,"AST OT" ,          "STL OT"   ,        "BLK OT" ,  "TOV OT" ,         
                         "PF OT",  "PTS OT", "GmSc OT", "plusMinus OT",     "DFS OT"   ,        "G.OPProup.1",   "MP OPP" ,      
                         "FG OPP", "FGA OPP",  "FG OPP.", "X OPP3P",       "X OPP3PA" ,     "FT OPP"  ,      "FTA OPP"  ,    
                         "ORB OPP","DRB OPP",  "TRB OPP","AST OPP",       "STL OPP"   ,    "BLK OPP",       "TOV OPP"  ,    
                         "PF OPP" ,"PTS OPP",  "G.OPPmSc",".OPPplusMinus", "Rest" )
  return( dataset )
}


ColnamesTEST = function( dataset ){
  
  colnames(dataset) = c(
    "Tm", "playerId", "playerName", "G", "Date", "Opp" , "GS","MP", "Rest", "FG P" ,         "FGA P"    ,    
    "FGx P",   "X3P P" ,"X3PA P","X3P PP" , "FT P"   ,       "FTA P" , "FTP P"  ,      "ORB P",         "DRB P"   ,     
    "TRB P" ,  "AST P", "STL P",  "BLK P"  ,       "TOV P" ,        "PF P",          "PTS P" ,       
    "GmSc P" , "plusMinus P","UsageRating",   "DFS P"        , "Pos"     ,      "Name"     ,     "NewTeam",   "G P",         
    "FGp MT" , "FGA MT"  ,       "FG MT"    ,     "X3P MT"   ,      "X3PA MT"   ,     "FT MT"  ,        "FTA MT"   ,     
    "ORB MT",  "DRB MT" ,        "TRB MT"  ,       "AST MT" ,        "STL MT"   ,      "BLK MT"   ,      "TOV MT" ,       
    "PF MT" ,  "PTS MT",         "GmSc MT",        "plusMinus MT",   "DFS MT"    ,     "G  MT"    ,      "FG OT"   ,        
    "FGA OT","FG OT",           "X3P OT",           "X3PA OT"   ,       "FT OT"   ,         "FTA OT"  ,         "ORB OT",          
    "DRB OT","TRB OT",           "AST OT",           "STL OT"   ,        "BLK OT"  ,         "TOV OT"   ,        "PF OT",           
    "PTS OT" , "GmSc OT",          "plusMinus OT",     "DFS OT",           "G.OPProup.1",   "MP OPP"    ,    "FG OPP",       
    "FGA OPP" , "FG OPP" ,      "X.OPP3P" ,      "X.OPP3PA" ,     "FT OPP"  ,      "FTA OPP"  ,     "ORB OPP"     , 
    "DRB OPP" , "TRB OPP" ,      "AST OPP",       "STL OPP" ,      "BLK OPP",       "TOV OPP"  ,     "PF OPP"     ,  
    "PTS OPP" ,   "G.OPPmSc",      ".OPPplusMinus" )
  
  return(dataset)
}

# ColnamesTEST(FinalTest)

Offense2014 = ColnamesTraining(Offense2014)
Offense2015 = ColnamesTraining(Offense2015)
Offense2016 = ColnamesTraining(Offense2016)



NewOffense = read.csv("All_Players2016-17.csv")
NewOffense = subset(NewOffense, NewOffense$G !=0)
NewOffense$MP = as.numeric(gsub(':','.',NewOffense$MP))

# NewOffense = subset(NewOffense, NewOffense$Date != "2017-02-02")


# OldOffense$MP = as.numeric(gsub(':','.',OldOffense$MP))


# NewOffense = subset(NewOffense, NewOffense$Date != "2017-02-12")
# NewOffense = subset(NewOffense, NewOffense$Date != "2017-02-14")
# NewOffense = subset(NewOffense, NewOffense$Date != "2017-02-13")
# 
# NewOffense = subset(NewOffense, NewOffense$Date != "2017-02-11")
# # NewOffense = subset(NewOffense, NewOffense$Date != "2017-01-20")
# NewOffense = subset(NewOffense, NewOffense$Date != "2017-01-21")

# Injuries = data.frame()
Injuried = read.csv("NotPlaying.csv")
exclude_Metric  <- c("X.nbsp.","X.nbsp.RP1","Age","Rk","position")

# 
# OldOffense <- OldOffense[, -which(names(OldOffense) %in% exclude_Metric)]
NewOffense <- NewOffense[, -which(names(NewOffense) %in% exclude_Metric)]

NewOffense=unique(NewOffense)

New2017 = read.csv("AllPlayersNew_20171.csv")
DKSal = read.csv("DKSalaries (44).csv")

for ( i in 1:nrow(New2017) ){
  if ( '-' %in% ( (strsplit(as.character(New2017[i,c("Pos")]),"")[[1]]) ) ){
    print ( New2017[i,c("Pos")] )
    New2017[i,c("Pos")] = substring( New2017[i,c("Pos")],1,1)
  }
  
}


#   
# OffensiveCumilative = NewOffense[0,]
# AllTeams = unique(OldOffense$Tm)
# OldOffense[is.na(OldOffense)] = 0
NewOffense[is.na(NewOffense)] = 0


# OldOffense$DFS = OldOffense$PTS + OldOffense$AST * 1.5 + ( OldOffense$TRB  ) * 1.25 +
# OldOffense$STL * 2 + OldOffense$BLK * 2
# 
NewOffense$DFS = NewOffense$PTS + NewOffense$AST * 1.5 + ( NewOffense$TRB ) * 1.25 +
  NewOffense$STL * 2 + NewOffense$BLK * 2 - NewOffense$TOV * 0.5 

write.csv(NewOffense,file = 'allSampleData.csv' )

NewOffense[,c("UsageRating")] = 0
New = NewOffense[0,]
AllPlayers = unique(NewOffense$playerId)

for (each in 1:length(AllPlayers)){
  PN = subset(NewOffense,NewOffense$playerId == AllPlayers[each] )
  
  
  for (J in 1:nrow(PN)){
    team = PN[J,]$Tm
    date = PN[J,]$Date
    
    PN_Teams = subset(NewOffense, NewOffense$Tm == team)
    PN_Teams = subset(PN_Teams, PN_Teams$Date == date)
    
    
    
    temp =  100 *  ( (PN[J,]$FGA) + 0.44 * (PN[J,]$FTA) + (PN[J,]$TOV) ) * ( sum(PN_Teams$MP) / 5  ) /  
      ( ( sum(PN_Teams$FGA) + sum(PN_Teams$FTA) * 0.44 + sum(PN_Teams$TOV)  )*PN[J,]$MP)
    
    PN[J,c("UsageRating")] = temp
    
  }
  New = rbind(New, PN)
  
  
}
NewOffense = New


## for each calculate the defensive stats. basically summation of everything.
############################################################################
# Calculate Usage Rating
########### NEWWWWW Progressive

AllTeams = unique(NewOffense$Tm)
OffensiveCumilative =  NewOffense[0,]

for (team in 1:length(AllTeams)) {
  TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team])
  TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$G == 1)
  
  
  OffensiveCumilative =rbind(OffensiveCumilative,
                             data.frame(Tm = TestSubPlayer[1,c("Tm")],
                                        G. = 1,
                                        FG = sum(TestSubPlayer[,c("FG")]),
                                        FGA = sum(TestSubPlayer[,c("FGA")]),
                                        FG. = sum(TestSubPlayer[,c("FG.")]),
                                        X3P = sum(TestSubPlayer[,c("X3P")]),
                                        X3PA = sum(TestSubPlayer[,c("X3PA")]),
                                        FT = sum(TestSubPlayer[,c("FT")]),
                                        FTA = sum(TestSubPlayer[,c("FTA")]) ,
                                        ORB = sum(TestSubPlayer[,c("ORB")]),
                                        DRB = sum(TestSubPlayer[,c("DRB")]),
                                        TRB = sum(TestSubPlayer[,c("TRB")]),
                                        AST = sum(TestSubPlayer[,c("AST")]),
                                        STL = sum( TestSubPlayer[,c("STL")] ),
                                        BLK = sum( TestSubPlayer[,c("BLK")] ),
                                        TOV = sum(TestSubPlayer[,c("TOV")]),
                                        PF = sum(TestSubPlayer[,c("PF")]),
                                        PTS = sum(TestSubPlayer[,c("PTS")]),
                                        GmSc = sum(TestSubPlayer[,c("GmSc")]),
                                        plusMinus = sum(TestSubPlayer[,c("plusMinus")]),
                                        DFS = sum(TestSubPlayer[,c("DFS")])
                                        #                          dfsdk = sum(TestSubPlayer[,c("Tgt")])
                             ) )        
  
  
}


OffensiveCumilative = NewOffense[0,]
#### Player Progressive####
DefensiveProgressiveNew = NewOffense[0,]
# OffensiveProgressive = OffensiveCumilative
NewOffense$plusMinus  = 0
AllTeams = unique(NewOffense$Tm)
### Defense
for (team in 1:length(AllTeams)) {
  TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team])
  # DefensiveCumilativeSub2 = subset(DefensiveProgressive,DefensiveProgressive$Tm == AllTeams[team])
  DefensiveCumilativeSub2 = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% AllTeams[team], ]
  # inital = TestSubPlayer[1,c("G.")]
  
  for ( games in 2:max(TestSubPlayer$G) ) {
    TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$G == games-1)
    DefensiveCumilativeSub2 = subset(DefensiveCumilativeSub2, DefensiveCumilativeSub2$G. == games-1)
    
    if (nrow(DefensiveCumilativeSub2) == 0){
      DefensiveCumilativeSub2[1,] = 0
      Tm1 = TestSubPlayer[1,c("Tm")]
    }
    
    if (nrow(DefensiveCumilativeSub2) != 0){
      Tm1 = DefensiveCumilativeSub2[1,c("Tm")]
    }
    
    DefensiveProgressiveNew =  rbind(DefensiveProgressiveNew, 
                                     data.frame(Tm = TestSubPlayer[1,c("Tm")],
                                                G. = games,
                                                FG = sum(TestSubPlayer[,c("FG")]) ,#+DefensiveCumilativeSub2[,c("FG")] ,
                                                FGA = sum(TestSubPlayer[,c("FGA")]),#+DefensiveCumilativeSub2[,c("FGA")],
                                                FG. = sum(TestSubPlayer[,c("FG.")]),#+DefensiveCumilativeSub2[,c("FG.")] ,
                                                X3P = sum(TestSubPlayer[,c("X3P")]),#+DefensiveCumilativeSub2[,c("X3P")],
                                                X3PA = sum(TestSubPlayer[,c("X3PA")]),#+DefensiveCumilativeSub2[,c("X3PA")],
                                                FT = sum(TestSubPlayer[,c("FT")]),#+DefensiveCumilativeSub2[,c("FT")],
                                                FTA = sum(TestSubPlayer[,c("FTA")]),#+DefensiveCumilativeSub2[,c("FTA")] ,
                                                ORB = sum(TestSubPlayer[,c("ORB")]),#,#+DefensiveCumilativeSub2[,c("ORB")],
                                                DRB = sum(TestSubPlayer[,c("DRB")]),#+DefensiveCumilativeSub2[,c("DRB")],
                                                TRB = sum(TestSubPlayer[,c("TRB")]),#+DefensiveCumilativeSub2[,c("TRB")],
                                                AST = sum(TestSubPlayer[,c("AST")]),#+DefensiveCumilativeSub2[,c("AST")],
                                                STL = sum( TestSubPlayer[,c("STL")] ),#+DefensiveCumilativeSub2[,c("STL")],
                                                BLK = sum( TestSubPlayer[,c("BLK")] ),#+DefensiveCumilativeSub2[,c("BLK")],
                                                TOV = sum(TestSubPlayer[,c("TOV")]),#+DefensiveCumilativeSub2[,c("TOV")],
                                                PF = sum(TestSubPlayer[,c("PF")]),#+DefensiveCumilativeSub2[,c("PF")],
                                                PTS = sum(TestSubPlayer[,c("PTS")]),#+DefensiveCumilativeSub2[,c("PTS")],
                                                GmSc = sum(TestSubPlayer[,c("GmSc")]),#+DefensiveCumilativeSub2[,c("GmSc")],
                                                plusMinus = sum(TestSubPlayer[,c("plusMinus")]),#+DefensiveCumilativeSub2[,c("plusMinus")],
                                                DFS = sum(TestSubPlayer[,c("DFS")]) #+DefensiveCumilativeSub2[,c("DFS")]
                                     ))
    DefensiveCumilativeSub2 = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% AllTeams[team], ]
    TestSubPlayer = subset(NewOffense, NewOffense$Tm == AllTeams[team])
  } 
  
}


DefensiveProgressiveNewAVG = DefensiveProgressiveNew[0,]
#### rdefensive aggresive
for (team in 1:length(AllTeams)) {
  TestSubPlayer = subset(DefensiveProgressiveNew, DefensiveProgressiveNew$Tm == AllTeams[team])
  
  
  if ( nrow(TestSubPlayer) > 3 ) {
    for (i in nrow(TestSubPlayer):3){
      
      DefensiveProgressiveNewAVG =  rbind(DefensiveProgressiveNewAVG, 
                                          data.frame(Tm = TestSubPlayer[1,c("Tm")],
                                                     G. = TestSubPlayer[i,c("G.")],
                                                     FG = mean(TestSubPlayer[i:1,c("FG")]), #+DefensiveCumilativeSub2[,c("FG")] ,
                                                     FGA = mean(TestSubPlayer[i:1,c("FGA")]),#+DefensiveCumilativeSub2[,c("FGA")],
                                                     FG. = mean(TestSubPlayer[i:1,c("FG.")]),#+DefensiveCumilativeSub2[,c("FG.")] ,
                                                     X3P = mean(TestSubPlayer[i:1,c("X3P")]),#+DefensiveCumilativeSub2[,c("X3P")],
                                                     X3PA = mean(TestSubPlayer[i:1,c("X3PA")]),#+DefensiveCumilativeSub2[,c("X3PA")],
                                                     FT = mean(TestSubPlayer[i:1,c("FT")]),#+DefensiveCumilativeSub2[,c("FT")],
                                                     FTA = mean(TestSubPlayer[i:1,c("FTA")]),#+DefensiveCumilativeSub2[,c("FTA")] ,
                                                     ORB = mean(TestSubPlayer[i:1,c("ORB")]),#+DefensiveCumilativeSub2[,c("ORB")],
                                                     DRB = mean(TestSubPlayer[i:1,c("DRB")]),#+DefensiveCumilativeSub2[,c("DRB")],
                                                     TRB = mean(TestSubPlayer[i:1,c("TRB")]),#+DefensiveCumilativeSub2[,c("TRB")],
                                                     AST = mean(TestSubPlayer[i:1,c("AST")]),#+DefensiveCumilativeSub2[,c("AST")],
                                                     STL = mean( TestSubPlayer[i:1,c("STL")] ),#+DefensiveCumilativeSub2[,c("STL")],
                                                     BLK = mean( TestSubPlayer[i:1,c("BLK")] ),#+DefensiveCumilativeSub2[,c("BLK")],
                                                     TOV = mean(TestSubPlayer[i:1,c("TOV")]),#+DefensiveCumilativeSub2[,c("TOV")],
                                                     PF = mean(TestSubPlayer[i:1,c("PF")]),#+DefensiveCumilativeSub2[,c("PF")],
                                                     PTS = mean(TestSubPlayer[i:1,c("PTS")]),#+DefensiveCumilativeSub2[,c("PTS")],
                                                     GmSc = mean(TestSubPlayer[i:1,c("GmSc")]),#+DefensiveCumilativeSub2[,c("GmSc")],
                                                     plusMinus = mean(TestSubPlayer[i:1,c("plusMinus")]),#+DefensiveCumilativeSub2[,c("plusMinus")],
                                                     DFS = mean(TestSubPlayer[i:1,c("DFS")]) #+DefensiveCumilativeSub2[,c("DFS")]
                                          ))
      
    }}    
  
  
}


DefensiveProgressiveNew=DefensiveProgressiveNewAVG




################################################################################################################
#############################################################################################################
#################################
###################################
PlayerProgressiveNew = NewOffense[0,]
AllPlayers = unique(NewOffense$playerId)

for (team in 1:length(AllPlayers)) {
  TestSubPlayer = subset(NewOffense, NewOffense$playerId == AllPlayers[team])
  # DefensiveCumilativeSub2 = subset(DefensiveProgressive,DefensiveProgressive$Tm == AllTeams[team])
  DefensiveCumilativeSub2 = PlayerProgressiveNew[PlayerProgressiveNew$Tm %in% AllTeams[team], ]
  # inital = TestSubPlayer[1,c("G.")]
  
  for ( games in 2:max(TestSubPlayer$G) ) {
    ### This game DFS score
    CurrentDFS = as.numeric( subset(TestSubPlayer, TestSubPlayer$G == games)$DFS )
    if(  length(CurrentDFS) == 0 ) CurrentDFS= 0.1
    TestSubPlayer = subset(TestSubPlayer, TestSubPlayer$G == games-1)
    DefensiveCumilativeSub2 = subset(DefensiveCumilativeSub2, DefensiveCumilativeSub2$G. == games-1)
    
    if (nrow(DefensiveCumilativeSub2) == 0){
      DefensiveCumilativeSub2[1,] = 0
      playerId = TestSubPlayer[1,c("playerId")]
    }
    
    #     if (nrow(DefensiveCumilativeSub2) != 0){
    #     playerId = DefensiveCumilativeSub2[1,c("Tm")]
    #     }
    
    PlayerProgressiveNew =  rbind(PlayerProgressiveNew, 
                                  data.frame(playerId =  playerId,
                                             G. = games,
                                             Date = (TestSubPlayer[1,c("Date")]),
                                             MP = sum(TestSubPlayer[,c("MP")]),
                                             FG = sum(TestSubPlayer[,c("FG")]), # +DefensiveCumilativeSub2[,c("FG")] ,
                                             FGA = sum(TestSubPlayer[,c("FGA")]),#+DefensiveCumilativeSub2[,c("FGA")],
                                             FG. = sum(TestSubPlayer[,c("FG.")]),#+DefensiveCumilativeSub2[,c("FG.")] ,
                                             X3P = sum(TestSubPlayer[,c("X3P")]),#+DefensiveCumilativeSub2[,c("X3P")],
                                             X3PA = sum(TestSubPlayer[,c("X3PA")]),#+DefensiveCumilativeSub2[,c("X3PA")],
                                             FT = sum(TestSubPlayer[,c("FT")]),#+DefensiveCumilativeSub2[,c("FT")],
                                             FTA = sum(TestSubPlayer[,c("FTA")]),#+DefensiveCumilativeSub2[,c("FTA")] ,
                                             ORB = sum(TestSubPlayer[,c("ORB")]),#+DefensiveCumilativeSub2[,c("ORB")],
                                             DRB = sum(TestSubPlayer[,c("DRB")]),#+DefensiveCumilativeSub2[,c("DRB")],
                                             TRB = sum(TestSubPlayer[,c("TRB")]),#+DefensiveCumilativeSub2[,c("TRB")],
                                             AST = sum(TestSubPlayer[,c("AST")]),#+DefensiveCumilativeSub2[,c("AST")],
                                             STL = sum( TestSubPlayer[,c("STL")] ),#+DefensiveCumilativeSub2[,c("STL")],
                                             BLK = sum( TestSubPlayer[,c("BLK")] ),#+DefensiveCumilativeSub2[,c("BLK")],
                                             TOV = sum(TestSubPlayer[,c("TOV")]),#+DefensiveCumilativeSub2[,c("TOV")],
                                             PF = sum(TestSubPlayer[,c("PF")]),#+DefensiveCumilativeSub2[,c("PF")],
                                             PTS = sum(TestSubPlayer[,c("PTS")]),#+DefensiveCumilativeSub2[,c("PTS")],
                                             GmSc = sum(TestSubPlayer[,c("GmSc")]),#+DefensiveCumilativeSub2[,c("GmSc")],
                                             plusMinus = sum(TestSubPlayer[,c("plusMinus")]),#+DefensiveCumilativeSub2[,c("plusMinus")],
                                             UsageRating = sum(TestSubPlayer[,c("UsageRating")]),
                                             DFS = (CurrentDFS)  ))
    
    
    DefensiveCumilativeSub2 = PlayerProgressiveNew[PlayerProgressiveNew$playerId %in% AllPlayers[team], ]
    TestSubPlayer = subset(NewOffense, NewOffense$playerId == AllPlayers[team])
  } 
  
}

PlayerProgressiveNew = subset(PlayerProgressiveNew, is.na(PlayerProgressiveNew$playerId) == FALSE )
############# Now that I have the stats build Model for each player. 

######################################
###################################### Avg player progressive over last 4 games ? ######
AllPlayers = unique(PlayerProgressiveNew$playerId)

PlayerProgressiveAveragesNew = PlayerProgressiveNew[0,]
for (each in 1:length(AllPlayers)) {
  TestSubPlayer = subset(PlayerProgressiveNew, PlayerProgressiveNew$playerId == AllPlayers[each] )
  # DefensiveCumilativeSub2 = subset(DefensiveProgressive,DefensiveProgressive$Tm == AllTeams[team])
  # DefensiveCumilativeSub2 = PlayerProgressive[PlayerProgressive$Tm %in% AllTeams[team], ]
  # inital = TestSubPlayer[1,c("G.")]
  playerId = AllPlayers[each]
  if ( nrow(TestSubPlayer) > 3 ) {
    for (i in (nrow(TestSubPlayer)):3){
      
      
      PlayerProgressiveAveragesNew =  rbind(PlayerProgressiveAveragesNew, 
                                            data.frame(playerId =  playerId,
                                                       G. = TestSubPlayer[i,c("G.")],
                                                       Date = (TestSubPlayer[i,c("Date")]),
                                                       MP = mean(TestSubPlayer[i:(i-2),c("MP")]),
                                                       
                                                       FG = mean(TestSubPlayer[i:(i-2),c("FG")]), # +DefensiveCumilativeSub2[,c("FG")] ,
                                                       FGA = mean(TestSubPlayer[i:(i-2),c("FGA")]),#+DefensiveCumilativeSub2[,c("FGA")],
                                                       FG. = mean(TestSubPlayer[i:(i-2),c("FG.")]),#+DefensiveCumilativeSub2[,c("FG.")] ,
                                                       X3P = mean(TestSubPlayer[i:(i-2),c("X3P")]),#+DefensiveCumilativeSub2[,c("X3P")],
                                                       X3PA = mean(TestSubPlayer[i:(i-2),c("X3PA")]),#+DefensiveCumilativeSub2[,c("X3PA")],
                                                       FT = mean(TestSubPlayer[i:(i-2),c("FT")]),#+DefensiveCumilativeSub2[,c("FT")],
                                                       FTA = mean(TestSubPlayer[i:(i-2),c("FTA")]),#+DefensiveCumilativeSub2[,c("FTA")] ,
                                                       ORB = mean(TestSubPlayer[i:(i-2),c("ORB")]),#+DefensiveCumilativeSub2[,c("ORB")],
                                                       DRB = mean(TestSubPlayer[i:(i-2),c("DRB")]),#+DefensiveCumilativeSub2[,c("DRB")],
                                                       TRB = mean(TestSubPlayer[i:(i-2),c("TRB")]),#+DefensiveCumilativeSub2[,c("TRB")],
                                                       AST = mean(TestSubPlayer[i:(i-2),c("AST")]),#+DefensiveCumilativeSub2[,c("AST")],
                                                       STL = mean( TestSubPlayer[i:(i-2),c("STL")] ),#+DefensiveCumilativeSub2[,c("STL")],
                                                       BLK = mean( TestSubPlayer[i:(i-2),c("BLK")] ),#+DefensiveCumilativeSub2[,c("BLK")],
                                                       TOV = mean(TestSubPlayer[i:(i-2),c("TOV")]),#+DefensiveCumilativeSub2[,c("TOV")],
                                                       PF = mean(TestSubPlayer[i:(i-2),c("PF")]),#+DefensiveCumilativeSub2[,c("PF")],
                                                       PTS = mean(TestSubPlayer[i:(i-2),c("PTS")]),#+DefensiveCumilativeSub2[,c("PTS")],
                                                       GmSc = mean(TestSubPlayer[i:(i-2),c("GmSc")]),#+DefensiveCumilativeSub2[,c("GmSc")],
                                                       plusMinus = mean(TestSubPlayer[i:(i-2),c("plusMinus")]),#+DefensiveCumilativeSub2[,c("plusMinus")],
                                                       UsageRating = mean(TestSubPlayer[i:(i-2),c("UsageRating")]),
                                                       DFS = (TestSubPlayer[i,c("DFS")])  ))
      
      
      
    }  
    
  }  
  
  
}

######################################
###################################### Avg player progressive over last 4 games ? ######


trainNew2017 = merge(PlayerProgressiveAveragesNew, New2017, by=c("playerId"))


PlayerCumilative = DefensiveCumilativeSub2[0,]
NewOffense = unique(NewOffense)
AllPlayers = unique(NewOffense$playerId)
TrainingDataNew = NewOffense[0,]
# rm(TrainingData)
#### Combined Alll Stats #####
######################

for ( each in 1:nrow(NewOffense)){
  if ( NewOffense[each,c("G")] > 3 ){
    TempOppTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$G. %in% NewOffense[each,c("G")], ]
    TempOppTeam = TempOppTeam[TempOppTeam$Tm %in% NewOffense[each,c("Opp")], ]
    
    TempMyTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$G. %in% NewOffense[each,c("G")], ]
    TempMyTeam = TempMyTeam[TempMyTeam$Tm %in% NewOffense[each,c("Tm")], ]
    
    Player =  trainNew2017[(trainNew2017$G.) %in% (NewOffense[each,c("G")] + 1), ]
    Player =  Player[Player$playerId %in% NewOffense[each,c("playerId")], ]
    Player$G. = Player$G. - 1
    if (  nrow(Player) == 0 ) {
      next
    }
    if (  Player$DFS == 0.10 ) {
      next
    }
    
    ##### Team opposing players ##########
    OppPlayers = trainNew2017[trainNew2017$NewTeam %in% NewOffense[each,c("Opp")],]
    
    t = sort( as.Date(unique(OppPlayers$Date)) )
    i = 2
    while ( as.Date(NewOffense[each,c("Date")]) > t[i]  && i < length(t)){
      i = i + 1
    }
    if ( i == length(t) )
    {
      i = i + 1
    }
    #### Find the last game played by the team before this match
    
    OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(t[i-1]), ]
    OppPlayers = OppPlayers[OppPlayers$Pos %in% Player[,c("Pos")], ]
    
    
    if ( nrow(OppPlayers) > 0 ){
      OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                 "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                 "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
      
      OppPlayers = aggregate( x = OppPlayers[,-c(1)], by = list(OppPlayers$Date) , FUN = sum )
      
      
      
    }
    
    if ( nrow(OppPlayers) == 0 ) {
      OppPlayers = trainNew2017[trainNew2017$NewTeam %in% NewOffense[each,c("Opp")],]
      t = sort( as.Date(unique(OppPlayers$Date)) )
      i = 2
      while ( as.Date(NewOffense[each,c("Date")]) > t[i]  && i < length(t)){
        i = i + 1
      }
      if ( i == length(t) )
      {
        i = i + 1
      }
      
      #### Find the last game played by the team before this match
      
      OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(t[i-1]), ]
      
      OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                 "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                 "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
      OppPlayers = aggregate( x = OppPlayers, by = list(OppPlayers$Date) , FUN = mean )
      
      OppPlayers = OppPlayers[,-c(2)]
    }
    
    OppPlayers[is.na(OppPlayers)] = 0
    cn = colnames(OppPlayers)
    cn <- sub("([A-Z]*)","\\1.OPP",cn)
    # cn
    colnames(OppPlayers) = cn
    
    TempTrainingData =merge(Player, TempMyTeam,by=c("G."))
    TempTrainingData =merge(TempTrainingData, TempOppTeam,by=c("G."))
    TempTrainingData = merge(TempTrainingData , OppPlayers )
    TrainingDataNew = rbind(TrainingDataNew, TempTrainingData )
  }
}



FinalTraining = ( TrainingDataNew)
# # PlayerPrediction
# Raw_Data2 = FinalTraining
# par(mfrow=c(1, 1))
# 
# 
# exclude_Metric  <- c("TRB.x","X3PA.x","PF.x","BLK.y","X3PA","ORB","FT","FTA","STL","TOV",
#                      "PF","AST","DRB","TRB","FGA","FG","PTS","FG.","DFS","GmSc","X3P.y",
#                      "X3PA.y","STL.y","AST.y","FG.y","GmSc.y","TOV.y","PF.y","DRB.y",
#                      "TRB.y","FGA.y","FG.y","PTS.y","DFS.y","ORB.y","FT.y","FTA.y","FTA.x",
#                      "GmSc.x","FGA.x","FG.x")
# 
# 
# Raw_Data2 <- Raw_Data2[, -which(names(Raw_Data2) %in% exclude_Metric)]
# 
# Refine_Data=Raw_Data2
# Refine_Data1 =Refine_Data
# trainM <- Refine_Data1
# # testM <- subset(Raw_Data2Test, Raw_Data2Test$G. == 21 )
# trainM[is.na(trainM)] <- 0

#############################################################33
#############################################################33
#############################################################33
####### number of dates
AllPlayers = unique(FinalTraining$playerId)
# AllPlayers = data.frame(unique(FinalTraining$playerId))

trainNew2017=FinalTraining[0,]
FinalTraining[,c("Rest")]=0
for ( each in 1:length(AllPlayers) )
{
  
  TA  = subset( FinalTraining, FinalTraining$playerId == AllPlayers[each] )
  TA = subset(TA, TA$G. > 0 )
  
  if ( nrow(TA) > 2 ){
    for ( i  in 2:nrow(TA) ){
      
      TA[i,c("Rest")] = as.numeric( as.Date( as.character(TA[i,c("Date")]) ) - as.Date( as.character(TA[i-1,c("Date")]) ) )
      
    }
  }
  
  trainNew2017 = rbind(trainNew2017,TA)
  
  
}
FinalTraining = trainNew2017
trainNew2017 = merge(trainNew2017, DKSal, by =c("Name") )


# trainNew2017 = subset(trainNew2017,trainNew2017$Date != '2016-12-15')
# trainNew2017 = subset(trainNew2017,trainNew2017$Date != '2016-12-14')

PlayerCumilativeTest = DefensiveCumilativeSub2[0,]
AllPlayers = unique(trainNew2017$playerId)

# # 
# T1 = 'ATL'
NewOffenseAverages = NewOffense[0,]
NewOffenseAverages[,c("Rest")]=0
AllPlayers = unique(NewOffense$playerId)

InjuriesPlayers = unique(Injuried[,1])
# 
# 
for ( i in 1:length(InjuriesPlayers))
{
  NewOffense = subset(NewOffense, NewOffense$playerName != as.character(InjuriesPlayers[i]) )
}



for (each in 1:length(AllPlayers)) {
  TestSubPlayer = subset(NewOffense, NewOffense$playerId == AllPlayers[each] )
  # DefensiveCumilativeSub2 = subset(DefensiveProgressive,DefensiveProgressive$Tm == AllTeams[team])
  # DefensiveCumilativeSub2 = PlayerProgressive[PlayerProgressive$Tm %in% AllTeams[team], ]
  # inital = TestSubPlayer[1,c("G.")]
  playerId = AllPlayers[each]
  if ( nrow(TestSubPlayer) > 3 ) {
    for (i in nrow(TestSubPlayer):3){
      
      
      NewOffenseAverages =  rbind(NewOffenseAverages, 
                                  data.frame(playerId =  playerId,
                                             playerName = TestSubPlayer[1,c("playerName")],
                                             G = TestSubPlayer[i,c("G")],
                                             Date = (TestSubPlayer[i,c("Date")]),
                                             Tm = (TestSubPlayer[i,c("Tm")]),
                                             Opp = (TestSubPlayer[i,c("Opp")]),
                                             GS = (TestSubPlayer[i,c("GS")]),
                                             MP = mean(TestSubPlayer[i:(i-2),c("MP")]),
                                             Rest= as.numeric( as.Date( as.character(TestSubPlayer[i,c("Date")]) ) - 
                                                                 as.Date( as.character(TestSubPlayer[i-1,c("Date")]) ) ),
                                             FG = mean(TestSubPlayer[i:(i-2),c("FG")]), # +DefensiveCumilativeSub2[,c("FG")] ,
                                             FGA = mean(TestSubPlayer[i:(i-2),c("FGA")]),#+DefensiveCumilativeSub2[,c("FGA")],
                                             FG. = mean(TestSubPlayer[i:(i-2),c("FG.")]),#+DefensiveCumilativeSub2[,c("FG.")] ,
                                             X3P = mean(TestSubPlayer[i:(i-2),c("X3P")]),#+DefensiveCumilativeSub2[,c("X3P")],
                                             X3PA = mean(TestSubPlayer[i:(i-2),c("X3PA")]),
                                             X3P. = mean(TestSubPlayer[i:(i-2),c("X3P.")]),#+DefensiveCumilativeSub2[,c("X3PA")],
                                             FT = mean(TestSubPlayer[i:(i-2),c("FT")]),#+DefensiveCumilativeSub2[,c("FT")],
                                             FTA = mean(TestSubPlayer[i:(i-2),c("FTA")]),#+DefensiveCumilativeSub2[,c("FTA")] ,
                                             FT. = mean(TestSubPlayer[i:(i-2),c("FT.")]),
                                             ORB = mean(TestSubPlayer[i:(i-2),c("ORB")]),#+DefensiveCumilativeSub2[,c("ORB")],
                                             DRB = mean(TestSubPlayer[i:(i-2),c("DRB")]),#+DefensiveCumilativeSub2[,c("DRB")],
                                             TRB = mean(TestSubPlayer[i:(i-2),c("TRB")]),#+DefensiveCumilativeSub2[,c("TRB")],
                                             AST = mean(TestSubPlayer[i:(i-2),c("AST")]),#+DefensiveCumilativeSub2[,c("AST")],
                                             STL = mean( TestSubPlayer[i:(i-2),c("STL")] ),#+DefensiveCumilativeSub2[,c("STL")],
                                             BLK = mean( TestSubPlayer[i:(i-2),c("BLK")] ),#+DefensiveCumilativeSub2[,c("BLK")],
                                             TOV = mean(TestSubPlayer[i:(i-2),c("TOV")]),#+DefensiveCumilativeSub2[,c("TOV")],
                                             PF = mean(TestSubPlayer[i:(i-2),c("PF")]),#+DefensiveCumilativeSub2[,c("PF")],
                                             PTS = mean(TestSubPlayer[i:(i-2),c("PTS")]),#+DefensiveCumilativeSub2[,c("PTS")],
                                             GmSc = mean(TestSubPlayer[i:(i-2),c("GmSc")]),#+DefensiveCumilativeSub2[,c("GmSc")],
                                             plusMinus = mean(TestSubPlayer[i:(i-2),c("plusMinus")]),#+DefensiveCumilativeSub2[,c("plusMinus")],
                                             UsageRating = mean(TestSubPlayer[i:(i-2),c("UsageRating")]),
                                             DFS = (TestSubPlayer[i,c("DFS")])  ))
      
      
      
    }  
    
  }  
  
  
}


NewOffenseAverages = merge(NewOffenseAverages, New2017, by=c("playerId"))


# T1 = 'CLE'
# T2 = 'NYK'



# rm(T1,T2)
TestDataFunc <- function(n1,n2) {
  TempNewOffense = NewOffenseAverages
  # TempNewOffense$G = TempNewOffense$G + 100
  TestData = trainNew2017[0,]
  T1 = n1
  T2 = n2
  T1Players= unique(trainNew2017[trainNew2017$NewTeam %in% T1,]$playerId)
  T2Players=unique(trainNew2017[trainNew2017$NewTeam %in% T2,]$playerId)
  
  
  for ( each in 1:length(T1Players) ){
    
    #   Gmax = NewOffense[NewOffense$playerId %in% T1Players[each], ]
    #   # myTm = Gmax[1,("Tm")]
    #   # oPPTm = Gmax[1,("Opp")]
    #   Gmax = max(Gmax$G)
    
    # if ( NewOffense[each,c("G")] > 1 ){
    TempOppTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% T2, ]
    Gmax = max(TempOppTeam$G.) -1
    TempOppTeam = TempOppTeam[TempOppTeam$G. %in% Gmax, ]
    
    #     
    
    TempMyTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% T1, ]
    Gmax = max(TempMyTeam$G.) -1
    TempMyTeam = TempMyTeam[TempMyTeam$G. %in% Gmax, ]
    
    
    
    
    
    # Player =  NewOffense[NewOffense$G %in% (Gmax), ]
    Player =  TempNewOffense[TempNewOffense$playerId %in% T1Players[each], ]
    if(nrow(Player) > 0) {
      Gmax = max(Player$G)
      Player =  Player[Player$G %in% (Gmax), ]
      # Player=rename(Player, c("G"="G."))
      # Player[,c("G.")] = max( Player[,c("G.")] ) - 1
      # Player$Opp = TempMyTeam$Tm
      # PID =Player$playerId
      # CurrentTeam = trainNew2017[trainNew2017$playerId %in% PID, ]$NewTeam[1]
      
      ##### Team opposing players ##########
      OppPlayers = TempNewOffense[TempNewOffense$Tm %in% T2,]
      Gmax = max ( as.Date(OppPlayers$Date) )
      OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(Gmax), ]
      
      # OppPlayers = OppPlayers[OppPlayers$Date %in% TempNewOffense[each,c("Date")], ]
      OppPlayers = OppPlayers[OppPlayers$Pos %in% Player[,c("Pos")], ]
      OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                 "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                 "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
      
      if ( nrow(OppPlayers) == 0 ) {
        OppPlayers = TempNewOffense[TempNewOffense$Tm %in% T2,]
        Gmax = max ( as.Date(OppPlayers$Date) )
        OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(Gmax), ]
        OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                   "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                   "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
        OppPlayers = aggregate( x = OppPlayers, by = list(OppPlayers$Date) , FUN = mean )
        OppPlayers = OppPlayers[,-c(2)]
      }
      else{
        OppPlayers = aggregate( x = OppPlayers[,-c(1)], by = list(OppPlayers$Date) , FUN = sum )
        
      }
      
      OppPlayers[is.na(OppPlayers)] = 0
      cn = colnames(OppPlayers)
      cn <- sub("([A-Z]*)","\\1.OPP",cn)
      # cn
      colnames(OppPlayers) = cn      
      
      
      
      
      Player$Tm = TempOppTeam$Tm
      TempTrainingData =merge(Player, TempOppTeam,by=c("Tm"))
      # TempTrainingData$G = TempMyTeam$G.
      TempTrainingData$Tm = TempMyTeam$Tm
      TempTrainingData =merge(TempTrainingData, TempMyTeam,by=c("Tm"))
      TempTrainingData = merge(TempTrainingData , OppPlayers )
      
      TestData = rbind(TestData, TempTrainingData )
      
      # }
    }
    # return (TestData)
  }
  
  
  for ( each in 1:length(T2Players) ){
    
    #     Gmax = NewOffense[NewOffense$playerId %in% T2Players[each], ]
    #     # myTm = Gmax[1,("Tm")]
    #     # oPPTm = Gmax[1,("Opp")]
    #     Gmax = max(Gmax$G)
    #     
    # if ( NewOffense[each,c("G")] > 1 ){
    TempOppTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% T1, ]
    Gmax = max(TempOppTeam$G.) -1
    TempOppTeam = TempOppTeam[TempOppTeam$G. %in% Gmax, ]
    
    #     
    
    TempMyTeam = DefensiveProgressiveNew[DefensiveProgressiveNew$Tm %in% T2, ]
    Gmax = max(TempMyTeam$G.) -1
    TempMyTeam = TempMyTeam[TempMyTeam$G. %in% Gmax, ]
    
    
    #     
    Player =  TempNewOffense[TempNewOffense$playerId %in% T2Players[each], ]
    if( nrow(Player) > 0 ) {
      Gmax = max(Player$G)
      Player =  Player[Player$G %in% (Gmax), ]
      
      # Player=rename(Player, c("G"="G."))
      # Player[,c("G.")] = max( Player[,c("G.")] ) - 1
      # PID =Player$playerId
      # CurrentTeam = trainNew2017[trainNew2017$playerId %in% PID, ]$NewTeam[1]
      
      ##### Team opposing players ##########
      OppPlayers = TempNewOffense[TempNewOffense$Tm %in% T1,]
      Gmax = max ( as.Date(OppPlayers$Date) )
      OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(Gmax), ]
      
      # OppPlayers = OppPlayers[OppPlayers$Date %in% TempNewOffense[each,c("Date")], ]
      OppPlayers = OppPlayers[OppPlayers$Pos %in% Player[,c("Pos")], ]
      OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                 "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                 "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
      
      if ( nrow(OppPlayers) == 0 ) {
        OppPlayers = TempNewOffense[TempNewOffense$Tm %in% T1,]
        Gmax = max ( as.Date(OppPlayers$Date) )
        OppPlayers = OppPlayers[OppPlayers$Date %in% as.factor(Gmax), ]       
        OppPlayers = OppPlayers[,c("Date"   ,   "MP"     ,   "FG"     ,   "FGA"    ,   "FG."    ,   "X3P"    ,   "X3PA"   ,   "FT"    ,    "FTA"    ,   
                                   "ORB"    ,   "DRB"     ,  "TRB"      , "AST"      ,
                                   "STL"    ,   "BLK"  ,     "TOV"    ,   "PF"   ,     "PTS"    ,   "GmSc" ,     "plusMinus")]
        OppPlayers = aggregate( x = OppPlayers, by = list(OppPlayers$Date) , FUN = mean )
        OppPlayers = OppPlayers[,-c(2)]
        
      }
      else{
        OppPlayers = aggregate( x = OppPlayers[,-c(1)], by = list(OppPlayers$Date) , FUN = sum )
        
      }
      
      OppPlayers[is.na(OppPlayers)] = 0
      cn = colnames(OppPlayers)
      cn <- sub("([A-Z]*)","\\1.OPP",cn)
      # cn
      colnames(OppPlayers) = cn      
      
      
      
      Player$Tm = TempOppTeam$Tm
      TempTrainingData =merge(Player, TempOppTeam,by=c("Tm"))
      # TempTrainingData$G = TempMyTeam$G.
      TempTrainingData$Tm = TempMyTeam$Tm
      TempTrainingData =merge(TempTrainingData, TempMyTeam,by=c("Tm"))
      TempTrainingData = merge(TempTrainingData , OppPlayers )
      
      TestData = rbind(TestData, TempTrainingData )
      # return (TestData)
      
    }
  }
  
  
  return (TestData)
  
}

FinalTest=NewOffense[0,]



## CHa - >CHO
# SA - SAS
# CHA  - CHO
# CHI = CHI
# BKN -> BRK
# NYK -> NYK
### Exlcude late players
# NewOffenseAverages = subset(NewOffenseAverages, NewOffenseAverages$playerId != 'derozde01')


# NOV8th
FinalTest=rbind(FinalTest, TestDataFunc('BRK','WAS') )
FinalTest=rbind(FinalTest, TestDataFunc('CLE','CHO') )

FinalTest=rbind(FinalTest, TestDataFunc('DEN','IND') )
FinalTest=rbind(FinalTest, TestDataFunc('DET','ORL') )
 

FinalTest=rbind(FinalTest, TestDataFunc('PHO','BOS') )
FinalTest=rbind(FinalTest, TestDataFunc('ATL','MIL') )
# 
# # 
FinalTest=rbind(FinalTest, TestDataFunc('NOP','HOU') )
FinalTest=rbind(FinalTest, TestDataFunc('PHI','CHI') )
# # 
# FinalTest=rbind(FinalTest, TestDataFunc('HOU','SAS') )
# FinalTest=rbind(FinalTest, TestDataFunc('NOP','UTA') )
# # 
# FinalTest=rbind(FinalTest, TestDataFunc('SAC','DEN') )
# FinalTest=rbind(FinalTest, TestDataFunc('BOS','LAC') )




##### Combine Old and New
# FinalTraining = trainNew2017
# AlloldDate = rbind(rbind(Offense2014, Offense2015), Offense2016)
FinalTraining = ColnamesTraining(FinalTraining)
# trainNew2017 = rbind(AlloldDate,FinalTraining )

# trainNew2017=trainNew2017[0,]
trainNew2017 = merge(FinalTraining, DKSal, by =c("Name") )


FinalTest = ColnamesTEST(FinalTest)

FinalTest = merge(FinalTest, DKSal, by =c("Name") )


save.image(file="TempFile.RData")


# PlayerPrediction
Raw_Data2 = trainNew2017
par(mfrow=c(1, 1))


exclude_Metric  <- c("TRB.x","X3PA.x","PF.x","BLK.y","X3PA","ORB","FT","FTA","STL","TOV",
                     "PF","AST","DRB","TRB","FGA","FG","FG.","DFS","GmSc","X3P.y",
                     "X3PA.y","STL.y","AST.y","FG.y","GmSc.y","TOV.y","PF.y","DRB.y",
                     "TRB.y","FGA.y","FG.y","DFS.y","ORB.y","FT.y","FTA.y","FTA.x",
                     "GmSc.x","FGA.x","FG.x","AvgPointsPerGame","GameInfo","teamAbbrev",
                     "NewTeam")


Raw_Data2 <- Raw_Data2[, -which(names(Raw_Data2) %in% exclude_Metric)]

Refine_Data=Raw_Data2
Refine_Data1 =Refine_Data
trainM <- Refine_Data1
# testM <- subset(Raw_Data2Test, Raw_Data2Test$G. == 21 )
trainM[is.na(trainM)] <- 0

####################################TESTINGGGGG
FinalTest = unique(FinalTest)
Raw_Data2 = FinalTest
par(mfrow=c(1, 1))


exclude_Metric  <- c("TRB.x","X3PA.x","PF.x","BLK.y","X3PA","ORB","FT","FTA","STL","TOV",
                     "PF","AST","DRB","TRB","FGA","FG","FG.","DFS","GmSc","X3P.y",
                     "X3PA.y","STL.y","AST.y","FG.y","GmSc.y","TOV.y","PF.y","DRB.y",
                     "TRB.y","FGA.y","FG.y","DFS.y","ORB.y","FT.y","FTA.y","FTA.x",
                     "GmSc.x","FGA.x","FG.x","AvgPointsPerGame","GameInfo","teamAbbrev",
                     "NewTeam","Position","Rk","Date","position","Age","X.nbsp.",
                     "Opp","X.nbsp.RP1","GS")


Raw_Data2 <- Raw_Data2[, -which(names(Raw_Data2) %in% exclude_Metric)]

Refine_Data=Raw_Data2
Refine_Data1 =Refine_Data
Test <- Refine_Data1
# testM <- subset(Raw_Data2Test, Raw_Data2Test$G. == 21 )
Test[is.na(Test)] <- 0

# TestMat = as.matrix(Test[,-c(1,2,3,22,23,33,52,27,32,19)] )
# head(Test)
# head(TestMat)
# library(Hmisc)
# SpearManCorrelation=varclus((TestMat), similarity = "spearman")
# 
# plot(SpearManCorrelation)
# abline(h=0.3)

# redun( DFS.x ~ FG..x + X3P.x + FT.x+   ORB.x+ DRB.x+AST.x +STL.x +
#          BLK.x +TOV.x +  PTS.y + MP +  Rest+  UsageRating+
#          FG..y + X3P+BLK + TRB.OPP + AST.OPP + STL.OPP+ BLK.OPP+TOV.OPP+PTS.OPP
#        ,data= trainM,  type = "adjusted" )

# red
##########################

# abline(h = .15, col = "blue", lty="dotted")

# VerifainM, ication <- data.frame( Name = factor(),Prestige = numeric(), brnn =numeric(),
#                             ctree = numeric(), df =numeric(),
#                             pen=numeric(),position= factor(), DFS=factor(), salary =factor(),facing=factor())

Verification1 <- data.frame( playerid = factor(),Forest = numeric(),ForestMAX = numeric(), brnn =numeric(),
                             brnnMAX =numeric(),
                             team = factor(),
                             H20 =numeric(),H20MAX =numeric(),position = factor(),xgboost = numeric(),
                             xgboostMaX = numeric(),
                             salary =factor(),UsageRating=factor(),
                             Mp= numeric() )

# H20MAX = max(FV6),
# xgboostMaX


trainM = subset(trainM,trainM$playerId != 'hillida01')
trainM = subset(trainM,trainM$playerId != 'napiesh01')
trainM = subset(trainM,trainM$playerId != 'bennean01')
trainM = subset(trainM, trainM$playerId !=  'harrijo01')
trainM = subset(trainM, trainM$playerId !=  'hieldbu01')
trainM = subset(trainM, trainM$playerId !=  'harrian01')
trainM = subset(trainM, trainM$playerId !=  'ennisty01')

# ## test Date  ennisty01
# trainM = subset(trainM,trainM$Date != '2016-12-16')
# trainM = subset(trainM,trainM$Date != '2016-12-15')
# trainM = subset(trainM,trainM$Date != '2016-12-14')

AllPlayers = unique(trainM$playerId)

# AllPlayers = unique(train$playerId)
c= as.data.frame(AllPlayers)
# 
# train = merge(train, New2017, by=c("playerId"))
# train = merge(train, DKSal, by =c("Name") )
# 
library(evtree)
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1.1))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

library(xgboost)
library(Matrix)
library(stringdist)
# 196 to left over
for(iPP in 1:length(AllPlayers) )
{
  # i= 17
  train = subset(trainM, trainM$playerId ==AllPlayers[iPP] )
  # test = subset(testM, testM$Tm.x == AllTeams[i])
  test = Test[Test$playerId %in% AllPlayers[iPP], ]
  # train[,c("TRB P")] = ( train$`ORB P`+  train$`DRB P` )
  #   trainpen=data.matrix(train)
  #   testpen=data.matrix(test)
  train[is.na(train)] = 0
  print(iPP)
  train = unique(train)
  test = unique(test)
  # splits <- splitdf(train, seed=808)
  # 
  # train = splits$trainset
  # test2 = splits$testset
  
  # TD = subset(train, train$playerId == "davisan02")
  # 
  # test$MP = 20
  # test$UsageRating = 20


  if (nrow(train) > 25 && nrow(test)>0 )
  {
    #   FF=cv.rq.pen(trainpen[,-c(1,2,3,18)],trainpen[,c(4)],tau=.5,lambda=NULL,
    #                weights=NULL,penalty="MCP",criteria="PBIC",cvFunc="check",nfolds=5,
    #                foldid=NULL,nlambda=100,eps=.0001,init.lambda=1)
    ### remove DFS
    # test = test[,-c(14)]
    TrainingH20= as.h2o(train)
    splits <- h2o.splitFrame(TrainingH20, c(0.85),  seed=1234)
    
    
    trainDNN  <- h2o.assign(splits[[1]], "train.hex") # 60%
    validDNN   <- h2o.assign(splits[[2]], "valid.hex") # 60%
    
    trainSparceMatrix = sparse.model.matrix( train$`DFS P` ~ train$`FT P` + train$`TRB P` +
                                              train$`STL P`  + train$`AST P` + train$`BLK P`+
                                               train$`UsageRating` + train$`MP` +  train$`Rest`  +
                                               train$`X3P OT` + train$`BLK OT` +  train$`TRB OPP` + train$`AST OPP`
                                             + train$`STL OPP` + train$`BLK OPP` + train$`TOV OPP`
                                             + train$`PTS OPP` )
    
    testSparseMatrix= sparse.model.matrix( test$`DFS P` ~  test$`FT P` + test$`TRB P`
                                           + test$`STL P`  + test$`AST P` + test$`BLK P`
                                           + test$`UsageRating` + test$`MP` +  test$`Rest`  +
                                             test$`X3P OT` + test$`BLK OT` +  test$`TRB OPP` + test$`AST OPP`
                                           + test$`STL OPP` + test$`BLK OPP` + test$`TOV OPP`
                                           + test$`PTS OPP`  )
    
    # test$`X3P P` + test$`FT P` + test$`TRB P`
    # + test$`STL P`  + test$`AST P` + test$`BLK P`
    # + test$`TOV P` +
    # TD2 = subset(test, test$playerId == "davisan02")
    TrainingH202= as.h2o(test)
    splits2 <- h2o.splitFrame(TrainingH202,  seed=1234)
    testDNN <- h2o.assign(splits2[[1]], "test.hex")  # 20%
    
    response <- "DFS P"
    
    predictors <- c("FG P","X3P P","FT P","ORB P" ,"DRB P","AST P" ,"STL P" ,
                    "BLK P" ,"TOV P",  "MP" ,    
                    "FG MT" , "X3P OT","BLK OT","Rest","UsageRating","TRB OPP","AST OPP",
                    "STL OPP","BLK OPP","TOV OPP","PTS OPP")
    
    sixX = test[1,c("Salary")]*6/1000
    
    FV6 = 0
    FV2 = 0
    FV5 = 0
    FV4 = 0
    FV3 = 0
    
    for(j in 1:1)
    {
      
      i = 1
      while( i < 5 ) {
        m1 <- h2o.deeplearning(
          model_id="dl_model_first", 
          training_frame=trainDNN, 
          validation_frame=validDNN,   ## validation dataset: used for scoring and early stopping
          x=predictors,
          y=response,
          #activation="Rectifier",  ## default
          hidden=c(100,100,100,100,100),       ## default: 2 hidden layers with 200 neurons each
          epochs=1,
          variable_importances=T    ## not enabled by default
        )
        
        FV6 = c(FV6, as.numeric(h2o.predict(m1,newdata=testDNN)[1,1]) )
        i = i + 1
      }
      
      
      # medRF  = -2
      # count = 0
      # i = 0
      # j = 1
      # medRFF =c()
      # 
      # "FG P","X3P P","FT P" , "ORB P" , "DRB P" ,  "AST P" 
      # , "STL P" , "BLK P" , "TOV P" ,
      
      # test$`FT P` + test$`TRB P`
      # + test$`STL P`  + test$`AST P` + test$`BLK P`
      # A <- vector("list",14)
      # 
      # while( medRF > (-10 + i) ) {
        
        rf = randomForest( train[,c("FT P" , "TRB P" ,  "AST P" 
                                    , "STL P" , "BLK P" , "UsageRating",
                                      "MP", "Rest" , "FG MT" ,"X3P OT","BLK OT"
                                      ,"TRB OPP","AST OPP","STL OPP", "BLK OPP","TOV OPP", "PTS OPP")], 
                          y = train[,c("DFS P")], ntree=1000
                               ,type='regression')
        tempFrame = randomForest::importance(rf)
        rNames = c( row.names(( tempFrame) ) )
        tempFrame = data.frame(rNames , c(tempFrame)  )
        tempFrame = tempFrame[with(tempFrame, order(-c.tempFrame.) ),]
        # tempFrame[with(tempFrame,order(-Weight)  ) ]
        name = file.path("C:","NBA_Prediction", 
                         paste("PlayerPlots/RRF-", train[1,1],"-importance",".jpg" , sep="") )
        jpeg(filename = name , antialias = "cleartype", quality = 500 ) 
        
        ( varImpPlot(rf) )
        
        dev.off()
        
        plotFunction = function( A, M , coln ){
          print(coln)
          for ( k in 1:length(colnames(test)) ){
            
            if ( stringdist( ( as.character( coln ) ),
                             as.character( colnames(test)[k] ) ) == 0 )
              
            { break }
            
          }
          
          TempDF=train[0,]
          # print(A) train$`DFS P` , train[,c(i)]
          TempDF = rbind(TempDF, data.frame( Actual= A  ,
                                             predicted = M ) ) 
          
          plotted=  ggplot(data = TempDF, aes(x=Actual, y=predicted ) ) + geom_point() + geom_line( aes(
            y = test[1,k] ), color="red" ) + labs( y =  ( as.character( coln ) ) ) + 
            geom_line( aes(x= sixX), color="green" )
          
          return(plotted)
        }   
        
        # for ( i in 1:nrow(tempFrame) ){
        #   if ( as.character (tempFrame[i,1]) == "train$Rest" ){
        #     tempFrame[i,1] = "train$`Rest`"
        #   }
        #   if ( as.character (tempFrame[i,1]) == "train$UsageRating" ){
        #     tempFrame[i,1] = "train$`UsageRating`"
        #   }
        #   if ( as.character (tempFrame[i,1]) == "train$MP" ){
        #     tempFrame[i,1] = "train$`MP`"
        #   }
        # }
        
        ########### Plot for RandomForest
        for ( i in 1:length(colnames(train)) ){
          
          
          
          if ( stringdist( ( as.character( tempFrame[1,1] ) ) ,
                           as.character( colnames(train)[i] ) ) == 0 )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , tempFrame[1,1] ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          }        
          
          if ( stringdist( ( as.character( tempFrame[2,1] ) ) ,
                           as.character( colnames(train)[i] ) ) == 0 )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , tempFrame[2,1] ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          }   
          
          if ( stringdist( ( as.character( tempFrame[3,1] ) ) ,
                           as.character( colnames(train)[i] ) ) == 0 )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , tempFrame[3,1] ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          }    
          
          if ( stringdist( ( as.character( tempFrame[4,1] ) ) ,
                           as.character( colnames(train)[i] ) ) == 0 )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , tempFrame[4,1] ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          }   
          
          
          if ( as.character( colnames(train)[i] ) == "train$`MP`" )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , "train$`MP`"  ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          }      
          
          if ( as.character( colnames(train)[i] ) == "train$`UsageRating`" )
          {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] , "train$`UsageRating`"  ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
          } 
          
          
          
        }
        
        
        
        
        
        
        
        
        # tempFrame = tempFrame[with(tempFrame,order(-IncNodePurity)  ) ]
        # tempFrame = subset(tempFrame, tempFrame$Feature != "(Intercept)")      
        
      #   count = count + 1
      #   medRF = median( predict(A[[j]], newdata = test2) -  test2$DFS.x )
      #   
      #   medRFF[j] = medRF
      #   j = j+ 1
      #   if (count> 10){
      #     print (i)
      #     i = i  + 0.5
      #     count = 0
      #     j = 1
      #   }
      #   
      # }
      # A = A[[which.max(medRFF)]]
      # 
      
      
      # train = train[1:100,]
      NeuralNet <- function(train) {
        
        return ( 
          brnn( x= data.matrix( train[,c("FT P" , "TRB P" ,  "AST P" 
                                         , "STL P" , "BLK P" , "UsageRating",  
                        "MP", "Rest" , "FG MT" ,"X3P OT","BLK OT"
                        ,"TRB OPP","AST OPP","STL OPP", "BLK OPP","TOV OPP", "PTS OPP")]  ), y = train[,c("DFS P")]
                
                ,neurons=15,epochs=100,cores=4 ) 
          
        )
        
        #     if( runif(1) < .5 ) stop()
        #     return(1)
        
        # "FG P","X3P P","FT P" , "ORB P" , "DRB P" ,  "AST P" 
        # , "STL P" , "BLK P" , "TOV P" ,
        
      }

      nn <- NULL
      attempt <- 1
      while( is.null(nn) && attempt <= 300 ) {
        attempt <- attempt + 1
        try(
          nn <- NeuralNet(train)
        )
      } 
      
      
      # nrow(test2)
      # medEV  = -2
      # count = 0
      # i = 0
      # j = 1
      # train$`X3P P` + train$`FT P` + train$`ORB P` 
      # + train$`DRB P`  + train$`STL P` + train$`BLK P`
      # + train$`TOV P` +
      # medEVV =c()
      # evtreea <- vector("list",14)      
      # 
      # while( medEV > ( -10 +  i) ) {
      trainSparceMatrix = sparse.model.matrix( train$`DFS P` ~ train$`FT P`+ train$`TRB P` + train$`AST P`
                                               + train$`STL P` + train$`BLK P`+
                                               train$`UsageRating` + train$`MP` +  train$`Rest`  +
                                                 train$`X3P OT` + train$`BLK OT` +  train$`TRB OPP` + train$`AST OPP`
                                               + train$`STL OPP` + train$`BLK OPP` + train$`TOV OPP`
                                               + train$`PTS OPP` )
        
      Labels = Matrix(train$`DFS P`, sparse = TRUE)
      
      dtrain <- xgb.DMatrix(data = trainSparceMatrix, label=Labels)
      
      xgbO = xgboost(data = dtrain ,booster = "gblinear" , eta = 0.1 , max_depth=15, nthread = 3,
                     nrounds=400,objective = "reg:linear" , verbose = 1 )
      # Boost-", train[1,1],"-" , colnames(train)[i],".jpg" 
      
      name = file.path("C:","NBA_Prediction", 
                       paste("PlayerPlots/Boost-", train[1,1],"-importance",".jpg" , sep="") )
      jpeg(filename = name , antialias = "cleartype", quality = 500 ) 
      
      ( xgb.plot.importance( xgb.importance(colnames(trainSparceMatrix),model = xgbO) ) )

      dev.off()
      
      
      tempFrame = xgb.importance(colnames(trainSparceMatrix),model = xgbO)
      tempFrame = tempFrame[with(tempFrame,order(-Weight)  ) ]
      tempFrame = subset(tempFrame, tempFrame$Feature != "(Intercept)")
       
      plotFunction = function( A, M , coln ){
        print(coln)
        for ( k in 1:length(colnames(test)) ){
          
          if ( stringdist( strsplit( as.character( coln ) , "`" )[[1]][2] ,
                           as.character( colnames(test)[k] ) ) == 0 )
            
          { break }
          
        }
        
        TempDF=train[0,]
        # print(A) train$`DFS P` , train[,c(i)] ,tempFrame[nrow(tempFrame)-1,1]
        TempDF = rbind(TempDF, data.frame( Actual= A  ,
                                           predicted = M ) ) 
        
        plotted=  ggplot(data = TempDF, aes(x=Actual, y=predicted ) ) + geom_point() + geom_line( aes(
          y = test[1,k] ), color="red" ) + labs( y =  strsplit( as.character( coln ) , "`" )[[1]][2] ) + 
          geom_line( aes(x= sixX), color="green" )
        
        return(plotted)
      }  
      
      for ( i in 1:nrow(tempFrame) ){
        if ( as.character (tempFrame[i,1]) == "train$Rest" ){
          tempFrame[i,1] = "train$`Rest`"
        }
        if ( as.character (tempFrame[i,1]) == "train$UsageRating" ){
          tempFrame[i,1] = "train$`UsageRating`"
        }
        if ( as.character (tempFrame[i,1]) == "train$MP" ){
          tempFrame[i,1] = "train$`MP`"
        }
      }
      ########## Plot IMportant metrics against actual
      for ( i in 1:length(colnames(train)) ){
        


        
        if ( stringdist( strsplit( as.character(tempFrame[1,1]) , "`" )[[1]][2] ,
                         as.character( colnames(train)[i] ) ) == 0 )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[1,1] ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }        
        
        
        if ( stringdist( strsplit( as.character(tempFrame[2,1]) , "`" )[[1]][2] ,
                        as.character( colnames(train)[i] ) ) == 0 )
        {
            plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[2,1] ) 
            name = file.path("C:","NBA_Prediction", 
                             paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
            jpeg(filename = name)      
            plot(plotted)
            dev.off()
        }
        
        if ( stringdist( strsplit( as.character(tempFrame[3,1]) , "`" )[[1]][2] ,
                         as.character( colnames(train)[i] ) ) == 0 )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[3,1] ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }
        
        if ( stringdist( strsplit( as.character(tempFrame[4,1]) , "`" )[[1]][2] ,
                         as.character( colnames(train)[i] ) ) == 0 )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[4,1] ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }        
        
        
        if ( stringdist( strsplit( as.character(tempFrame[nrow(tempFrame),1]) , "`" )[[1]][2] ,
                         as.character( colnames(train)[i] ) ) == 0 )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[nrow(tempFrame),1] ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }        
        
        
        if ( stringdist( strsplit( as.character(tempFrame[nrow(tempFrame)-1,1]) , "`" )[[1]][2] ,
                         as.character( colnames(train)[i] ) ) == 0 )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] ,tempFrame[nrow(tempFrame)-1,1] ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/Boost-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }        
        
        if ( as.character( colnames(train)[i] ) == "train$`MP`" )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] , "train$`MP`"  ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        }      
        
        if ( as.character( colnames(train)[i] ) == "train$`UsageRating`" )
        {
          plotted = plotFunction( train$`DFS P` , train[,c(i)] , "train$`UsageRating`"  ) 
          name = file.path("C:","NBA_Prediction", 
                           paste("PlayerPlots/RRF-", train[1,1],"-" , colnames(train)[i],".jpg" , sep="") )
          jpeg(filename = name)      
          plot(plotted)
          dev.off()
        } 
        
        
        
      }
      
      
      

      if (nrow(test) > 0 && (!is.null(nn)) ) {                      
        # FV6= c(FV6, predict(prestige.fit , test[,-c(19)] ) )
        
        FV2= c(FV2, predict( rf,  test[,c( "FT P" , "TRB P" ,  "AST P" 
                                           , "STL P" , "BLK P" , "UsageRating",  
                                         "MP", "Rest" , "FG MT" ,"X3P OT","BLK OT"
                                         ,"TRB OPP","AST OPP","STL OPP", "BLK OPP","TOV OPP", "PTS OPP")]
                            ,type = c("response") )
               )
        
        FV5= c(FV5 , predict(xgbO, testSparseMatrix)    )
        
        FV4=c(FV4,       predict(nn,newdata =data.matrix( test[,c("FT P" , "TRB P" ,  "AST P" 
                                                                  , "STL P" , "BLK P" , "UsageRating",  
                                                          "MP", "Rest" , "FG MT" ,"X3P OT","BLK OT"
                                                        ,"TRB OPP","AST OPP","STL OPP", "BLK OPP","TOV OPP", "PTS OPP")] )  
                                 ,type = c("response") )
              )
        
        # FV3 = c(FV3, as.numeric(h2o.predict(best_model,newdata=testDNN)[1,1]) )
        
      }
      
      
    }      
    
    
    
    Verification1 =rbind(Verification1, data.frame(playerid = as.character( test[1,c("playerName")]) ,
                                                   team = as.character(test[1,c("Tm")]) ,
                                                   # Prestige = median(FV6[2:length(FV6)]) ,
                                                   brnn = median(FV4[2:length(FV4)]),
                                                   Forest = median(FV2[2:length(FV2)]),
                                                   xgboost = median(FV5[2:length(FV5)]),
                                                   H20=median(FV6[2:length(FV6)]), 
                                                   # DNNBest = median(FV3[2:length(FV3)]),
                                                   
                                                   brnnMAX = max(FV4),
                                                   ForestMAX = max(FV2),
                                                   H20MAX = max(FV6),
                                                   xgboostMaX = max(FV5),
                                                   # medRFF = medRF,
                                                   # medEVF = medEV,
                                                   
                                                   position = train[1,c("Position")],
                                                   salary = test[1,c("Salary")],
                                                   UsageRating =test[1,c("UsageRating")],
                                                   Mp = test[1,c("MP")]
    ) )
    
    
    
  }    
  
  
  
}


# Verification1=Verification1[!duplicated(Verification1[,1]),]

# Verification1[,c("PrestigePer")] = Verification1[,c("Prestige")]*100/Verification1[,c("salary")]
Verification1[,c("medPer")] = Verification1[,c("brnnMAX")]*100/Verification1[,c("salary")]
Verification1[,c("ForestPer")] = Verification1[,c("ForestMAX")]*100/Verification1[,c("salary")]
# Verification1[,c("evtreePer")] = Verification1[,c("evtree")]*100/Verification1[,c("salary")]
# 
Verification1[,c("xgboostPer")] = Verification1[,c("xgboostMaX")]*100/Verification1[,c("salary")]
Verification1[,c("h2oPer")] = Verification1[,c("H20MAX")]*100/Verification1[,c("salary")]

Verification1[,c("6x")] = Verification1[,c("salary")]*6/1000
# Verification1[,c("DiffAvg")] = Verification1[,c("6x")]-Verification1[,c("DNN")]
# Verification1[,c("DiffBest")] = Verification1[,c("6x")]-Verification1[,c("DNNBest")]

# Verification1[,c("Difference")] = Verification1[,c("6x")] - median( Verification1[,c("Prestige")] )
Verification2 = data.frame( Verification1[,c("playerid","team","brnnMAX","ForestMAX",
                                             "H20MAX","xgboostMaX","position","salary","UsageRating","Mp","6x")] )

# Verification1[]
write.csv(Verification1, file = "NBA_Mar24_NewXGB.csv")
write.csv(Verification2, file="c:/Users/suhas/Documents/Website/web-dev-template-master/Website/data file/Test.csv")
# mongoimport -h ds157509.mlab.com:57509 -d testdatabase_suhas -c predictionmodels -u SuhasKabinna -p ilovepanda123 --file Test.csv --type csv --headerline

  