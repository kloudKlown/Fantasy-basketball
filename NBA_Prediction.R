############ Load Library
library(Hmisc)
library(corrplot)
library(brnn)
library(h2o)
library(randomForest)
library(Matrix)
library(xgboost)
library(stringdist)

###########################################################
localH2O <- h2o.init()
All_16_Combined = read.csv('All_16_Combined.csv')
All_17 = read.csv('All_NBA_17.csv', header = FALSE)
TT = read.csv('All_NBA_17_Today.csv', header = FALSE)
Position = read.csv('PlayerPosition_17.csv')
Dvoa = read.csv('NewOffense_Combined_17.csv')
DateCheck = "2018-01-25"

####################### 16 part

# colnames(All_16) = c("Date","B1","B2","Rating","Player","Pos","Salary","Min","Max","Team",
#                      "Opp","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg Proj","Min Proj",
#                      "Own1","Own2","Imp Pts","Act Pts","FP_Min","PER",
#                      "Usage","Pro","My","Bargain","Opp_Plus_Minus","PaceD","Refs","Ref1","Ref2","TS_Per","Fouls_36",
#                      "Points_Touch","Touches","B2B","Rest","Pts","Opp Pts","delta","Spread",
#                      "O_U","Spread_per","PPG","Change","Consistency",
#                      "Upside","Duds","Count","YPPG","YPlus_Minus","YChange","YConsistency","YUpside","YDuds","YCount")
# 
# All_16[All_16 == '&nbsp;'] <- 0
# All_16["B5"] = 0
# All_16["S_B"] = 0
# All_16["PF_Min-M"] = 0
# All_16$Date =  as.Date(All_16$Date, "%m/%d/%Y")
# All_16$Opp =(gsub('[@,]', '', All_16$Opp))
# 
# 
# 
# All_16[] <- lapply(All_16, as.character)
# #### Because there are naming differences make everything uniform
# for(each in 1:nrow(All_16)){
#   
#   
#   N  = amatch(as.character(All_16[each,"Player"] ), Dvoa$playerName , maxDist = 5)
#   i = 0
#   
#   while(is.na(N)){
#     N  = amatch(as.character(All_16[each,"Player"] ),  Dvoa$playerName , maxDist = (5 + i))
#     i = i + 1
#     if (i > 5){
#       break
#     }
#   }
#   if(i < 10)
#   {
#     All_16[each,"Player"] =  as.character(Dvoa[N,"playerName"])
#   }
# }
# 
# All_16 = data.frame(All_16)
# All_16["Key"] = paste(paste(All_16$Player, All_16$Date), All_16$Opp)
# Dvoa["Key"] = paste(paste(Dvoa$Player, Dvoa$Date), Dvoa$Opp)
# All_16_Combined = rbind.fill(All_16[0,], Dvoa[0,])
# 
# names(Dvoa)[names(Dvoa) == 'playerName'] <- 'Player'
# 
# unique(Dvoa$Key[Dvoa$Key %nin% All_16$Key])
# 
# 
# All_16_Combined = merge(All_16, Dvoa, by = c("Player","Date","Opp"))

########## 17 
colnames(All_17) = c("Date","B1","B2","Rating","Player","Pos","Salary","Min","Max","Team",
                     "Opp","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg Proj","Min Proj",
                     "Own1","Own2","Imp Pts","Act Pts","FP_Min","PF_Min-M","PER",
                     "Usage","Pro","My","Bargain","Opp_Plus_Minus","PaceD","Refs","TS_Per","Fouls_36",
                     "Points_Touch","Touches","S_B","B2B","Rest","Pts","Opp Pts","delta","Spread",
                     "O_U","Spread_per","PPG","Change","Consistency",
                     "Upside","Duds","Count","YPPG","YPlus_Minus","YChange","YConsistency","YUpside","YDuds","YCount","B5")

########## 17 
colnames(TT) = c("Date","B1","B2","Rating","Player","Pos","Salary","Min","Max","Team",
                     "Opp","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg Proj","Min Proj",
                     "Own1","Own2","Imp Pts","Act Pts","FP_Min","PF_Min-M","PER",
                     "Usage","Pro","My","Bargain","Opp_Plus_Minus","PaceD","Refs","TS_Per","Fouls_36",
                     "Points_Touch","Touches","S_B","B2B","Rest","Pts","Opp Pts","delta","Spread",
                     "O_U","Spread_per","PPG","Change","Consistency",
                     "Upside","Duds","Count","YPPG","YPlus_Minus","YChange","YConsistency","YUpside","YDuds","YCount","B5")

All_17 = rbind(All_17, TT)

All_17[All_17 == '&nbsp;'] <- 0
All_17["Ref1"] = 0
All_17["Ref2"] = 0
All_17$Date =  as.Date(All_17$Date, "%m/%d/%Y")
All_17$Opp =(gsub('[@,]', '', All_17$Opp))



All_17[] <- lapply(All_17, as.character)
#### Because there are naming differences make everything uniform
for(each in 1:nrow(All_17)){
  
  
  N  = amatch(as.character(All_17[each,"Player"] ), Dvoa$playerName , maxDist = 5)
  i = 0
  
  while(is.na(N)){
    N  = amatch(as.character(All_17[each,"Player"] ),  Dvoa$playerName , maxDist = (5 + i))
    i = i + 1
    if (i > 5){
      break
    }
  }
  if(i < 5)
  {
    All_17[each,"Player"] =  as.character(Dvoa[N,"playerName"])
  }
}

All_17 = data.frame(All_17)
All_17["Key"] = paste(paste(All_17$Player, All_17$Date), All_17$Opp)
Dvoa["Key"] = paste(paste(Dvoa$Player, Dvoa$Date), Dvoa$Opp)
All_17_Combined = rbind.fill(All_17[0,], Dvoa[0,])

names(Dvoa)[names(Dvoa) == 'playerName'] <- 'Player'

unique(Dvoa$Key[Dvoa$Key %nin% All_17$Key])


All_17_Combined = merge(All_17, Dvoa, by = c("Player","Date","Opp"))

names(All_17_Combined)[names(All_17_Combined) == 'Key.x'] <- 'Key'


All_16_Combined = All_16_Combined[,-1] 
All_17_Combined = All_17_Combined[,-114] 
names(All_17_Combined)[names(All_17_Combined) %nin% names(All_16_Combined)]

All_16 = rbind(All_16_Combined, All_17_Combined)
########################### Today's data Prep


Position = read.csv('Position.csv', header = FALSE)
Dvoa = read.csv('DVOA.csv')
Today = subset(All_17, All_17$Date == DateCheck)

colnames(Position) = c("ActualPos","Player")
colnames(Dvoa) = c("","Opp","Date","G.","ActualPos","FG","FGA","FGP","X3P","X3PA","FT","FTA",
                   "ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS","GMSC","plusMinus")

Dvoa_Set = aggregate(Dvoa$G., by = list(Dvoa$Opp), max)
NewDvoa = Dvoa[0,]

for(k in 1:nrow(Dvoa_Set)){
  NewDvoa = rbind(NewDvoa, subset(Dvoa, Dvoa$G. == Dvoa_Set[k,2] & Dvoa$Opp == as.character(Dvoa_Set[k,1])  ))  
}

Dvoa = NewDvoa
rm(NewDvoa)
rm(Dvoa_Set)
Dvoa["PTS"] = Dvoa["FG"] + Dvoa["X3P"]
Dvoa["REB"] = Dvoa["TRB"] 
Dvoa["STL"] = Dvoa["STL"] 
Dvoa["BLK"] = Dvoa["BLK"] 

if (ncol(Position) > 2){
  Position = Position[,1:2]
}
Position[] <- lapply(Position, as.character)
#### Because there are naming differences make everything uniform
for(each in 1:nrow(Position)){
  
  maxDist = 1
  N  = amatch(as.character(Position[each,"Player"] ), Today$Player , maxDist = 1)
  i = 0
  
  while(is.na(N)){
    N  = amatch(as.character(Position[each,"Player"] ), Today$Player , maxDist = (maxDist + i))
    i = i + 1
    if (i > 6){
      Position[each,"Player"] =  as.character(Position[each,"Player"] )
      break
    }
  }
  
  if(i < 6)
  {
    Position[each,"Player"] =  as.character(Today[N,"Player"])
  }
}

Position = data.frame(Position)

Today2 = merge(Today, Position, by = c("Player") )

Today_Not =  subset(Today, Today$Player %nin% Today2$Player)
Today_Not["ActualPos"] = gsub("/.*", "", Today_Not$Pos)

Today2 = rbind(Today_Not, Today2)
levels(Dvoa$Opp)[levels(Dvoa$Opp)=="PHO"] <- "PHX"
Today2 = merge(Today2, Dvoa, by = c("Opp","ActualPos") )


Today = Today2


colnames(Position) = c("Pos","PName")
colnames(Dvoa) = c("TeamName","Opp","Pos","Season","L5","L10","PTS","REB","AST","STL","BLK","3PM","FGP",
                   "FTP","TO","Rank")




Position[] <- lapply(Position, as.character)

#### Because there are naming differences make everything uniform
for(each in 1:nrow(Position)){
  
  
 N  = amatch(as.character(Position[each,"PName"] ), All_16$Player , maxDist = 5)
 i = 0
 
 while(is.na(N)){
   N  = amatch(as.character(Position[each,"PName"] ), All_16$Player , maxDist = (5 + i))
   i = i + 1
   if (i > 10){
     
     break
   }
 }
 if(i < 10)
 {
   Position[each,"PName"] =  as.character(All_16[N,"Player"])
 }
}

Position = data.frame(Position)


#### Combine Today's data with previous All_16

All_16["PTS"] = All_16["FG.y"] + All_16["X3P.y"]
All_16["REB"] = All_16["TRB.y"] 
All_16["STL"] = All_16["STL.y"] 
All_16["BLK"] = All_16["BLK.y"] 

All_16 = All_16[,c(
  "Player","Pos","Salary","Team","Opp","Rating","Date","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus",
  "Pts_Sal","Usg.Proj","Min.Proj","Own1","Own2","Imp.Pts","Act.Pts","FP_Min","PER",
  "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  "O_U","Spread_per","PPG","Consistency",
  "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount",
  "PTS","REB","STL","BLK"
)]

Today[,c("Date")] = Today$Date.x
Today = Today[,c(
  "Player","Pos","Salary","Team","Opp","Rating","Date","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus",
  "Pts_Sal","Usg.Proj","Min.Proj","Own1","Own2","Imp.Pts","Act.Pts","FP_Min","PER",
  "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  "O_U","Spread_per","PPG","Consistency",
  "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount",
  "PTS","REB","STL","BLK"
)]

All_16 = rbind(All_16, Today)

#### Fix Format issues in the data
All_16[is.null(All_16)] = 0
All_16[is.na(All_16)] = 0


All_16$Salary = as.numeric(gsub('[$,]', '', All_16$Salary))
All_16$Bargain =as.numeric(gsub('[%,]', '', All_16$Bargain))

All_16$Consistency =as.numeric(gsub('[%,]', '', All_16$Consistency))
All_16$Duds  =as.numeric(gsub('[%,]', '', All_16$Duds))
All_16$Count =as.numeric(gsub('[%,]', '', All_16$Count))
All_16$Upside =as.numeric(gsub('[%,]', '', All_16$Upside))

All_16$Opp =(gsub('[@,]', '', All_16$Opp))


All_16$YConsistency =as.numeric(gsub('[%,]', '', All_16$YConsistency))
All_16$YDuds  =as.numeric(gsub('[%,]', '', All_16$YDuds))
All_16$YCount =as.numeric(gsub('[%,]', '', All_16$YCount))
All_16$YUpside =as.numeric(gsub('[%,]', '', All_16$YUpside))

TempAll_16 = All_16
for(i in 6:length(names(TempAll_16))) {
  if (names(TempAll_16)[i] == "Date"){
    next;
  }
  TempAll_16[,i] = as.numeric(TempAll_16[,i])
}


TempAll_16[is.na(TempAll_16)] = 0

TempAll_16[is.null(TempAll_16)] = 0

##################### Spearman rank correlation ###########################
spearmanP = varclus(as.matrix(TempAll_16[,c(
  "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
  "Own1","Own2","Imp.Pts","Act.Pts","FP_Min","PER",
  "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  "O_U","Spread_per","PPG","Consistency",
  "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK"
  
)]),
similarity = "spearman")

plot(spearmanP)
abline(h=0.3)

##################### Spearman rank correlation ###########################
spearmanP = varclus(as.matrix(TempAll_16[,c(
  "Rating","Salary","Pts_Sal","Usg.Proj","Min.Proj", "FP_Min","PER",
  "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
  "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
  "O_U","Spread_per","PPG","Upside","Duds","Count","YPPG","YUpside","YDuds","YCount","PTS","REB","STL","BLK"
  
)]),
similarity = "spearman")

plot(spearmanP)
abline(h=0.3)


################################## PLayer BY player Prediction
# DateCheck = "2017-11-04"
All_16 = TempAll_16
Data_Cleaned = All_16[,c("Player","Team","Opp","Pos","Date","Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                         "Own1","Own2","Imp.Pts","Act.Pts","FP_Min","PER",
                         "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                         "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                         "O_U","Spread_per","PPG","Consistency",
                         "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK" )]
Data_Cleaned = unique(Data_Cleaned)
Data_Cleaned_Test = subset(Data_Cleaned, ( as.Date(Data_Cleaned$Date) == as.Date(DateCheck) ) )

Data_Cleaned_Train = subset(Data_Cleaned, as.Date(Data_Cleaned$Date) < as.Date(DateCheck))

######
 # Data_Cleaned_Test = subset(Data_Cleaned_Test, as.character(Data_Cleaned_Test$Player) == "Lou Williams")
 # Data_Cleaned_Test$Rating = 81.1
##

playerNames = unique(Data_Cleaned_Test$Player)


Results = data.frame( RFPred = numeric(), Xgb = numeric(), Name = factor(), Pos = factor() ,
                      Salary = numeric(), Actual = numeric() , HTeam = factor(), OTeam = factor(),
                      Pts = numeric(), DNNPer = numeric(), DNN = numeric(),xgbPLUSMINUS = numeric(),
                      RFPLUSMINUS = numeric())

for (each in 30:length(playerNames)){
  Data_Cleaned_Test = subset(Data_Cleaned, Data_Cleaned$Date == DateCheck 
                             & Data_Cleaned$Player == as.character(playerNames[each]) )

  Data_Cleaned_Train = subset(Data_Cleaned, Data_Cleaned$Date != DateCheck
                              & Data_Cleaned$Player == as.character(playerNames[each]) )
  print (playerNames[each])
  print (each)
  ### This ensures atleast 1 row of data exists for prediction
  if (nrow(Data_Cleaned_Test) < 1 ){
    next
  }
  
  #### If less than 15 rows then use that Teams's data
  if (nrow(Data_Cleaned_Train) < 15){
    Data_Cleaned_Train = subset(Data_Cleaned, Data_Cleaned$Date != DateCheck
                                & Data_Cleaned$Team
                                == as.character( unique ( subset(Data_Cleaned, Data_Cleaned$Player == as.character(playerNames[each]) )$Team ) )
                                
    )
   
  }

  if (nrow(Data_Cleaned_Train) < 15){
    next
  }
  
  #### People to skip
  if (playerNames[each] == "Kay Felder" & playerNames[each] == "Ian Mahinmi" & playerNames[each] == "Jodie Meeks"){
    next
  }
  
  ######### Construct Models
  indices = sample(1:nrow(Data_Cleaned_Train), 7, replace = FALSE)
  Data_Cleaned_Train_PlusMinusTrain = Data_Cleaned_Train[-indices, ]
  Data_Cleaned_Train_PlusMinusTest = Data_Cleaned_Train[indices, ]
  

  #########RF
  rf = randomForest( Data_Cleaned_Train[,c( "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                                            "Imp.Pts","FP_Min","PER",
                                            "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                                            "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                                            "O_U","Spread_per","PPG","Consistency",
                                            "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK" )], 
                     y = Data_Cleaned_Train[,c("Act.Pts")], ntree=100
                     ,type='regression')
  
  rf_PlusMinus =  randomForest( Data_Cleaned_Train_PlusMinusTrain[,c(  "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                                                                       "Own1","Own2","Imp.Pts","FP_Min","PER",
                                                                       "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                                                                       "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                                                                       "O_U","Spread_per","PPG","Consistency",
                                                                       "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK"  )], 
                                y = Data_Cleaned_Train_PlusMinusTrain[,c("Act.Pts")], ntree=100
                                ,type='regression')
  
  ####XGB
  trainSparceMatrix = sparse.model.matrix( Data_Cleaned_Train$`Act.Pts` ~ 
                                             (Data_Cleaned_Train$Rating + Data_Cleaned_Train$`Usg.Proj` + Data_Cleaned_Train$Pts_Sal 
                                              + Data_Cleaned_Train$`Min.Proj` + Data_Cleaned_Train$Pro + Data_Cleaned_Train$Bargain
                                              + Data_Cleaned_Train$Own1 + Data_Cleaned_Train$PER + Data_Cleaned_Train$Pts + Data_Cleaned_Train$Usage +
                                                Data_Cleaned_Train$Opp_Plus_Minus + Data_Cleaned_Train$PaceD + Data_Cleaned_Train$TS_Per + Data_Cleaned_Train$Fouls_36 +
                                                Data_Cleaned_Train$Points_Touch + Data_Cleaned_Train$Touches + Data_Cleaned_Train$Rest + Data_Cleaned_Train$Pts +
                                                Data_Cleaned_Train$`Opp.Pts` + Data_Cleaned_Train$delta + Data_Cleaned_Train$Spread + Data_Cleaned_Train$O_U +
                                                Data_Cleaned_Train$Spread_per + Data_Cleaned_Train$Upside + Data_Cleaned_Train$Duds + Data_Cleaned_Train$Count +
                                                Data_Cleaned_Train$YPlus_Minus + Data_Cleaned_Train$YDuds + Data_Cleaned_Train$YCount+    Data_Cleaned_Train$PTS +
                                                Data_Cleaned_Train$REB + Data_Cleaned_Train$STL + Data_Cleaned_Train$BLK ))
  
  Labels = Matrix(Data_Cleaned_Train$`Act.Pts`, sparse = TRUE)
  
  dtrain <- xgb.DMatrix(data = trainSparceMatrix, label=Labels)
  
  trainSparceMatrix_PlusMinus = sparse.model.matrix(     Data_Cleaned_Train_PlusMinusTrain$`Act.Pts` ~ 
                                                           (Data_Cleaned_Train_PlusMinusTrain$Rating + Data_Cleaned_Train_PlusMinusTrain$`Usg.Proj` + Data_Cleaned_Train_PlusMinusTrain$Pts_Sal 
                                                            + Data_Cleaned_Train_PlusMinusTrain$`Min.Proj` + Data_Cleaned_Train_PlusMinusTrain$Pro + Data_Cleaned_Train_PlusMinusTrain$Bargain
                                                            + Data_Cleaned_Train_PlusMinusTrain$Own1 + Data_Cleaned_Train_PlusMinusTrain$PER + Data_Cleaned_Train_PlusMinusTrain$Pts + Data_Cleaned_Train_PlusMinusTrain$Usage +
                                                              Data_Cleaned_Train_PlusMinusTrain$Opp_Plus_Minus + Data_Cleaned_Train_PlusMinusTrain$PaceD + Data_Cleaned_Train_PlusMinusTrain$TS_Per + Data_Cleaned_Train_PlusMinusTrain$Fouls_36 +
                                                              Data_Cleaned_Train_PlusMinusTrain$Points_Touch + Data_Cleaned_Train_PlusMinusTrain$Touches + Data_Cleaned_Train_PlusMinusTrain$Rest + Data_Cleaned_Train_PlusMinusTrain$Pts +
                                                              Data_Cleaned_Train_PlusMinusTrain$`Opp.Pts` + Data_Cleaned_Train_PlusMinusTrain$delta + Data_Cleaned_Train_PlusMinusTrain$Spread + Data_Cleaned_Train_PlusMinusTrain$O_U +
                                                              Data_Cleaned_Train_PlusMinusTrain$Spread_per + Data_Cleaned_Train_PlusMinusTrain$Upside + Data_Cleaned_Train_PlusMinusTrain$Duds + Data_Cleaned_Train_PlusMinusTrain$Count +
                                                              Data_Cleaned_Train_PlusMinusTrain$YPlus_Minus + Data_Cleaned_Train_PlusMinusTrain$YDuds + Data_Cleaned_Train_PlusMinusTrain$YCount+    Data_Cleaned_Train_PlusMinusTrain$PTS +
                                                              Data_Cleaned_Train_PlusMinusTrain$REB + Data_Cleaned_Train_PlusMinusTrain$STL + Data_Cleaned_Train_PlusMinusTrain$BLK ))
  Labels_PlusMinus = Matrix(Data_Cleaned_Train_PlusMinusTrain$`Act.Pts`, sparse = TRUE)
  
  dtrain_PlusMinus <- xgb.DMatrix(data = trainSparceMatrix_PlusMinus, label=Labels_PlusMinus)
  
####H20
  
  TrainingH20= as.h2o(Data_Cleaned_Train)
  splits <- h2o.splitFrame(TrainingH20, c(0.9),  seed=1234)
  
  
  trainDNN  <- h2o.assign(splits[[1]], "train.hex") # 60%
  validDNN   <- h2o.assign(splits[[2]], "valid.hex") # 60%
    TrainingH202= as.h2o(Data_Cleaned_Test)
  splits2 <- h2o.splitFrame(TrainingH202,  seed=0)
  testDNN <- h2o.assign(splits2[[1]], "test.hex")  # 20%
  
  response <- "Act.Pts"
  
  predictors <- c( "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                   "Own1","Own2","Imp.Pts","FP_Min","PER",
                   "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                   "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                   "O_U","Spread_per","PPG","Consistency",
                   "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK" )
  
  
  m1 <- h2o.deeplearning(
    model_id="dl_model_first", 
    training_frame=trainDNN, 
    validation_frame=validDNN,   ## validation dataset: used for scoring and early stopping
    x=predictors,
    y=response,
    nfold = 5,
    #activation="Rectifier",  ## default
    hidden=c(300,100),       ## default: 2 hidden layers with 200 neurons each
    variable_importances=T,
    epochs = 5,
    categorical_encoding = "OneHotInternal"
  )

  
  

  
    
  #################################Predictions
  
  RFPred = predict( rf,  Data_Cleaned_Test[,c(  "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                                                "Own1","Own2","Imp.Pts","FP_Min","PER",
                                                "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                                                "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                                                "O_U","Spread_per","PPG","Consistency",
                                                "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK"  )] 
                    ,type = c("response") )
  
  
  RFPred_PlusMinus = predict( rf_PlusMinus,  Data_Cleaned_Train_PlusMinusTest[,c(  "Rating","Salary","Proj","Ceiling","Floor","Proj_Plus_Minus","Pts_Sal","Usg.Proj","Min.Proj",
                                                                                   "Own1","Own2","Imp.Pts","FP_Min","PER",
                                                                                   "Usage","Pro","Bargain","Opp_Plus_Minus","PaceD","TS_Per","Fouls_36",
                                                                                   "Points_Touch","Touches","Rest","Pts","Opp.Pts","delta","Spread",
                                                                                   "O_U","Spread_per","PPG","Consistency",
                                                                                   "Upside","Duds","Count","YPPG","YPlus_Minus","YConsistency","YUpside","YDuds","YCount","PTS","REB","STL","BLK"  )] 
                             ,type = c("response") )
  plusMinus = Data_Cleaned_Train_PlusMinusTest$`Act.Pts` - RFPred_PlusMinus
   RFPLUSMINUS_M = ceil(min(plusMinus))
   RFPLUSMINUS_P = ceil(max(plusMinus))
  
  testSparseMatrix = sparse.model.matrix( 
    Data_Cleaned_Test$`Act.Pts` ~ 
      (Data_Cleaned_Test$Rating + Data_Cleaned_Test$`Usg.Proj` + Data_Cleaned_Test$Pts_Sal 
       + Data_Cleaned_Test$`Min.Proj` + Data_Cleaned_Test$Pro + Data_Cleaned_Test$Bargain
       + Data_Cleaned_Test$Own1 + Data_Cleaned_Test$PER + Data_Cleaned_Test$Pts + Data_Cleaned_Test$Usage +
         Data_Cleaned_Test$Opp_Plus_Minus + Data_Cleaned_Test$PaceD + Data_Cleaned_Test$TS_Per + Data_Cleaned_Test$Fouls_36 +
         Data_Cleaned_Test$Points_Touch + Data_Cleaned_Test$Touches + Data_Cleaned_Test$Rest + Data_Cleaned_Test$Pts +
         Data_Cleaned_Test$`Opp.Pts` + Data_Cleaned_Test$delta + Data_Cleaned_Test$Spread + Data_Cleaned_Test$O_U +
         Data_Cleaned_Test$Spread_per + Data_Cleaned_Test$Upside + Data_Cleaned_Test$Duds + Data_Cleaned_Test$Count +
         Data_Cleaned_Test$YPlus_Minus + Data_Cleaned_Test$YDuds + Data_Cleaned_Test$YCount+    
         Data_Cleaned_Test$REB + Data_Cleaned_Test$STL + Data_Cleaned_Test$BLK
    )) 
  
  xgbO = xgboost(data = dtrain ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
                 nrounds=2000,objective = "reg:linear" , verbose = 0 )
  
  predict(xgbO,testSparseMatrix )
  
  
  testSparseMatrix_PlusMinus = sparse.model.matrix( 
    Data_Cleaned_Train_PlusMinusTest$`Act.Pts` ~ 
      (Data_Cleaned_Train_PlusMinusTest$Rating + Data_Cleaned_Train_PlusMinusTest$`Usg.Proj` + Data_Cleaned_Train_PlusMinusTest$Pts_Sal 
       + Data_Cleaned_Train_PlusMinusTest$`Min.Proj` + Data_Cleaned_Train_PlusMinusTest$Pro + Data_Cleaned_Train_PlusMinusTest$Bargain
       + Data_Cleaned_Train_PlusMinusTest$Own1 + Data_Cleaned_Train_PlusMinusTest$PER + Data_Cleaned_Train_PlusMinusTest$Pts + Data_Cleaned_Train_PlusMinusTest$Usage +
         Data_Cleaned_Train_PlusMinusTest$Opp_Plus_Minus + Data_Cleaned_Train_PlusMinusTest$PaceD + Data_Cleaned_Train_PlusMinusTest$TS_Per + Data_Cleaned_Train_PlusMinusTest$Fouls_36 +
         Data_Cleaned_Train_PlusMinusTest$Points_Touch + Data_Cleaned_Train_PlusMinusTest$Touches + Data_Cleaned_Train_PlusMinusTest$Rest + Data_Cleaned_Train_PlusMinusTest$Pts +
         Data_Cleaned_Train_PlusMinusTest$`Opp.Pts` + Data_Cleaned_Train_PlusMinusTest$delta + Data_Cleaned_Train_PlusMinusTest$Spread + Data_Cleaned_Train_PlusMinusTest$O_U +
         Data_Cleaned_Train_PlusMinusTest$Spread_per + Data_Cleaned_Train_PlusMinusTest$Upside + Data_Cleaned_Train_PlusMinusTest$Duds + Data_Cleaned_Train_PlusMinusTest$Count +
         Data_Cleaned_Train_PlusMinusTest$YPlus_Minus + Data_Cleaned_Train_PlusMinusTest$YDuds + Data_Cleaned_Train_PlusMinusTest$YCount+    Data_Cleaned_Train_PlusMinusTest$PTS +
         Data_Cleaned_Train_PlusMinusTest$REB + Data_Cleaned_Train_PlusMinusTest$STL + Data_Cleaned_Train_PlusMinusTest$BLK
      )) 
  
  xgbO_PlusMinus = xgboost(data = dtrain_PlusMinus ,booster = "gblinear" , eta = 0.1 , max_depth=50, nthread = 4,
                 nrounds=2000,objective = "reg:linear" , verbose = 0 )
  
  plusMinus =  Data_Cleaned_Train_PlusMinusTest$`Act.Pts` - predict(xgbO_PlusMinus,testSparseMatrix_PlusMinus )
  xgbPLUSMINUS_M = ceil(min(plusMinus))
  xgbPLUSMINUS_P = ceil(max(plusMinus))
##################################
  
  
  
  Prediction2 =  as.data.frame(RFPred)
  Prediction2["RFPer"] = as.data.frame(  Prediction2["RFPred"]*100/(Data_Cleaned_Test$`Salary`) )
  Prediction2["RF_M"] =  as.data.frame(RFPLUSMINUS_M)
  Prediction2["RF_P"] =  as.data.frame(RFPLUSMINUS_P)
  Prediction2["Actual"] =  as.data.frame(Data_Cleaned_Test$`Act.Pts`)
  Prediction2["Salary"] =  as.data.frame(Data_Cleaned_Test$`Salary`)
  Prediction2["Name"] =  as.data.frame(Data_Cleaned_Test$Player)
  Prediction2["HTeam"] =  as.data.frame(Data_Cleaned_Test$Team)
  Prediction2["Opp"] = as.data.frame(Data_Cleaned_Test$Opp)
  Prediction2["Pts"] =  as.data.frame(Data_Cleaned_Test$Pts)
  Prediction2["Pos"] =  as.data.frame(Data_Cleaned_Test$Pos)
  Prediction2["Xgb"] = as.data.frame(predict(xgbO, testSparseMatrix))
  Prediction2["XgbPer"] = as.data.frame(  Prediction2["Xgb"]*100/(Data_Cleaned_Test$`Salary`) )
  Prediction2["xgb_M"] =  as.data.frame(xgbPLUSMINUS_M)
  Prediction2["xgb_P"] =  as.data.frame(xgbPLUSMINUS_P)
  Prediction2["DNN"] = as.data.frame(h2o.predict(m1,newdata=testDNN))
  Prediction2["DNNPer"] = as.data.frame(  Prediction2["DNN"]*100/(Data_Cleaned_Test$`Salary`) )
  
  Results = rbind(Results, Prediction2)
  
}

Results

Backup = Results

Results[,"ActualPos"] = 0
Results[,"Rank"] = 0
Results[,"L5"] = 0
Results[,"L10"] = 0
Results[,"Player_Per"] = 0
Results[,"Player_Med"] = 0
Results[,"Player_Projected"] = 0



colnames(Position) = c("Pos","PName")

Dvoa = read.csv("DVOA_Rank.csv", header = FALSE)
colnames(Dvoa) = c("TeamName","TNShort","Pos","L5","L10","PTS","REB","AST","STL","BLK","3PM","FGP",
                   "FTP","TO","Rank")

for(each in 1:nrow(Results)){
  # Find actual Position
  P = subset(Position, Position$PName == as.character(Results[each,"Name"]) )
  if (nrow(P) > 0){
    Results[each, "ActualPos"] = as.character(P[1,"Pos"])
  }

  #Find Rank, L5, L10
  P = subset(Dvoa, Dvoa$TNShort == as.character(Results[each,"Opp"]) & 
                    Dvoa$Pos == as.character(Results[each,"ActualPos"])  )
  if (nrow(P) > 0){
    Results[each, "Rank"] = as.numeric(P$Rank)
    Results[each, "L5"] = as.numeric(P$L5)
    Results[each, "L10"] = as.numeric(P$L10)
  }
  
  
}



# All_17 = subset(All_17, All_17$Date != "11/6/2017")
All_17$Act.Pts = as.numeric(All_17$Act.Pts)
for(each in 1:nrow(Results)){
  
  ######### Split ###
  ### if Postition was not found then use Position from FL
  if ( Results[each, "ActualPos"] == "0"){
    
    positions = ((unlist(strsplit(as.character( Results[each, "Pos"] ), "/"))))
    
    if (length(positions) > 1 ){
      sub_T = subset(All_17, All_17$Team == as.character(Results[each, "HTeam"]) )
      
      T1 =  subset( sub_T , grepl(positions[1], sub_T$Pos))
      T2 =  subset( sub_T , grepl(positions[2], sub_T$Pos))
      T1 = rbind(T1,T2)
      T1 = unique(T1)
      
    }
    
    if (length(positions) == 1){
      sub_T = subset(All_17, All_17$Team == as.character(Results[each, "HTeam"]) )
      
      T1 =  subset( sub_T , grepl(positions[1], sub_T$Pos))
      T1 = unique(T1)
    }
    
  }
  
  ### if ActualPos is providied as they are starters then you don't need to use FL probable positions
  if (Results[each, "ActualPos"] != "0") {
    
    positions = Results[each, "ActualPos"]
    sub_T = subset(All_17, All_17$Team == as.character(Results[each, "HTeam"]) )
    T1 =  subset( sub_T , grepl(positions[1], sub_T$Pos))
    T1 = unique(T1)
    
  }
  
  ### Get All players in that position on this team
  Player_Points = subset(T1, T1$Player == as.character(Results[each, "Name"])  )
  NonPlayer_Points = subset(T1, T1$Player != as.character(Results[each, "Name"]))
  
  
  ### Remove  Starters who have flexible position
  if (Results[each, "ActualPos"] != "0"){
    
    playes_NotFound = unique(NonPlayer_Points$Player)
    for(each2 in 1:length(playes_NotFound)){
      C =  which(Results$Name == as.character(playes_NotFound[each2]) )
      
      if ( length(C) > 0 ) {
        
        if (Results[C,"ActualPos"] != "0" ){
          NonPlayer_Points = subset(NonPlayer_Points, NonPlayer_Points$Player != as.character(Results[C, "Name"]))
        }
      }
      
    }
    
  }
    
  
  Results[each,"Player_Per"] = sum((Player_Points$`Act.Pts`)) / ( sum((Player_Points$`Act.Pts`)) + sum((NonPlayer_Points$`Act.Pts`)) )
  Results[each,"Player_Med"] = median(Player_Points$`Act.Pts`)
  Results[each,"Player_Projected"] =  Results[each, "L5"] * Results[each,"Player_Per"]
  
}




 # unique(Results$Name)
Results = unique(Results)

write.csv(Results, file = "NBA_Test_v01_25_2018.csv")

############### exp#####################

