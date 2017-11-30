load("Bussiere.RData", envir = parent.frame(), verbose = FALSE)

datPlayerParcAtp <- data.frame(character(nbPlayer), character(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),numeric(nbPlayer),stringsAsFactors = FALSE)
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="character.nbPlayer."] <- "player"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="character.nbPlayer..1"] <- "Atp"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer."] <- "Hard"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..1"] <- "Carpet"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..2"] <- "Clay"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..3"] <- "WHard"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..4"] <- "WCarpet"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..5"] <- "WClay"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..6"] <- "PWHard"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..7"] <- "PWCarpet"
names(datPlayerParcAtp )[names(datPlayerParcAtp)=="numeric.nbPlayer..8"] <- "PWClay"
write.csv(datPlayerParcAtp, file = "playerParcAtp.csv")

save(datPlayerParcAtp, smp,file="Bussiere.RData")
#switch for the question
print(1/(4/100))
surfaceSwitch <- function(datPlayerParcAtp, surface, line,winner ){
  #TODO to Optimize
  switch(surface, 
         Hard={
           datPlayerParcAtp$Hard[line] <- datPlayerParcAtp$Hard[line] + 1
           if(winner){
             datPlayerParcAtp$WHard[line] <- datPlayerParcAtp$WHard[line] + 1
             
           } 
         },
         Clay={
           datPlayerParcAtp$Clay[line] <- datPlayerParcAtp$Clay[line] + 1  
           if(winner) {
             datPlayerParcAtp$WClay[line] <- datPlayerParcAtp$WClay[line] + 1  
             
           }
         },
         Carpet={
           datPlayerParcAtp$Carpet[line] <- datPlayerParcAtp$Carpet[line] + 1  
           if(winner) { 
             datPlayerParcAtp$WCarpet[line] <- datPlayerParcAtp$WCarpet[line] + 1 
             
           }
         }
  )
  return(datPlayerParcAtp)
}

for (i in 1:Size) {
  surface <- toString(smp[i, "surface"])
  winner <- toString(smp[i, "winner_name"])
  atpWinner <- toString(smp[i, "winner_rank"])
  indices <- which(datPlayerParcAtp$player == winner)
  line <- 0
  if (length(indices) == 0 ){
    indices <- which(datPlayerParcAtp$player == "")
    line <- indices[1]
    datPlayerParcAtp$player[line] <- winner
  }
  else {
    line <- indices[1]
  }
  datPlayerParcAtp$Atp[line] <-  atpWinner
  datPlayerParcAtp <- surfaceSwitch(datPlayerParcAtp, surface,line,TRUE)
  if (winner == "Ivo Heuberger")
  {
    print(winner)
    print(datPlayerParcAtp$Clay[line])
    print(datPlayerParcAtp$WClay[line])
    datPlayerParcAtp$PWClay[line] <- (datPlayerParcAtp$WClay[line] / (datPlayerParcAtp$Clay[line]/100))
    print(datPlayerParcAtp$PWClay[line])
  }
  datPlayerParcAtp$PWHard[line] <- (datPlayerParcAtp$WHard[line] / (datPlayerParcAtp$Hard[line]/100))       
  datPlayerParcAtp$PWClay[line] <- (datPlayerParcAtp$WClay[line] / (datPlayerParcAtp$Clay[line]/100))
  datPlayerParcAtp$PWCarpet[line] <- (datPlayerParcAtp$WCarpet[line] / (datPlayerParcAtp$Carpet[line]/100)) 
  if (winner == "Ivo Heuberger")
  {
    print(winner)
    print(datPlayerParcAtp$Clay[line])
    print(datPlayerParcAtp$WClay[line])
    datPlayerParcAtp$PWClay[line] <- (datPlayerParcAtp$WClay[line] / (datPlayerParcAtp$Clay[line]/100))
    print(datPlayerParcAtp$PWClay[line])
  }
  
  
  loser <- toString(smp[i, "loser_name"])
  atpLoser <- toString(smp[i, "loser_rank"])
  indices <- which(datPlayerParcAtp$player == loser)
  line <- 0
  if (length(indices) == 0 ){
    indices <- which(datPlayerParcAtp$player == "")
    line <- indices[1]
    datPlayerParcAtp$player[line] <- loser
  }
  else {
    line <- indices[1]
  }
  datPlayerParcAtp$Atp[line] <- atpLoser
  datPlayerParcAtp <- surfaceSwitch(datPlayerParcAtp, surface,line,FALSE)
  
}

write.csv(datPlayerParcAtp, file = "playerParcAtp.csv")

