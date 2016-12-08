
library(plyr)
library(rethinking)

#checking for personality Q differences
#remodelling for risk as outcome and looking at rank*sex effects

myData <- read.delim('./spaceshipDataStan.txt')
myData = na.omit(myData)

dens(myData$personalityScore)
mean(myData$personalityScore)

Personality <- myData$personalityScore
Personality2 <- myData$personalityScoreR2

#preparing the data for the model:

colnames(myData)[3] <- "Gender"
myData$Sex <- myData$SEX -1

colnames(myData)[9] <- "CondName"
myData$Condition <- myData$CONDITION -2

Choice <- myData$CHOICE -1
myData$Choice <- myData$CHOICE -1

#if else for k-1 dummy variables

myData$AsocialRisky <- ifelse(myData$CondName == "saferisky", 1, 0)
myData$SocialRisky <- ifelse(myData$CondName == "riskysafe", 1, 0)

Rank <- myData$Rank 
Rank[Rank == 3] <- 2
Rank[Rank == 5] <- 3
NRanks = length(unique(Rank))
myData$Rank <- Rank

NParticipants = length(unique(myData$ID))
OldID <- myData$ID
ParticipantID <- array(0,length(myData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}

myData$ParticipantID <- ParticipantID

myRiskData <- myData[!(myData$CONDITION==2),]


mM.pers <- map(
  alist(
        Personality ~ dnorm(mu, sigma),
        mu <- a + b*Sex,  
        a ~ dnorm(20,10),
        b ~ dnorm(0,10),
        sigma ~ dunif(0,10)
  ),
  data=myData )

precis(mM.pers)

mM.pers2 <- map(
  alist(
    Personality ~ dnorm(mu, sigma),
    mu <- a ,  
    a ~ dnorm(20,10),
    sigma ~ dunif(0,10)
  ),
  data=myData )

precis(mM.pers2)
compare(mM.pers,mM.pers2)


mM.pers3 <- map(
  alist(
    Personality2 ~ dnorm(mu, sigma),
    mu <- a ,  
    a ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=myData )
precis(mM.pers3)

mM.pers4 <- map(
  alist(
    Personality2 ~ dnorm(mu, sigma),
    mu <- a + b*Sex,  
    a ~ dnorm(0,10),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=myData )

precis(mM.pers4)
compare(mM.pers4,mM.pers3)
