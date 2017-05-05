
library(plyr)
library(rethinking)
library(lsr)

#checking for personality Q differences
#remodelling for risk as outcome and looking at rank*sex effects

myData <- read.delim('./spaceshipData.txt')
myData = na.omit(myData)

# compute personality score without Qs 7 & 8 to check if still sex difference for Thesis corrections
myData$personalityScoreREMOVED <- myData$q1 + myData$q2 + 
                                  myData$q3 + myData$q4 + myData$q5 + myData$q6 +
                                  myData$q9 + myData$q10 + myData$q11 + myData$q12  

myData$personalityScoreREMOVED <- myData$personalityScoreREMOVED/60
myData$personalityScore_removed <- myData$personalityScoreREMOVED*60


PersonalityScore <- myData$personalityScore
PersonalityR2 <- myData$personalityScoreR2
PersonalityR <- myData$personalityScoreREMOVED

colnames(myData)[2] <- "Gender"
Sex <- myData$SEX -1
Gender <- myData$Gender

#colnames(myData)[9] <- "CondName"
#myData$Condition <- myData$CONDITION -2

#Choice <- myData$CHOICE -1
#myData$Choice <- myData$CHOICE -1

#if else for k-1 dummy variables

#myData$AsocialRisky <- ifelse(myData$CondName == "saferisky", 1, 0)
#myData$SocialRisky <- ifelse(myData$CondName == "riskysafe", 1, 0)

#Rank <- myData$Rank 
#Rank[Rank == 3] <- 2
#Rank[Rank == 5] <- 3
#NRanks = length(unique(Rank))
#myData$Rank <- Rank

#NParticipants = length(unique(myData$ID))
#OldID <- myData$ID
#ParticipantID <- array(0,length(myData$ID))
#for (index in 1:NParticipants){
#  ParticipantID[OldID == unique(OldID)[index]] = index
#}

#myData$ParticipantID <- ParticipantID


myRiskData <- myData[!(myData$CONDITION==2),]

myPData <- myData[!duplicated(myData$ID),]
PersonalityR2 <- myPData$personalityScoreR2
PersonalityR <- myPData$personalityScoreREMOVED
Sex <- myPData$SEX

mM.pers <- map(
  alist(
        PersonalityR ~ dnorm(mu, sigma),
        mu <- a + b*Sex,  
        a ~ dnorm(20,10),
        b ~ dnorm(0,10),
        sigma ~ dunif(0,10)
  ),
  data=myPData )

precis(mM.pers)

mM.pers2 <- map(
  alist(
    PersonalityR ~ dnorm(mu, sigma),
    mu <- a ,  
    a ~ dnorm(20,10),
    sigma ~ dunif(0,10)
  ),
  data=myPData )

precis(mM.pers2)
compare(mM.pers,mM.pers2)


personalityPlot
personalityPlot <- ggplot(myPData, aes(myPData$personalityScore, fill = Gender)) 
personalityPlot + scale_fill_grey(start = 0.1, end = 0.9) + geom_density(alpha = 0.2) + theme_bw() + 
  theme(text = element_text(size=12), axis.title.y=element_text(margin=margin(0,12,0,0))) +
  scale_y_continuous(limits=c(0,0.09), expand = c(0,0)) +
  scale_x_continuous(limits=c(0,60), expand= c(0,0)) +
  xlab("\nRisky impulsivity Score") + ylab("Density") 

personalityMean = tapply(myPData$personalityScore_removed, list(myPData$Sex),mean)
personalityMean

personalitySd = tapply(myPData$personalityScore_removed, list(myPData$Sex),sd)
personalitySd

cohensD(myPData$personalityScore_removed[myPData$SEX==2],myPData$personalityScore_removed[myPData$SEX==1])
cohensD(myPData$personalityScore[myPData$SEX==2],myPData$personalityScore[myPData$SEX==1])


myData$personalityScore_removed[myData$SEX==2]
myData$personalityScore_removed[myData$SEX==1]
