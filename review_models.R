#trying multi-level model with ppt random effect for best model after review for spaceships:


library(plyr)
library(rethinking)
myData <- read.delim('./spaceshipData.txt')
myData = na.omit(myData)

#preparing the data for the model:
colnames(myData)[2] <- "Gender"
myData$Sex <- myData$SEX -1
colnames(myData)[8] <- "CondName"
myData$Condition <- myData$CONDITION -2
myData$Choice <- ifelse(myData$CHOICE == 2, 0, 1)
Choice <- myData$Choice
myData$Personality <- myData$personalityScoreR2

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

#best model from previous set for comparison:

just_interactions_sex <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_sex*Sex + b_sex_AsocialRisky*Sex*AsocialRisky + b_sex_SocialRisky*Sex*SocialRisky,  
        a ~ dnorm(0,10),
        c(b_sex,b_sex_AsocialRisky, b_sex_SocialRisky) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=3000, chains=4, cores=1 )

precis(just_interactions_sex)

#adding participant random effect
multiLevelModel <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky + a_p[ParticipantID],
        a ~ dnorm(0,10),
        c(b_s, b_s_AR, b_s_SR) ~ dnorm(0,4),
        a_p[ParticipantID] ~ dnorm(0, sigma_p),
        sigma_p ~ dcauchy(0,1)
  ),
  data=myData, warmup=1000, iter=3000, chains=4, cores=1 )

precis(multiLevelModel)
compare(multiLevelModel, just_interactions_sex)
plot(just_interactions_sex)
plot(multiLevelModel)


#rank random effect as well? 
multilevelModel2 <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky + a_p[ParticipantID] + a_r[Rank],
        a ~ dnorm(0,10),
        c(b_s, b_s_AR, b_s_SR) ~ dnorm(0,4),
        a_p[ParticipantID] ~ dnorm(0, sigma_p),
        a_r[Rank] ~ dnorm(0, sigma_r),
        sigma_p ~ dcauchy(0,1),
        sigma_r ~ dcauchy(0,1)
  ),
  data=myData, warmup=1000, iter=3000, chains=4, cores=1 )

precis(multilevelModel2)
plot(multiLevelModel)
plot(multilevelModel2)
compare(just_interactions_sex, multiLevelModel, multilevelModel2)
