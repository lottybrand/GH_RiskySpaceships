
## Models that I want to put into the ensemble 

library(plyr)
library(rethinking)
myData <- read.delim('./spaceshipData.txt')
myData = na.omit(myData)

#preparing the data for the model:
colnames(myData)[2] <- "Gender"
myData$Sex <- myData$SEX -1
colnames(myData)[8] <- "CondName"
myData$Condition <- myData$CONDITION -2
Choice <- myData$CHOICE -1
myData$Choice <- myData$CHOICE -1
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

#playing with subsets for some decriptives#

AR_Subset <- myData[myData$CondName == "saferisky",]
SR_Subset <- myData[myData$CondName == "riskysafe",]
C_Subset<- myData[myData$CondName =="safesafe",]

table(AR_Subset$Gender)
table(SR_Subset$Gender)
table(C_Subset$Gender)


#First model, full model: 

full_model <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_AR*AsocialRisky + b_SR*SocialRisky + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky + b_r*Rank,  
        a ~ dnorm(0,10),
        c( b_s, b_AR, b_SR, b_s_AR, b_s_SR, b_r) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(full_model)
plot(full_model)

#Null model

null_model <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a, 
        a ~ dnorm(0,10)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(null_model)

#Just Sex

just_sex <- mM.2 <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex,
        a ~ dnorm(0,10),
        c(b_s) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(just_sex)

#Just Conditions

just_conditions <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_AR*AsocialRisky + b_SR*SocialRisky,  
        a ~ dnorm(0,10),
        c(b_AR, b_SR) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(just_conditions)

#Just Interactions

just_interactions <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky,  
        a ~ dnorm(0,10),
        c(b_s_AR, b_s_SR) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(just_interactions)
coeftab(just_interactions)
plot(coeftab(just_interactions))
compare(full_model,null_model,just_sex,just_conditions,just_interactions)

#Just interactions and sex

just_interactions_sex <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky,  
        a ~ dnorm(0,10),
        c( b_s, b_s_AR, b_s_SR) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

plot(coeftab(just_interactions_sex))
coeftab(just_interactions_sex)
precis(just_interactions_sex)

#just conditions and sex
just_conditions_sex <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_AR*AsocialRisky + b_SR*SocialRisky,  
        a ~ dnorm(0,10),
        c( b_s, b_AR, b_SR) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

compare(full_model,null_model,just_sex,just_conditions,just_interactions, just_interactions_sex, just_conditions_sex)
compare(just_conditions,just_interactions, just_interactions_sex, just_conditions_sex)


#create new data frame for predicted estimates a la page 297 to use with ensemble below 
d.pred<- data.frame(
  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
  Sex = c(0,0,0,1,1,1) #men in C, SR, AR, women in C, SR, AR,
)

#okay let's try ensemble 
spaceship.ensemble <- ensemble(just_conditions,just_interactions, just_interactions_sex, just_conditions_sex, data = d.pred)

mu <- apply(spaceship.ensemble$link,2,mean)
mu.PI <- apply(spaceship.ensemble$link,2,PI)


# now add this info into a nice table of d.pred
d.pred$means = apply(spaceship.ensemble$link,2,mean)
d.pred$PI.L = apply(spaceship.ensemble$link,2,PI)[1,]
d.pred$PI.U = apply(spaceship.ensemble$link,2,PI)[2,]

# make a graph friendly table 
d.pred$Cond <- ifelse((d.pred$AsocialRisky == "0") & (d.pred$SocialRisky == "0"), 2, 
                           +    ifelse((d.pred$SocialRisky=="1") & (d.pred$AsocialRisky == "0"), 1,
                                       +    ifelse((d.pred$AsocialRisky == "1"), 3, 99)))

namedCond <- d.pred$Cond
namedCond[namedCond==1] <- "Social Risky"
namedCond[namedCond==2] <- "Control"
namedCond[namedCond==3] <- "Asocial Risky"
d.pred$Condition <- namedCond

colnames(d.pred)[3] <- "Sex.num"

Gender <- d.pred$Sex.num
Gender[Gender==0] <- "Male"
Gender[Gender==1] <- "Female"
d.pred$Sex <- Gender

# Try to plot this 
cutoff <- data.frame(yintercept=0.5, cutoff=factor(0.5))
limits <- aes(ymax = d.pred$PI.U, ymin = d.pred$PI.L)
tryingPlot <- ggplot(data = d.pred, aes(Condition, means, colour = Sex))
  tryingPlot + geom_point(data = d.pred, stat="identity", size = 3.5) + 
    geom_errorbar(limits, width = 0.08) +
    geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, show.legend=FALSE) + 
    theme_bw() + theme(text = element_text(size=18)) + ylab("Proportion Chose Asocial Info") + ylim(0,1) +
    scale_x_discrete(limits=c("Social Risky", "Control","Asocial Risky")) +
    ggtitle("Model Predicted Means")
  
#plot raw data
  
rawData <- read.delim('./rawPercentages.txt')
levels(rawData$Condition)[levels(rawData$Condition)=="RiskyAsocial"] <- "Asocial Risky"
levels(rawData$Condition)[levels(rawData$Condition)=="RiskySocial"] <- "Social Risky"
rawData$propUpper <- rawData[, 7]/100

limitsRaw <- aes(ymax = rawData$propUpper, ymin = rawData$propLower)
tryingPlot <- ggplot(data = rawData, aes(Condition, propAsocial, colour = Sex))
tryingPlot + geom_point(data = rawData, size = 3.5) + 
  geom_errorbar(limitsRaw, width = 0) +
  theme_bw() + theme(text = element_text(size=26)) + ylab("Proportion Chose Asocial") + ylim(0,1) +
  scale_x_discrete(limits=c("Social Risky", "Control", "Asocial Risky")) +
  ggtitle("Raw Means")

#plot WAICS
models.compared <- compare(full_model,null_model,just_sex,just_conditions,just_interactions, just_interactions_sex, just_conditions_sex)
plot(models.compared, SE=TRUE, dSE=TRUE)


#check for personality predicting social/asocial 
pers_model <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_s*Sex + b_AR*AsocialRisky + b_SR*SocialRisky + b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky + b_r*Rank + b_pers*Personality,  
        a ~ dnorm(0,10),
        c( b_s, b_AR, b_SR, b_s_AR, b_s_SR, b_r, b_pers) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(pers_model, prob = 0.05)

#compared with just personality 
pers_model2 <- map2stan(
  alist(Choice ~ dbinom(1, p),
        logit(p) <- a + b_pers*Personality,  
        a ~ dnorm(0,10),
        c(b_pers) ~ dnorm(0,4)
  ),
  data=myData, warmup=1000, iter=6000, chains=1, cores=1 )

precis(pers_model2, prob = 0.95)

