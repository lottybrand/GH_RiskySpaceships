

#trying multi-level model with ppt random effect for all models after review for spaceships:
#choice is also recoded here so that social = 1


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


#McElreath's multilevel with non-centred parameterisation for full_model

FullModel <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_AR*AsocialRisky + 
      b_SR*SocialRisky +
      b_s_AR*Sex*AsocialRisky + 
      b_s_SR*Sex*SocialRisky +
      b_r*Rank +
      b_p*Personality,
    a ~ dnorm(0,10),
    c(b_s, b_s_AR, b_s_SR, b_r, b_AR, b_SR, b_p) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(FullModel)


### Null model

NullModel <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
        logit(p) <- a + a_p[ID]*sigma_p, 
        a ~ dnorm(0,10),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(NullModel)

###Just Sex

just_sex <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
        logit(p) <- a + a_p[ID]*sigma_p +
        b_s*Sex,
        a ~ dnorm(0,10),
        b_s ~ dnorm(0,4),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"),
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(just_sex)

###Just Conditions

just_conditions <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
        logit(p) <- a + a_p[ID]*sigma_p +
        b_AR*AsocialRisky + b_SR*SocialRisky,  
        a ~ dnorm(0,10),
        c(b_AR, b_SR) ~ dnorm(0,4),
        a_p[ID] ~ dnorm(0,1),
        sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"),
  warmup=1000, iter=6000, chains=1, cores=1 )

precis(just_conditions)

#Just Interactions

just_interactions <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s_AR*Sex*AsocialRisky + b_s_SR*Sex*SocialRisky,
      a ~ dnorm(0,10),
      c(b_s_AR, b_s_SR) ~ dnorm(0,4),
      a_p[ID] ~ dnorm(0,1),
      sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(just_interactions)


#Just interactions and sex

just_interactions_sex <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_s_AR*Sex*AsocialRisky + 
      b_s_SR*Sex*SocialRisky,
      a ~ dnorm(0,10),
      c(b_s, b_s_AR, b_s_SR) ~ dnorm(0,4),
      a_p[ID] ~ dnorm(0,1),
      sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

plot(coeftab(just_interactions_sex))
coeftab(just_interactions_sex)
precis(just_interactions_sex)

#just conditions and sex
just_conditions_sex <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_AR*AsocialRisky + 
      b_SR*SocialRisky,
      a ~ dnorm(0,10),
      c(b_s, b_AR, b_SR) ~ dnorm(0,4),
      a_p[ID] ~ dnorm(0,1),
      sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(just_conditions_sex)

compare(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex)

#trying multilevel predictions for new clusters, p.376 onwards
#Can't get ensemble to work here (need dummy data for personality and rank as well?!)
#will just use the just_interactions_sex model

#create new data frame for predicted estimates (p.378) 
d.pred<- data.frame(
  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
  Sex = c(0,0,0,1,1,1), #men in C, SR, AR, women in C, SR, AR,
  ID = rep(ParticipantID, 6)
)
#
a_p_zeros <- matrix(0,1000,88)


#can't use ensemble here 
#spaceship.ensemble <- ensemble(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex, data=d.pred)

#following page 379
link.just_interactions_sex <- link(just_interactions_sex, n=1000, data = d.pred,
                                   replace=list(a_p = a_p_zeros))


# now add this info into a nice table of d.pred
d.pred$means = apply(link.just_interactions_sex,2,mean)
d.pred$PI.L = apply(link.just_interactions_sex,2,PI)[1,]
d.pred$PI.U = apply(link.just_interactions_sex,2,PI)[2,]

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

#saving d.pred
#write.table(d.pred, file = "d.pred", sep = "\t")
#reopen this again
#readD.Pred <- read.delim("d.pred", sep = "\t")

#now re-plot (don't have to swap axis this time!)
#so these are predictions based just on the best fitting model (just_interactions_sex) -I think!


limits <- aes(ymax = d.pred$PI.U, ymin = d.pred$PI.L)
predPlot <- ggplot(data = d.pred, aes(Condition, means, shape = Sex))
predPlot + geom_point(data = d.pred, stat="identity", position = position_dodge(width=0.3), size = 3.5) + 
  geom_errorbar(limits, width = 0.08, position = position_dodge(width=0.3)) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) + 
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Information") +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete(limits=c("Control", "Social Risky","Asocial Risky")) 


#plot raw data for comparison:

#Sex as factor for this plot:
Sex <- myData$Sex
Sex[Sex==0] <- "Male"
Sex[Sex==1] <- "Female"
myData$Sex <- Sex

#Condition rename also
myData$CONDITION[myData$CONDITION==1]  <- "Social Risky"
myData$CONDITION[myData$CONDITION==2] <- "Control"
myData$CONDITION[myData$CONDITION==3] <- "Asocial Risky"
myData$Condition <- myData$CONDITION


rawPlot <- ggplot(myData, aes(Condition, Choice, shape = Sex)) +
  stat_summary(fun.y=mean, position= position_dodge(0.3), geom = "point", size = 2.8) +
  stat_summary(fun.data = mean_cl_normal, position = position_dodge(0.3), geom = "errorbar", width = 0.08) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) +
  theme_bw() +
  scale_x_discrete(limits = c("Control", "Social Risky", "Asocial Risky")) +
  scale_y_continuous(limits=c(0,1))
rawPlot

#trying risk model, need to faff with data to remove Control condition (where risk=0 always)

myRiskData <- myData[!(myData$CONDITION==2),]

#sort the relevant variables
Rank <- myRiskData$Rank 
Rank[Rank == 3] <- 2
Rank[Rank == 5] <- 3
NRanks = length(unique(Rank))
myRiskData$Rank <- Rank

#need to actually use this as bunch of participant IDs missing now:
NParticipants = length(unique(myRiskData$ID))
OldID <- myRiskData$ID
ParticipantID <- array(0,length(myRiskData$ID))
for (index in 1:NParticipants){
  ParticipantID[OldID == unique(OldID)[index]] = index
}
#I don't know why this had to be done but the below needs to be done, some sort of labelling mess:
myRiskData$ParticipantID <- ParticipantID
ID <- ParticipantID
myRiskData$ParticipantID <- ID
myRiskData$ID <- ParticipantID

RiskModel <- map2stan(
  alist(
    RISK ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_r*Rank + 
      b_SR*Sex*Rank,
      a ~ dnorm(0,10),
      c(b_s, b_r, b_SR) ~ dnorm(0,4),
      a_p[ID] ~ dnorm(0,1),
      sigma_p ~ dcauchy(0,1)
  ),
  data=myRiskData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=1, cores=1 )

precis(RiskModel)


#trying McElreath's order model? what is order here? is this modelling both rank as an ordered and random variable at the same time??

mm3 <- map2stan(
  alist(
    Choice ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_AR*AsocialRisky + 
      b_SR*SocialRisky +
      b_s_AR*Sex*AsocialRisky + 
      b_s_SR*Sex*SocialRisky +
      b_p*Personality +
      b_order*Rank +
      a_r[Rank],
    a ~ dnorm(0,10),
    c(b_s, b_s_AR, b_s_SR, b_AR, b_SR, b_p, b_order) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    a_r[Rank] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(mm3)
