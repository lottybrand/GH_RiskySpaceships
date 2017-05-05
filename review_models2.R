

#trying multi-level model with ppt random effect for all models after review for spaceships:
#choice is also recoded here so that social = 1
#no a priori reason to have pers in fulModel


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
Personality <- myData$Personality

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
      b_r*Rank,
    a ~ dnorm(0,10),
    c(b_s, b_s_AR, b_s_SR, b_r, b_AR, b_SR) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=myData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=3, cores=3 )

precis(FullModel)
plot(precis(FullModel,pars=c("a","b_s","b_AR","b_SR","b_s_AR","b_s_SR","b_r")))

## Alternative full suggested by McElreath, replacing sex*condition with personality*condition)

#FullModel2 <- map2stan(
#  alist(
#    Choice ~ dbinom(1, p),
#    logit(p) <- a + a_p[ID]*sigma_p + 
#      b_s*Sex + 
#      b_AR*AsocialRisky + 
#      b_SR*SocialRisky +
#      b_p_AR*Personality*AsocialRisky + 
#      b_p_SR*Personality*SocialRisky +
#      b_r*Rank +
#      b_p*Personality,
#    a ~ dnorm(0,10),
#    c(b_s, b_p_AR, b_p_SR, b_r, b_AR, b_SR, b_p) ~ dnorm(0,4),
#    a_p[ID] ~ dnorm(0,1),
#    sigma_p ~ dcauchy(0,1)
#  ),
#  data=myData, constraints=list(sigma_p="lower=0"), 
#  warmup=1000, iter=2000, chains=3, cores=3 )

#precis(FullModel2)
#compare(FullModel,FullModel2)

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

#precis(NullModel)

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

#precis(just_sex)

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

#precis(just_conditions)

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

#precis(just_interactions)


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
plot(precis(just_interactions_sex,pars=c("a", "b_s", "b_s_AR", "b_s_SR"), depth=2))

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


#trying multilevel predictions: p.376 onwards
#create new data frame for predicted estimates (p.378) 

d.predNew<- data.frame(
  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
  Sex = c(0,0,0,1,1,1), #men in C, SR, AR, women in C, SR, AR,
  ID = rep(2, 6), #random placeholder?
  Rank = rep(2,6) #random placeholder?
)

#create spaceship ensemble like page 204 but use replace from page 379 for an average participant

a_p_zeros <- matrix(0,1000,88)

#spaceship.ensemble <- ensemble(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex, data=d.predNew)

spaceship.ensemble <- ensemble(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex, data=d.predNew,
                               replace = list(a_p = a_p_zeros))

d.predNew$means = apply(spaceship.ensemble$link,2,mean)
d.predNew$PI.L = apply(spaceship.ensemble$link,2,PI)[1,]
d.predNew$PI.U = apply(spaceship.ensemble$link,2,PI)[2,]

str(spaceship.ensemble)

###### OR create predictions for new clusters via p.379 #########

#this line: #post <- extract.samples(spaceship.ensemble)# doesn't work so try simulating based on best models and smuggling that into ensemble?
#it seems to work, but not sure if it's legit, need to double check how it's working
#also doesn't seem to make much difference. 


#post <- extract.samples(just_interactions_sex)
#a_p_sims <- rnorm(88000,0,post$sigma_p)
#a_p_sims <- matrix(a_p_sims,1000,88)

#spaceship.ensemble <- ensemble(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex, data=d.predNew,
#                               replace = list(a_p = a_p_sims))

#d.predNew$means = apply(spaceship.ensemble$link,2,mean)
#d.predNew$PI.L = apply(spaceship.ensemble$link,2,PI)[1,]
#d.predNew$PI.U = apply(spaceship.ensemble$link,2,PI)[2,]

#### NOW SKIP TO "MAKE A PLOT FRIENDLY TABLE, LINE 285 ######



##### Try average intercepts for Full Model, p.378 ######

#d.predNew<- data.frame(
#  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
#  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
#  Sex = c(0,0,0,1,1,1), #men in C, SR, AR, women in C, SR, AR,
#  ID = rep(2, 6), #random placeholder?
#  Personality = rep(2,6), #random placeholder?
#  Rank = rep(2,6) #random placeholder?
#)

#link.FullModel <- link(FullModel, n=1000, data=d.predNew,
#                       replace=list(a_p = a_p_zeros))

#
#d.predNew$means = apply(link.FullModel,2,mean)
#d.predNew$PI.L = apply(link.FullModel,2,PI)[1,]
#d.predNew$PI.U = apply(link.FullModel,2,PI)[2,]



### OR TRY simulating new actor intercepts instead, based on just best fitting model, bottom page 379

#d.predNew<- data.frame(
#  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
#  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
#  Sex = c(0,0,0,1,1,1), #men in C, SR, AR, women in C, SR, AR,
#  ID = rep(2,6), #random placeholder?
#  Personality = rep(2,6),
#  Rank = rep(2,6) #placeholder?
#)

#post <- extract.samples(just_interactions_sex)
#a_p_sims <- rnorm(1000,0,post$sigma_p)
#a_p_sims <- matrix(a_p_sims,1000,88)
#link.just_interactions_sex <- link(just_interactions_sex, n=1000, data=d.predNew,
#                                   replace=list(a_p = a_p_sims))

#d.predNew$means = apply(link.just_interactions_sex,2,mean)
#d.predNew$PI.L = apply(link.just_interactions_sex,2,PI)[1,]
#d.predNew$PI.U = apply(link.just_interactions_sex,2,PI)[2,]


###### MAKE THE PLOT FRIENDLY TABLE  ######
d.predNew$Cond <- ifelse((d.predNew$AsocialRisky == "0") & (d.predNew$SocialRisky == "0"), 2, 
                         +    ifelse((d.predNew$SocialRisky=="1") & (d.predNew$AsocialRisky == "0"), 1,
                                     +    ifelse((d.predNew$AsocialRisky == "1"), 3, 99)))

namedCond <- d.predNew$Cond
namedCond[namedCond==1] <- "Social Risky"
namedCond[namedCond==2] <- "Control"
namedCond[namedCond==3] <- "Asocial Risky"
d.predNew$Condition <- namedCond

colnames(d.predNew)[3] <- "Sex.num"

Gender <- d.predNew$Sex.num
Gender[Gender==0] <- "Male"
Gender[Gender==1] <- "Female"
d.predNew$Sex <- Gender

#saving d.predNew
write.table(d.predNew, file = "d.predNew_05.05.17", sep = "\t")
#reopen this again
#readD.Pred <- read.delim("d.predNew", sep = "\t")


###### MAKE THE PLOT #######

limits <- aes(ymax = d.predNew$PI.U, ymin = d.predNew$PI.L)
predPlot <- ggplot(data = d.predNew, aes(Condition, means, shape = Sex))
predPlot + geom_point(data = d.predNew, stat="identity", position = position_dodge(width=0.3), size = 2.8) + 
  geom_errorbar(limits, width = 0.08, position = position_dodge(width=0.3)) +
  geom_hline(aes(yintercept=0.5), linetype="dashed", show.legend=FALSE) + 
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Source") +
  scale_y_continuous(limits=c(0,1), expand = c(0,0)) +
  scale_x_discrete(limits=c("Control", "Social Risky","Asocial Risky")) 


meansTable = tapply(d.predNew$means, list(d.predNew$Condition, d.predNew$Sex),mean)
meansTable
upperTable = tapply(d.predNew$PI.U, list(d.predNew$Condition, d.predNew$Sex),mean)
upperTable
lowerTable = tapply(d.predNew$PI.L, list(d.predNew$Condition, d.predNew$Sex),mean)
lowerTable


##### PLOTTING RAW DATA FOR COMPARISON #######

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
  theme_bw() + theme(text = element_text(size=12), axis.title.x=element_blank(), axis.title.y=element_text(margin=margin(0,12,0,0))) + 
  ylab("Proportion Chose Social Information") +
  scale_x_discrete(limits = c("Control", "Social Risky", "Asocial Risky")) +
  scale_y_continuous(limits=c(0,1))
rawPlot

#trying risk model, need to faff with data to remove Control condition (where risk=0 always)
###### MUST re-load myData at top of the file, then follow the steps below here, before running Risk model, otherwise lots of errors (R remembers everything..!)######
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

myRiskData$ID <- ParticipantID
Sex <- myRiskData$SEX
Risk <- myRiskData$RISK
myRiskData$Personality <- myRiskData$personalityScoreR2


RiskModel <- map2stan(
  alist(
    RISK ~ dbinom(1, p),
    logit(p) <- a + a_p[ID]*sigma_p + 
      b_s*Sex + 
      b_r*Rank + 
      b_SR*Sex*Rank +
      b_p*Personality,
    a ~ dnorm(0,10),
    c(b_s, b_r, b_SR, b_p) ~ dnorm(0,4),
    a_p[ID] ~ dnorm(0,1),
    sigma_p ~ dcauchy(0,1)
  ),
  data=myRiskData, constraints=list(sigma_p="lower=0"), 
  warmup=1000, iter=2000, chains=1, cores=1 )

precis(RiskModel)
plot(precis(RiskModel,pars=c("a","b_s","b_r","b_SR"),depth=2))

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
