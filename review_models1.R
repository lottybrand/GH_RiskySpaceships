

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

#probably can't do below? need to make predictions based on multilevel models?? 

#create new data frame for predicted estimates a la page 297 to use with ensemble below 
d.pred<- data.frame(
  SocialRisky = c(0,1,0,0,1,0), #this is to balance all possible combinations, see d.pred at end
  AsocialRisky = c(0,0,1,0,0,1), # ie when SR is 0 AR is 1 etc etc 
  Sex = c(0,0,0,1,1,1) #men in C, SR, AR, women in C, SR, AR,
)

#okay let's try ensemble 
ensemble(FullModel,NullModel,just_sex,just_conditions,just_interactions,just_interactions_sex,just_conditions_sex)

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










##makeing predictions for same clusters or new clusters? pp.376/381
trying_page378 <- list(
  prosoc_left = c(0,1,0,1),
  condition = c(0,0,1,1),
  actor = rep(2,4))


#trying McElreath's order model

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
