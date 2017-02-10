library(car)
library(lme4)
library(lmerTest)
library(sciplot)
library(nlme)
library(multcomp)

data2015<-read.table("./data/clean/data2015_allfl.txt",header=T,sep="\t",dec=".")

head(data2015)
tail(data2015)
str(data2015)
data2015$pred<-as.factor(data2015$pred)
data2015$ori_new<-as.factor(data2015$ori_new)

with(data2015,hist(most_adv))
with(data2015,hist(phen_index))

#Keep pops where n>=10: 8 with vs 2 without predator
data2015_10 <- data2015[!(as.numeric(data2015$pop) %in% which(table(data2015$pop)<10)),]
data2015_10$pop<-droplevels(data2015_10$pop)

#Keep pops where n>=5: 9 with vs 3 without predator
data2015_5 <- data2015[!(as.numeric(data2015$pop) %in% which(table(data2015$pop)<5)),]
data2015_5$pop<-droplevels(data2015_5$pop)

#Keep pops where n>=4: 11 with vs 3 without predator
data2015_4 <- data2015[!(as.numeric(data2015$pop) %in% which(table(data2015$pop)<4)),]
data2015_4$pop<-droplevels(data2015_4$pop)

# Model 1: differences among populations? ####
with(data2015_10, lineplot.CI(pop,most_adv))
with(data2015_10, lineplot.CI(pop,phen_index))
with(data2015_10, lineplot.CI(pop,log(phen_index)))

with(data2015_4, bargraph.CI(pop,log(phen_index),err.width=0.02,ylab="Mean bud stage"))
with(data2015_4, bargraph.CI(pop,most_adv,err.width=0.02,ylab="Max bud stage"))


m1<-lm(log(phen_index)~pop,data=data2015_10)
m2<-lm(log(phen_index)~pop+ori_new,data=data2015_10)#Equivalent to pop/ori_new!
Anova(m1) #Same result as anova and as Anova(m1,type="III") - cause only 2 predictor
Anova(m2) #Not same as anova! Cause result of anova depends on order of predictors!
anova(m1,m2) #including ori is better
#In m1, pop seems *, but when including ori (m2), it is not anymore *
#Include ori as a random factor
m3<-lme(log(phen_index) ~ pop, data = data2015_10, random = ~ 1|ori_new)
Anova(m3)#pop NS - not exactly same as anova, but similar
#1|ori_new --> R sees the nesting cause the oris are numbered for all pops

m4<-lm(log(phen_index)~pop,data=data2015_4)
m5<-lm(log(phen_index)~pop+ori_new,data=data2015_4)
Anova(m4) 
Anova(m5) 
anova(m4,m5)
m6<-lme(log(phen_index) ~ pop, data = data2015_4, random = ~ 1|ori_new)
Anova(m6)

m7<-lm(most_adv~pop,data=data2015_10)
m8<-lm(most_adv~pop+ori_new,data=data2015_10)
Anova(m7) 
Anova(m8) 
anova(m7,m8)
m9<-lme(most_adv ~ pop, data = data2015_10, random = ~ 1|ori_new)
Anova(m9) # p=0.04886
summary(m9)
#Post-hoc comparisons
m9c <- glht(m9, linfct=mcp(pop="Tukey"))
summary(m9c) #None significant? Strange... (but oh well... p was almost 0.05)

m10<-lm(most_adv~pop,data=data2015_4)
m11<-lm(most_adv~pop+ori_new,data=data2015_4)
Anova(m10) 
Anova(m11) 
anova(m10,m11)
m12<-lme(most_adv ~ pop, data = data2015_4, random = ~ 1|ori_new)
Anova(m12) # NS

#Conclusion: mostly no difs between pops after accounting for ori
#(except small effect when using data2015_10 and most_adv)

#Group "Herr" and "Göt009" pops into the same
data2015$pop_g<-data2015$pop
lpop1 <- levels(data2015$pop_g)
ind1 <- c(which(lpop1 == 'Göt009a'), which(lpop1 == 'Göt009b'))
levels(data2015$pop_g)[ind1] <- 'Göt009'
lpop2 <- levels(data2015$pop_g)
ind2 <- c(which(lpop2 == 'Her003'), which(lpop2 == 'Her004'), which(lpop2 == 'Her005'))
levels(data2015$pop_g)[ind2] <- 'Her'

#Not better (code erased)

# Model 2: differences related to predation? ####
with(data2015_10, lineplot.CI(pred,most_adv))
with(data2015_10, lineplot.CI(pred,phen_index))
with(data2015_10, lineplot.CI(pred,log(phen_index)))

m13<-lme(log(phen_index) ~ pred, data = data2015_10, random = ~ 1|pop/ori_new)
Anova(m13)
m14<-lme(log(phen_index) ~ pred, data = data2015_4, random = ~ 1|pop/ori_new)
Anova(m14)
m15<-lme(most_adv ~ pred, data = data2015_10, random = ~ 1|pop/ori_new)
Anova(m15)
m16<-lme(most_adv ~ pred, data = data2015_4, random = ~ 1|pop/ori_new)
Anova(m16)

#No effect of predation (expected as no difs between pops)