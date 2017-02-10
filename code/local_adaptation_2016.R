library(car)
library(lme4)
library(lmerTest)
library(sciplot)
library(nlme)
library(multcomp)
library(dplyr)
library(data.table)

data2016s<-read.table("./data/clean/phen_cgarden_2016_clean_short.txt",header=T,sep="\t",dec=".")

head(data2016s)
tail(data2016s)
str(data2016s)
data2016s$ori_new<-as.factor(data2016s$ori_new)
data2016s$pred<-as.factor(data2016s$pred)
data2016s$stage0808<-as.integer(data2016s$stage0808)
data2016s<-data2016s[complete.cases(data2016s),]

#Same analysis as for 2015 with stage0808 (=stage0808 variable) ####
with(data2016s,hist(stage0808))

#Keep pops where n>=10: 7 with vs 1 without predator
data2016s_10 <- data2016s[!(as.numeric(data2016s$pop) %in% which(table(data2016s$pop)<10)),]
data2016s_10$pop<-droplevels(data2016s_10$pop)

#Keep pops where n>=5: 11 with vs 5 without predator
data2016s_5 <- data2016s[!(as.numeric(data2016s$pop) %in% which(table(data2016s$pop)<5)),]
data2016s_5$pop<-droplevels(data2016s_5$pop)

#Keep pops where n>=4: 11 with vs 6 without predator
data2016s_4 <- data2016s[!(as.numeric(data2016s$pop) %in% which(table(data2016s$pop)<4)),]
data2016s_4$pop<-droplevels(data2016s_4$pop)

# Model 1: differences among populations? ####
with(data2016s_10, lineplot.CI(pop,stage0808))
with(data2016s_5, lineplot.CI(pop,stage0808))
with(data2016s_4, lineplot.CI(pop,stage0808))

with(data2016s_4, bargraph.CI(pop,stage0808,err.width=0.02,ylab="Max bud stage"))

m17<-lm(stage0808~pop,data=data2016s_10)
m18<-lm(stage0808~pop+ori_new,data=data2016s_10)
Anova(m17) 
Anova(m18) 
anova(m17,m18)
m19<-lme(stage0808 ~ pop, data = data2016s_10, random = ~ 1|ori_new)
Anova(m19) # NS
summary(m19)

m20<-lm(stage0808~pop,data=data2016s_4)
m21<-lm(stage0808~pop+ori_new,data=data2016s_4)
Anova(m20) 
Anova(m21) 
anova(m20,m21)
m22<-lme(stage0808 ~ pop, data = data2016s_4, random = ~ 1|ori_new)
Anova(m22) # p=0.05416
summary(m22)
#Post-hoc comparisons
m22c <- glht(m22, linfct=mcp(pop="Tukey"))
summary(m22c) 

#Conclusion: mostly no difs between pops after accounting for ori
#(except small effect when using data2016s_4)

#Group "Herr" and "Göt009" pops into the same
data2016s$pop_g<-data2016s$pop
lpop1 <- levels(data2016s$pop_g)
ind1 <- c(which(lpop1 == 'Göt009a'), which(lpop1 == 'Göt009b'))
levels(data2016s$pop_g)[ind1] <- 'Göt009'
lpop2 <- levels(data2016s$pop_g)
ind2 <- c(which(lpop2 == 'Her003'), which(lpop2 == 'Her004'), which(lpop2 == 'Her005'))
levels(data2016s$pop_g)[ind2] <- 'Her'

#Not better (code erased)

# Model 2: differences related to predation? ####
with(data2016s_10, lineplot.CI(pred,stage0808))
with(data2016s_4, lineplot.CI(pred,stage0808))

m23<-lme(stage0808 ~ pred, data = data2016s_10, random = ~ 1|pop/ori_new)
Anova(m23)
m24<-lme(stage0808 ~ pred, data = data2016s_4, random = ~ 1|pop/ori_new)
Anova(m24)

#No effect of predation (expected as no difs between pops)

#New analysis with first, mean, last flowering day & duration #####
data2016l<-read.table("./data/clean/phen_cgarden_2016_clean_long.txt",header=T,sep="\t",dec=".")

head(data2016l)
tail(data2016l)
str(data2016l)
data2016l$ori_new<-as.factor(data2016l$ori_new)
data2016l$pred<-as.factor(data2016l$pred)
data2016l$OBJECTID<-as.factor(data2016l$OBJECTID)
data2016l$date<-as.Date(data2016l$date,format="%d/%m/%Y")

#Calculate first, mean, last flowering day & duration

setDT(data2016l)[, date := as.Date(date)]

data2016l_first<-as.data.frame(dcast(data2016l, OBJECTID ~ fl, fun=min, fill=NA, value.var="date"))
str(data2016l_first)
head(data2016l_first)
data2016l_first<-data2016l_first[,c(1,3)]
names(data2016l_first)[names(data2016l_first)=="1"] <- "first"

data2016l_last<-as.data.frame(dcast(data2016l, OBJECTID ~ fl, fun=max, fill=NA, value.var="date"))
str(data2016l_last)
head(data2016l_last)
data2016l_last<-data2016l_last[,c(1,3)]
names(data2016l_last)[names(data2016l_last)=="1"] <- "last"

data2016l_firstlast<-merge(data2016l_first,data2016l_last)
head(data2016l_firstlast)
str(data2016l_firstlast)

data2016l<-data.frame(data2016l)

data2016l_calc<-merge(data2016l_firstlast,unique(data2016l[c("OBJECTID", "pop", "pred","ori_new")]),all.y=F)
head(data2016l_calc)
str(data2016l_calc)
data2016l_calc<-data2016l_calc[complete.cases(data2016l_calc),]
data2016l_calc$first_j<-as.integer(format(data2016l_calc$first, "%j"))
data2016l_calc$last_j<-as.integer(format(data2016l_calc$last, "%j"))
data2016l_calc$mean<-rowMeans(cbind(data2016l_calc$first_j,data2016l_calc$last_j))
data2016l_calc$dur<-as.integer(difftime(data2016l_calc$last ,data2016l_calc$first , units = c("days")))
#Sometimes zero (if only seen flowering one day) or very small values

#See distributions
with(data2016l_calc,hist(first_j))
with(data2016l_calc,hist(last_j))
with(data2016l_calc,hist(mean))
with(data2016l_calc,hist(dur))

cor(data2016l_calc[7:10]) #Correlations among the four measures

#Keep pops where n>=10: 7 with vs 1 without predator
data2016l_calc_10 <- data2016l_calc[!(as.numeric(data2016l_calc$pop) %in% which(table(data2016l_calc$pop)<10)),]
data2016l_calc_10$pop<-droplevels(data2016l_calc_10$pop)

#Keep pops where n>=5: 11 with vs 5 without predator
data2016l_calc_5 <- data2016l_calc[!(as.numeric(data2016l_calc$pop) %in% which(table(data2016l_calc$pop)<5)),]
data2016l_calc_5$pop<-droplevels(data2016l_calc_5$pop)

#Keep pops where n>=4: 11 with vs 6 without predator
data2016l_calc_4 <- data2016l_calc[!(as.numeric(data2016l_calc$pop) %in% which(table(data2016l_calc$pop)<4)),]
data2016l_calc_4$pop<-droplevels(data2016l_calc_4$pop)

# Model 1: differences among populations? ####
with(data2016l_calc_10, lineplot.CI(pop,first_j))
with(data2016l_calc_5, lineplot.CI(pop,first_j))
with(data2016l_calc_4, lineplot.CI(pop,first_j))

with(data2016l_calc_10, lineplot.CI(pop,last_j))
with(data2016l_calc_5, lineplot.CI(pop,last_j))
with(data2016l_calc_4, lineplot.CI(pop,last_j))

with(data2016l_calc_10, lineplot.CI(pop,mean))
with(data2016l_calc_5, lineplot.CI(pop,mean))
with(data2016l_calc_4, lineplot.CI(pop,mean))

with(data2016l_calc_4, lineplot.CI(pop,first_j,err.width=0.02,type="p",
    ylab="First flowering day",cex.axis=0.8,las=2))
with(data2016l_calc_4, lineplot.CI(pop,last_j,err.width=0.02,type="p",
    ylab="Last flowering day",cex.axis=0.8,las=2))
with(data2016l_calc_4, lineplot.CI(pop,mean,err.width=0.02,type="p",
    ylab="Mean flowering day",cex.axis=0.8,las=2))

m25<-lm(first_j~pop,data=data2016l_calc_10)
m26<-lm(first_j~pop+ori_new,data=data2016l_calc_10)
Anova(m25) 
Anova(m26) 
anova(m25,m26)#Only pop best
m27<-lme(first_j ~ pop, data = data2016l_calc_10, random = ~ 1|ori_new) 
Anova(m27) # pop significant!!!
summary(m27)
m27c <- glht(m27, linfct=mcp(pop="Tukey"))
summary(m27c) 

m28<-lm(first_j~pop,data=data2016l_calc_4)
m29<-lm(first_j~pop+ori_new,data=data2016l_calc_4)
Anova(m28) 
Anova(m29) 
anova(m28,m29)#Only pop best
m30<-lme(first_j ~ pop, data = data2016l_calc_4, random = ~ 1|ori_new)
Anova(m30) # pop significant!!!
summary(m30)
#Post-hoc comparisons
m30c <- glht(m30, linfct=mcp(pop="Tukey"))
summary(m30c) 

#Conclusion: difs between pops after accounting for ori (but not that many pairs difere)

m31<-lm(last_j~pop,data=data2016l_calc_10)
m32<-lm(last_j~pop+ori_new,data=data2016l_calc_10)
Anova(m31) 
Anova(m32) 
anova(m31,m32)#Only pop best
m33<-lme(last_j ~ pop, data = data2016l_calc_10, random = ~ 1|ori_new) 
Anova(m33) # pop significant!!!
summary(m33)
m33c <- glht(m33, linfct=mcp(pop="Tukey"))
summary(m33c) 

m34<-lm(last_j~pop,data=data2016l_calc_4)
m35<-lm(last_j~pop+ori_new,data=data2016l_calc_4)
Anova(m34) 
Anova(m35) 
anova(m34,m35)#Only pop best
m36<-lme(last_j ~ pop, data = data2016l_calc_4, random = ~ 1|ori_new)
Anova(m36) # pop significant!!!
summary(m36)
#Post-hoc comparisons
m36c <- glht(m36, linfct=mcp(pop="Tukey"))
summary(m36c) 

#Conclusion: difs between pops after accounting for ori (but not that many pairs difere)

m37<-lm(mean~pop,data=data2016l_calc_10)
m38<-lm(mean~pop+ori_new,data=data2016l_calc_10)
Anova(m37) 
Anova(m38) 
anova(m37,m38)#Only pop best
m39<-lme(mean ~ pop, data = data2016l_calc_10, random = ~ 1|ori_new) 
Anova(m39) # pop significant!!!
summary(m39)
m39c <- glht(m39, linfct=mcp(pop="Tukey"))
summary(m39c) 

m40<-lm(mean~pop,data=data2016l_calc_4)
m41<-lm(mean~pop+ori_new,data=data2016l_calc_4)
Anova(m40) 
Anova(m41) 
anova(m40,m41)#Only pop best
m42<-lme(mean ~ pop, data = data2016l_calc_4, random = ~ 1|ori_new)
Anova(m42) # pop significant!!!
summary(m42)
#Post-hoc comparisons
m42c <- glht(m42, linfct=mcp(pop="Tukey"))
summary(m42c) 

#Conclusion: difs between pops after accounting for ori (but not that many pairs difere)



# Model 2: differences related to predation? ####
with(data2016l_calc_10, lineplot.CI(pred,first_j))
with(data2016l_calc_4, lineplot.CI(pred,first_j))
with(data2016l_calc_10, lineplot.CI(pred,last_j))
with(data2016l_calc_4, lineplot.CI(pred,last_j))
with(data2016l_calc_10, lineplot.CI(pred,mean))
with(data2016l_calc_4, lineplot.CI(pred,mean))


m43<-lme(first_j ~ pred, data = data2016l_calc_10, random = ~ 1|pop/ori_new)
m44<-lme(first_j ~ pred, data = data2016l_calc_4, random = ~ 1|pop/ori_new)
m45<-lme(last_j ~ pred, data = data2016l_calc_10, random = ~ 1|pop/ori_new)
m46<-lme(last_j ~ pred, data = data2016l_calc_4, random = ~ 1|pop/ori_new)
m47<-lme(mean ~ pred, data = data2016l_calc_10, random = ~ 1|pop/ori_new)
m48<-lme(mean ~ pred, data = data2016l_calc_4, random = ~ 1|pop/ori_new)

Anova(m43)
Anova(m44)
Anova(m45)
Anova(m46)
Anova(m47)
Anova(m48)

#No effect of predation





