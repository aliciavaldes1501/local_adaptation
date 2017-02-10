library(ggplot2)
library(tidyr)

#Use data with 4+ pl flowering per pop in 2015/2016
data2015_4$year<-"2015"
data2016s_4$year<-"2016"
head(data2015_4)
head(data2016s_4)

d15<-data2015_4[c(1:4,7)]
d15$phen<-d15$most_adv
d15$most_adv<-NULL
head(d15)

d16<-data2016s_4[c(2,4,19:20,22)]
d16$phen<-d16$stage0808
d16$stage0808<-NULL
head(d16)
d16<-d16[,c(1,3,2,4,5)]

str(d15)
str(d16)
d1516<-rbind(d15,d16)
str(d1516)
d1516$year<-as.factor(d1516$year)
d1516$phen<-as.numeric(d1516$phen)
head(d1516)

pdf("./results/figures/difs_phen_yrs.pdf", family="Times",width=6,height=4)
bargraph.CI(pop,phen,group=year,data=d1516,legend=T,err.width=0.02,
            ylim=c(0,6),cex.names=0.8,cex.axis=0.8,ylab="Mean stage of most advanced bud",
            main="Stage noted on 4-6 Aug 2015 and 8 Aug 2016",las=2)
dev.off()

#Difs in mean phenology of each pop among years?
m49<-lm(phen~year*pop,data=subset(d1516,!pop=="Bor012"&!pop=="Sve005"))
m49<-lme(phen~year,random=~year|pop,data=subset(d1516,!pop=="Bor012"&!pop=="Sve005"))
Anova(m49)
summary(m49)
m49c <- glht(m49, linfct=mcp(year="Tukey"))
summary(m49c) 

dif1516_agg<-aggregate(d1516$phen,list(pop=d1516$pop,year=d1516$year),FUN=mean)
dif1516_agg <- spread(dif1516_agg, year, x)
dif1516_agg<-dif1516_agg[complete.cases(dif1516_agg),]
names(dif1516_agg)[names(dif1516_agg)=="2015"] <- "phen15"
names(dif1516_agg)[names(dif1516_agg)=="2016"] <- "phen16"
cor(dif1516_agg[2:3])
with(dif1516_agg,plot(phen15,phen16))
