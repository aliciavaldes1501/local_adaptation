library(car)

data2015_10<-read.table("./data/clean/data2015_10_short.txt",header=T,sep="\t",dec=",")

head(data2015_10)
tail(data2015_10)
str(data2015_10)

with(data2015_10,hist(phen))

with(data2015_10,Anova(lm(as.integer(phen)~pred+id%in%pop)))

with(data2015_10,Anova(lm(as.integer(phen)~pred+pop/ori/id)))
summary(with(data2015_10,lm(as.integer(phen)~pred+pop/ori/id)))

with(data2015_10,Anova(lm(as.integer(phen)~pred+id%in%pop+id%in%ori)))



