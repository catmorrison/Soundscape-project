### Code to run the statistical analysis for Not silent yet: the shifting soundscapes of spring

### Catriona Morrison 2021

### Site-level changes in the total number of individuals and species

### Europe

### libraries
library(lme4)
library(AICcmodavg)


counts<-read.csv('Europecounts.csv')


###Calculate the total number of indiviudals and species at each site in each year
ind<-tapply(counts$COUNT,list(counts$Site2,counts$YEAR),sum,na.rm=T)

counts$tab<-1
spps<-tapply(counts$tab,list(counts$Site2,counts$YEAR),sum,na.rm=T)

ind<-data.frame(ind=c(ind),spps=c(spps),YEAR=rep(colnames(ind),each=dim(ind)[1]),Site2=rep(row.names(ind),dim(ind)[2]))
###
ind<-ind[!is.na(ind$ind),]

###Calculate the mean and sd of the total number of species and indivduals at each site
siteMind<-data.frame(mmeansind=tapply(ind$ind,ind$Site2,mean,na.rm=T),sdsind=tapply(ind$ind,ind$Site2,sd,na.rm=T),Site2=row.names(tapply(ind$ind,ind$Site2,sd,na.rm=T)),mmeansspps=tapply(ind$spps,ind$Site2,mean,na.rm=T),sdsspps=tapply(ind$spps,ind$Site2,sd,na.rm=T))

###Add means to original dataset
ind2<-merge(ind,siteMind,by='Site2')

###Calculate standardised total number of individuals and species at each site in each year
ind2$indS<-(ind2$ind-ind2$mmeansind)/ind2$sdsind
ind2$sppsS<-(ind2$spps-ind2$mmeansspps)/ind2$sdsspps

dat<-ind2

### Add on lat and longs
datLL<-read.csv("metricsEuropemeans.csv")

names(datLL)[1]<-'Site2'
LL$Site2<-paste(datLL$Site2,'.txt',sep='')

dat3<-merge(dat,datLL,by=c('Site2','YEAR'))
 
###Make a catergorical year variable
dat3$YEARf<-as.factor(dat3$YEAR)

###GLMMS

###Individuals model with year as continuous variable
mod1indS<-lmer(indS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Individuals model with year as categorical variable
modindS<-lmer(indS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Species model with year as continuous variable
mod1sppsS<-lmer(sppsS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Species model with year as categorical variable
modsppsS<-lmer(sppsS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Example of predicting annual variation and trend from the GLMMs
newdata<-data.frame(LAT=mean(dat3$LAT),LONG=mean(dat3$LONG),YEARf=sort(unique(dat3$YEARf)),YEAR=as.numeric(as.character(sort(unique(dat3$YEARf)))),YEAR1=sort(unique(dat3$YEAR1)))

boot <- lme4::bootMer(mod1indS, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$pred1se <- apply(boot$t, 2, sd)
newdata$pred1 <- apply(boot$t, 2, function(x) mean(x))

boot <- lme4::bootMer(modindS, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$predse <- apply(boot$t, 2, sd)
newdata$pred <- apply(boot$t, 2, function(x) mean(x))


### North America
counts<-read.csv('UScounts.csv')

###Calculate the total number of indiviudals and species at each site in each year
ind<-tapply(counts$COUNT,list(counts$Site2,counts$YEAR),sum,na.rm=T)

counts$tab<-1
spps<-tapply(counts$tab,list(counts$Site2,counts$YEAR),sum,na.rm=T)

ind<-data.frame(ind=c(ind),spps=c(spps),YEAR=rep(colnames(ind),each=dim(ind)[1]),Site2=rep(row.names(ind),dim(ind)[2]))
###
ind<-ind[!is.na(ind$ind),]

###Calculate the mean and sd of the total number of species and indivduals at each site
siteMind<-data.frame(mmeansind=tapply(ind$ind,ind$Site2,mean,na.rm=T),sdsind=tapply(ind$ind,ind$Site2,sd,na.rm=T),Site2=row.names(tapply(ind$ind,ind$Site2,sd,na.rm=T)),mmeansspps=tapply(ind$spps,ind$Site2,mean,na.rm=T),sdsspps=tapply(ind$spps,ind$Site2,sd,na.rm=T))

###Add means to original dataset
ind2<-merge(ind,siteMind,by='Site2')

###Calculate standardised total number of individuals and species at each site in each year
ind2$indS<-(ind2$ind-ind2$mmeansind)/ind2$sdsind
ind2$sppsS<-(ind2$spps-ind2$mmeansspps)/ind2$sdsspps

dat<-ind2

### Add on lat and longs
datLL<-read.csv("metricsUSmeans.csv")

names(datLL)[1]<-'Site2'
LL$Site2<-paste(datLL$Site2,'.txt',sep='')

dat3<-merge(dat,datLL,by=c('Site2','YEAR'))

###Make a catergorical year variable
dat3$YEARf<-as.factor(dat3$YEAR)

###GLMMS

###Individuals model with year as continuous variable
mod1indS<-lmer(indS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|Site2)+(1|YEAR1),dat=dat,REML=F)

###Individuals model with year as categorical variable
modindS<-lmer(indS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|Site2)+(1|YEAR1),dat=dat,REML=F)

###Species model with year as continuous variable
mod1sppsS<-lmer(sppsS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|Site2)+(1|YEAR1),dat=dat,REML=F)

###Species model with year as categorical variable
modsppsS<-lmer(sppsS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|Site2)+(1|YEAR1),dat=dat,REML=F)

###Example of predicting annual variation and trend from the GLMMs
newdata<-data.frame(LAT=mean(dat3$LAT),LONG=mean(dat3$LONG),YEARf=sort(unique(dat3$YEARf)),YEAR=as.numeric(as.character(sort(unique(dat3$YEARf)))),YEAR1=sort(unique(dat3$YEAR1)))

boot <- lme4::bootMer(mod1indS, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$pred1se <- apply(boot$t, 2, sd)
newdata$pred1 <- apply(boot$t, 2, function(x) mean(x))

boot <- lme4::bootMer(modindS, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$predse <- apply(boot$t, 2, sd)
newdata$pred <- apply(boot$t, 2, function(x) mean(x))



### Site-level GLMs
dat$YEAR<-as.numeric(as.character(dat$YEAR))

allslopes<-NULL

for (i in 1:length(sites)){
  dat2<-dat[dat$Site2==sites[i],]
  modS<-glm(sppsS~YEAR,dat=dat2)
  slopeS<-summary(modS)$coefficients[2,1]
  sesS<-summary(modS)$coefficients[2, 2]
  modIND<-glm(indS~YEAR,dat=dat2)
  slopeIND<-summary(modIND)$coefficients[2,1]
  sesIND<-summary(modIND)$coefficients[2, 2]
  slopes<-data.frame(Site2=sites[i],slopeS=slopeS,Std.ErrorS=sesS,slopeIND=slopeIND,Std.ErrorIND=sesIND)
  allslopes<-rbind(allslopes,slopes)
  print(i)
}





