###Code to run the statistical analysis for Not silent yet: the shifting soundscapes of spring

###Catriona Morrison 2021

###Site-level changes in acoustic metrics

###libraries
library(lme4)
library(car)
library(MuMIn)

dat<-read.csv("metricsEuropemeans.csv")

####Calculate the mean and sd of each acoustic metric within each site

siteMAEI<-tapply(dat$AEIm,dat$SITE,mean)
siteSDAEI<-tapply(dat$AEIm,dat$SITE,sd)

siteMBI<-tapply(dat$BIm,dat$SITE,mean)
siteSDBI<-tapply(dat$BIm,dat$SITE,sd)

siteMADI<-tapply(dat$ADIm,dat$SITE,mean)
siteSDADI<-tapply(dat$ADIm,dat$SITE,sd)

siteMH<-tapply(dat$Hm,dat$SITE,mean)
siteSDH<-tapply(dat$Hm,dat$SITE,sd)

###Bring these means together
stands<-data.frame(SITE=rownames(siteMH),meansAEI=siteMAEI,sdsAEI=siteSDAEI,meansBI=siteMBI,sdsBI=siteSDBI,meansADI=siteMADI,sdsADI=siteSDADI,meansH=siteMH,sdsH=siteSDH)

###Add means to orginal dataset
dat<-merge(dat,stands,by='SITE')

###Make standardised metrics
dat$AEImS<-(dat$AEIm-dat$meansAEI)/dat$sdsAEI
dat$BImS<-(dat$BIm-dat$meansBI)/dat$sdsBI
dat$ADImS<-(dat$ADIm-dat$meansADI)/dat$sdsADI
dat$HmS<-(dat$Hm-dat$meansH)/dat$sdsH

###Create a catergorical year variable 
dat$YEARf<-as.factor(dat$YEAR)

###GLMMs

###Model with year as continuous variable
mod1ADI<-lmer(ADImS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modADI<-lmer(ADImS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1AEI<-lmer(AEImS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modAEI<-lmer(AEImS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1BI<-lmer(BImS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modBI<-lmer(BImS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1H<-lmer(HmS~LAT+LONG+YEAR1+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modH<-lmer(HmS~LAT+LONG+YEARf+(1|SITE)+(1|COUNTRY)+(1|YEAR1),dat=dat,REML=F)

###Example of predicting annual variation and trend from the GLMMs

boot <- lme4::bootMer(modADI, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$predseADI <- apply(boot$t, 2, sd)
newdata$predADI <- apply(boot$t, 2, function(x) mean(x))

boot1 <- lme4::bootMer(mod1ADI, 
                       function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                       nsim = 500, 
                       use.u = FALSE, 
                       type = 'parametric', 
                       parallel = 'multicore', 
                       ncpus = 4)

newdata$predse1ADI <- apply(boot1$t, 2, sd)
newdata$pred1ADI <- apply(boot1$t, 2, function(x) mean(x))

plot(predADI~YEAR,data=newdata,pch=20,ylim=c(-0.2,0.2),xlab='',ylab='Predicted standardised ADI (± 1SE)',type='b',cex.axis=1.5,cex.lab=1.5)
segments(newdata$YEAR,I(newdata$predADI+newdata$predseADI),newdata$YEAR,I(newdata$predADI-newdata$predseADI))
lines(pred1ADI~YEAR,data=newdata,lwd=2)
x1<-c(newdata$YEAR,rev(newdata$YEAR))
y1<-c(I(newdata$pred1ADI+newdata$predse1ADI),rev(I(newdata$pred1ADI-newdata$predse1ADI)))
polygon(x1,y1,col=rgb(128/255, 128/255, 128/255,alpha=0.4), border=NA)
mtext('(a)',side=3,line=1,at=1997.5,cex=1.5)


###North America

dat<-read.csv("metricsUSmeans.csv")

####Calculate the mean and sd of each acoustic metric within each site

siteMAEI<-tapply(dat$AEIm,dat$SITE,mean)
siteSDAEI<-tapply(dat$AEIm,dat$SITE,sd)

siteMBI<-tapply(dat$BIm,dat$SITE,mean)
siteSDBI<-tapply(dat$BIm,dat$SITE,sd)

siteMADI<-tapply(dat$ADIm,dat$SITE,mean)
siteSDADI<-tapply(dat$ADIm,dat$SITE,sd)

siteMH<-tapply(dat$Hm,dat$SITE,mean)
siteSDH<-tapply(dat$Hm,dat$SITE,sd)

###Bring these means together

stands<-data.frame(SITE=rownames(siteMH),meansAEI=siteMAEI,sdsAEI=siteSDAEI,meansBI=siteMBI,sdsBI=siteSDBI,meansADI=siteMADI,sdsADI=siteSDADI,meansH=siteMH,sdsH=siteSDH)

###Add means to orginal dataset
dat<-merge(dat,stands,by='SITE')

###Make standardised metrics
dat$AEImS<-(dat$AEIm-dat$meansAEI)/dat$sdsAEI
dat$BImS<-(dat$BIm-dat$meansBI)/dat$sdsBI
dat$ADImS<-(dat$ADIm-dat$meansADI)/dat$sdsADI
dat$HmS<-(dat$Hm-dat$meansH)/dat$sdsH

###Create a catergorical year variable 
dat$YEARf<-as.factor(dat$YEAR)

###GLMMs

###Model with year as continuous variable
mod1ADI<-lmer(ADImS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modADI<-lmer(ADImS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1AEI<-lmer(AEImS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modAEI<-lmer(AEImS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1BI<-lmer(BImS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modBI<-lmer(BImS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as continuous variable
mod1H<-lmer(HmS~LAT+LONG+YEAR1+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Model with year as categorical variable
modH<-lmer(HmS~LAT+LONG+YEARf+(1|STATE)+(1|ROUTE2)+(1|SITE)+(1|YEAR1),dat=dat,REML=F)

###Example of predicting annual variation and trend from the GLMMs

boot <- lme4::bootMer(modADI, 
                      function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                      nsim = 500, 
                      use.u = FALSE, 
                      type = 'parametric', 
                      parallel = 'multicore', 
                      ncpus = 4)

newdata$predseADI <- apply(boot$t, 2, sd)
newdata$predADI <- apply(boot$t, 2, function(x) mean(x))

boot1 <- lme4::bootMer(mod1ADI, 
                       function(x) predict(x, newdata = newdata, re.form = NA, type = 'response'), 
                       nsim = 500, 
                       use.u = FALSE, 
                       type = 'parametric', 
                       parallel = 'multicore', 
                       ncpus = 4)

newdata$predse1ADI <- apply(boot1$t, 2, sd)
newdata$pred1ADI <- apply(boot1$t, 2, function(x) mean(x))

plot(predADI~YEAR,data=newdata,pch=20,ylim=c(-0.2,0.2),xlab='',ylab='Predicted standardised ADI (± 1SE)',type='b',cex.axis=1.5,cex.lab=1.5)
segments(newdata$YEAR,I(newdata$predADI+newdata$predseADI),newdata$YEAR,I(newdata$predADI-newdata$predseADI))
lines(pred1ADI~YEAR,data=newdata,lwd=2)
x1<-c(newdata$YEAR,rev(newdata$YEAR))
y1<-c(I(newdata$pred1ADI+newdata$predse1ADI),rev(I(newdata$pred1ADI-newdata$predse1ADI)))
polygon(x1,y1,col=rgb(128/255, 128/255, 128/255,alpha=0.4), border=NA)
mtext('(a)',side=3,line=1,at=1997.5,cex=1.5)

###Example of test for spatial auto-correlation

library(ape) ## load SAC package

dat$mod.res <- residuals(modADI) ## export residuals

###Calculate morans I for each year
yyears<-sort(unique(dat$YEAR1))
M3<-NULL

for(i in 1:length(yyears)){
dat1<-dat[dat$YEAR1==yyears[i],]

# #### Create an inverse distance matrix
sac.df<-data.frame(long=dat1$LONG,lat=dat1$LAT,resid=dat1$mod.res)
datadis <- as.matrix(dist(cbind(sac.df$long, sac.df$lat)))
datadis.inv <- 1/datadis
diag(datadis.inv) <- 0
datadis.inv[is.infinite(datadis.inv)] <- 0

### Test for SAC

M1<-Moran.I(sac.df$resid, datadis.inv)

###Save results
M2<-data.frame(observed=M1[1],expected=M1[2],sd=M1[3],p.value=M1[4],year=yyears[i])
M3<-rbind(M3,M2)
print(i)

} ###end of i

### Site-level GLMs - Example for ADI
sites<-unique(dat$SITE)

allslopesADI<-NULL

for (i in 1:length(sites)){
  dat2<-dat[dat$SITE==sites[i],]
  mod<-lm(AEImS~YEAR,dat=dat2)
  slope<-summary(mod)$coefficients[2,1]
  ses<-summary(mod)$coefficients[2, 2]
  rsq<-summary(mod)$r.squared
  adjrsq<-summary(mod)$r.squared
  p.value<-pf(summary(mod)$fstatistic[1], summary(mod)$fstatistic[2],
              summary(mod)$fstatistic[3], lower.tail = FALSE)
  slopesAEI<-data.frame(Site2=sites[i],slope=slope,Std.Error=ses,rsq=rsq,adjrsq=adjrsq,p.value=p.value)

allslopesADI<-rbind(allslopesADI,slopesADI)
} ### end of i




