###Code to run the statistical analysis for Not silent yet: the shifting soundscapes of spring

###Catriona Morrison 2021

### Response to acoustic metrics to changes in community structure
### Multi-species community 

###Europe #######
###Bring in Europe data and take the mean over the 1000 itterations, save output in datEU

setwd("COMMUNITY_EU")

files<-list.files()

library(stringi)

noind<-stri_extract_first_regex(files, "[0-9]+")
nospps<-stri_extract_last(files, regex = "[0-9]+")


datEU2<-NULL
for (i in 1:70){
  dat<-read.table(files[i], header=T)
  dat<-data.frame(aei_left=colMeans(dat)[2],left_area=colMeans(dat)[3],adi_left=colMeans(dat)[4],H=colMeans(dat)[5],noind=as.numeric(noind[i]),nospps=as.numeric(nospps[i]))
  dat$run<-files[i]
  datEU2<-rbind(datEU2,dat)
}

str(datEU2)

datEU<-datEU2


### North America
###Bring in North America data and take the mean over the 1000 itterations, save output in datUS
setwd("~/Dropbox/UEA/Soundscape/Data/VALIDATION_0221/COMMUNITY_US")

files<-list.files()

noind<-stri_extract_first_regex(files, "[0-9]+")
nospps<-stri_extract_last(files, regex = "[0-9]+")


datUS2<-NULL
for (i in 1:70){
  dat<-read.table(files[i], header=T)
  dat<-data.frame(aei_left=colMeans(dat)[2],left_area=colMeans(dat)[3],adi_left=colMeans(dat)[4],H=colMeans(dat)[5],noind=as.numeric(noind[i]),nospps=as.numeric(nospps[i]))
  dat$run<-files[i]
  datUS2<-rbind(datUS2,dat)
}

str(datUS2)

datUS<-datUS2

###GLMs
datUS$noind<-log(datUS$noind)
datEU$noind<-log(datEU$noind)

datUS$nospps<-log(datUS$nospps)
datEU$nospps<-log(datEU$nospps)

modAEIUS<-lm(aei_left~noind+nospps,data=datUS)
modAEIEU<-lm(aei_left~noind+nospps,data=datEU)

modBIUS<-lm(left_area~noind*nospps,data=datUS)
modBIEU<-lm(left_area~noind*nospps,data=datEU)

modADI<-lm(adi_left~noind*nospps,data=datUS)
modADI<-lm(adi_left~noind*nospps,data=datEU)

modHUS<-lm(H~noind*nospps,data=datUS)
modHEU<-lm(H~noind*nospps,data=datEU)


####Single species

dat1<-read.table("EUmerged_SPECIES.txt",header=T)

###Calculate the mean and se of each acoustic metric (across the 100 runs) for each species for each number of individuals.

aei1<-tapply(dat1$AEI,list(dat1$species,dat1$count),mean,na.rm=T)
BI1<-tapply(dat1$BI,list(dat1$species,dat1$count),mean,na.rm=T)
adi1<-tapply(dat1$ADI,list(dat1$species,dat1$count),mean,na.rm=T)
H1<-tapply(dat1$H,list(dat1$species,dat1$count),mean,na.rm=T)

se<-function(x){sd(x)/sqrt(length(x))}

sesH1<-sapply(tapply(H1,col(H1), na.exclude), se)
sesadi1<-sapply(tapply(adi1,col(H1), na.exclude), se)
sesaei1<-sapply(tapply(aei1,col(H1), na.exclude), se)
sesBI1<-sapply(tapply(BI1,col(H1), na.exclude), se)

###Bring data together - one row for each species
varrsEU<-data.frame(aei=c(aei1),BI=c(BI1),H=c(H1),adi=c(adi1),inds=rep(c(1:10,20,30,40,50),each=446))

###North America

dat1<-read.table("USmerged_SPECIES.txt",header=T)
names(dat1)[2]<-'count'

###Calculate the mean and se of each acoustic metric (across the 100 runs) for each species for each number of individuals.

aei1<-tapply(dat1$AEI,list(dat1$species,dat1$count),mean,na.rm=T)
BI1<-tapply(dat1$BI,list(dat1$species,dat1$count),mean,na.rm=T)
adi1<-tapply(dat1$ADI,list(dat1$species,dat1$count),mean,na.rm=T)
H1<-tapply(dat1$H,list(dat1$species,dat1$count),mean,na.rm=T)

se<-function(x){sd(x)/sqrt(length(x))}
sesH1<-sapply(tapply(H1,col(H1), na.exclude), se)
sesadi1<-sapply(tapply(adi1,col(H1), na.exclude), se)
sesaei1<-sapply(tapply(aei1,col(H1), na.exclude), se)
sesBI1<-sapply(tapply(BI1,col(H1), na.exclude), se)

###Bring data together - one row for each species
varrsEU<-data.frame(aei=c(aei1),BI=c(BI1),H=c(H1),adi=c(adi1),inds=rep(c(1:10,20,30,40,50),each=620))

###GLMs
varrsEU$indsl<-log(varrsEU$inds)
varrsUS$indsl<-log(varrsUS$inds)

modAEIUS<-lm(aei~indsl,data=varrsUS)
modAEIEU<-lm(aei~indsl,data=varrsEU)

modBIUS<-lm(BI~indsl,data=varrsUS)
modBIEU<-lm(BI~indsl,data=varrsEU)

modADIUS<-lm(adi~indsl,data=varrsUS)
modADIEU<-lm(adi~indsl,data=varrsEU)

modHUS<-lm(H~indsl,data=varrsUS)
modHEU<-lm(H~indsl,data=varrsEU)

