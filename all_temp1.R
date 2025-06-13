###############################################################################
# CREATE 3 OBJECTS:
#
# 1) A VECTOR WITH NAMES OF REGIONS OF ENGLAND AND WALES
#
# 2) A LIST WITH THE DATA FOR EACH REGION, INCLUDING:
#   - DATE, YEAR, MONTH, DAY, TIME, DAY OF THE YEAR, DAY OF THE WEEK
#   - REGION NUMBERS AND NAMES
#   - MEAN, MINIMUM AND MAXIMUM TEMPERATURE
#   - DEW-POINT TEMPERATURE AND RELATIVE HUMIDITY
#   - MORTALITY (ALL-CAUSE)
#
# 3) A FUNCTION TO COMPUTE THE Q-AIC
#
###############################################################################

# LOAD PACKAGES 
library(dlnm) ; library(mvmeta) ; library(splines)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASET
#data <- read.csv("data.csv",row.names=1)

library(readxl)
All2 <- read_excel("~/Desktop/Vyy/Projects - đang làm/farmer/data/data2mien/All2.xlsx")
View(All2)


data<-All2
dim(data)
head(data)

data$dow<-weekdays(data$date)
data$time <- seq(nrow(data))

# REGIONS
regions <- as.character(unique(data$regnames))

# CREATE A LIST WITH THE REGIONAL SERIES
data <- lapply(regions,function(x) data[data$regnames==x,])
names(data) <- regions
m <- length(regions)

# TEMPERATURE RANGES
ranges <- t(sapply(data, function(x) range(x$tmean,na.rm=T)))

####################################################################

# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

sum(data$hos2)
#

####################################################################
# RUN FIRST-STAGE ANALYSIS FOR A SINGLE REGION
####################################################################
# 1. Dien Bien
# SELECT REGION
reg <- "DB"
sum(data[[reg]]$hos2)
min(data[[reg]]$hos2)
max(data[[reg]]$hos2)
## Descriptive analysis
Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
summary
sd

sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
summary
sd

sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
summary
sd

sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

summary
sd

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  between weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR ALL, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2005-2015",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2005-2015",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2005-2015",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```
# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos2 ~ cb + dow + ns(time,df=11*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)


#all-causes
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(hos2~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

predglm1 $allRRfit[predglm1 $predvar==29]
predglm1 $allRRlow[predglm1 $predvar==29]
predglm1 $allRRhigh[predglm1 $predvar==29]
#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=28), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.6,1.8),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(predglm1,var=29,xlab="Lag (days)",ylab="RR",ylim=c(0.95,1.05),lwd=1.5,
     main="lag-specific association at temperature=29oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()

#
# Hospitalization Coefs and SD (1oC above MMT==28)
```{r}
hospital_db<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_db)<-c("RR","RRlow","RRhigh")
rownames(hospital_db)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,11*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=28)

hospital_db[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="29"],3)
hospital_db[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="29"],3)
hospital_db[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="29"],3)

write.csv(hospital_db,"hospitaldb coefs.csv")

```

####################################################################
# 2. Tuyen Quang
# SELECT REGION
reg <- "TQ"
sum(data[[reg]]$hos2)

Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  betweens weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR all, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2010-2015",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2010-2015",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2010-2015",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```
# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos1 ~ cb + dow + ns(time,df=6*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1,cen=26)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2,cen=26)
crlag <- crossreduce(cb,model,type="lag",value=3,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)
crvar <- crossreduce(cb,model,type="var",value=29,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)

##
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(hos1~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#tang 2 do
pred1 <- crosspred(cbglm1,glm1, at=20.4)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#tang 4 do from 18.4
pred1 <- crosspred(cbglm1,glm1, at=22.4)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow


pred1$allRRfit[pred1$predvar==29]
pred1$allRRlow[pred1$predvar==29]
pred1$allRRhigh[pred1$predvar==29]

#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=19), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=20)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.4,2.0),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(predglm1,var=27,xlab="Lag (days)",ylab="RR",ylim=c(0.95,1.05),lwd=1.5,
     main="lag-specific association at temperature=27oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()

#
# Hospitalization Coefs and SD (1oC above MMT==19)
```{r}
hospital_tq<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_tq)<-c("RR","RRlow","RRhigh")
rownames(hospital_tq)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=19)

hospital_tq[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="20"],3)
hospital_tq[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="20"],3)
hospital_tq[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="20"],3)

write.csv(hospital_tq,"hospitaltq coefs.csv")

```
####################################################################
# 3.
# SELECT REGION
reg <- "HT"
sum(data[[reg]]$hos2)

Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  betweens weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR all, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2017-2020",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2017-2020",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2017-2020",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```

# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos2 ~ cb + dow + ns(time,df=4*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1,cen=26)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2,cen=26)
crlag <- crossreduce(cb,model,type="lag",value=3,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)
crvar <- crossreduce(cb,model,type="var",value=29,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)

##
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(hos2~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)



# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

predglm1$allRRfit[predglm1$predvar==32]
predglm1$allRRlow[predglm1$predvar==32]
predglm1$allRRhigh[predglm1$predvar==32]

#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=17), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=18)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.4,2.0),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(predglm1,var=18,xlab="Lag (days)",ylab="RR",ylim=c(0.96,1.04),lwd=1.5,
     main="lag-specific association at temperature=18oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()

# Hospitalizarion Coefs and SD (1oC above MMT==26)
```{r}
hospital_ht<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_ht)<-c("RR","RRlow","RRhigh")
rownames(hospital_ht)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=17)

hospital_ht[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="18"],3)
hospital_ht[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="18"],3)
hospital_ht[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="18"],3)

write.csv(hospital_ht,"hospitalht coefs.csv")

```

####################################################################
# 4.
# SELECT REGION
reg <- "VL"
sum(data[[reg]]$hos2)

Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  betweens weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR all, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2006-2013",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2006-2013",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2006-2013",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```


# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos2 ~ cb + dow + ns(time,df=8*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1,cen=26)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2,cen=26)
crlag <- crossreduce(cb,model,type="lag",value=3,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)
crvar <- crossreduce(cb,model,type="var",value=29,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)

##
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(hos2~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)



# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

predglm1$allRRfit[predglm1$predvar==32]
predglm1$allRRlow[predglm1$predvar==32]
predglm1$allRRhigh[predglm1$predvar==32]

#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25.5), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.2,1.6),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(pred1,var=26.5,xlab="Lag (days)",ylab="RR",,ylim=c(0.96,1.04),lwd=1.5,
     main="lag-specific association at temperature=26.5oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()


#
# Hospitalizarion Coefs and SD (1oC above MMT==25.5)
```{r}
hospital_vl<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_vl)<-c("RR","RRlow","RRhigh")
rownames(hospital_vl)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,8*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25.5)

hospital_vl[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="26.5"],3)
hospital_vl[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="26.5"],3)
hospital_vl[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26.5"],3)

write.csv(hospital_vl,"hospitalvl coefs.csv")

```

####################################################################
# 5.
# SELECT REGION
reg <- "AG"
sum(data[[reg]]$hos2)

Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  between weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR ALL, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2011-2014",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2011-2014",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2011-2014",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```

# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos2 ~ cb + dow + ns(time,df=4*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1,cen=26)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2,cen=26)
crlag <- crossreduce(cb,model,type="lag",value=3,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)
crvar <- crossreduce(cb,model,type="var",value=29,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)

##
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(hos2~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)



# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#tang 4 do
# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=29.5)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

predglm1$allRRfit[predglm1$predvar==32]
predglm1$allRRlow[predglm1$predvar==32]
predglm1$allRRhigh[predglm1$predvar==32]

#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=25), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=26)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.4,2.4),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(pred1,var=26.2,xlab="Lag (days)",ylab="RR",ylim=c(0.96,1.04),lwd=1.5,
     main="lag-specific association at temperature=26.2oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()

# Hospitalization Coefs and SD (1oC above MMT==25)
```{r}
hospital_ag<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_ag)<-c("RR","RRlow","RRhigh")
rownames(hospital_ag)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,4*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=25)

hospital_ag[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="26"],3)
hospital_ag[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="26"],3)
hospital_ag[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="26"],3)

write.csv(hospital_ag,"hospitalag coefs.csv")

```
#

####################################################################
# 6.
# SELECT REGION
reg <- "BP"
sum(data[[reg]]$hos2)

Table 1. Descriptive statistics
```{r}
library(Hmisc)
describe(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

summary(data[[reg]]$hos2)
sd(data[[reg]]$hos2,na.rm = TRUE)

summary(data[[reg]]$tmean)
sd(data[[reg]]$tmean,na.rm = TRUE)

summary(data[[reg]]$tmin)
sd(data[[reg]]$tmin,na.rm = TRUE)

summary(data[[reg]]$tmax)
sd(data[[reg]]$tmax,na.rm = TRUE)

summary(data[[reg]]$rh)
sd(data[[reg]]$rh,na.rm = TRUE)

summary(data[[reg]]$rain)
sd(data[[reg]]$rain,na.rm = TRUE)

sum(data[[reg]]$hos)

sum(data[[reg]]$infect)
sum(data[[reg]]$infect)/sum(data[[reg]]$hos)
sum(data[[reg]]$res)
sum(data[[reg]]$res)/sum(data[[reg]]$hos)
sum(data[[reg]]$cardi)
sum(data[[reg]]$cardi)/sum(data[[reg]]$hos)
sum(data[[reg]]$mental)
sum(data[[reg]]$mental)/sum(data[[reg]]$hos)

sum(data[[reg]]$icd_AB)
summary(data[[reg]]$icd_AB)
sd(data[[reg]]$icd_AB)

sum(data[[reg]]$icd_J)
summary(data[[reg]]$icd_J)
sd(data[[reg]]$icd_J)

sum(data[[reg]]$icd_F)
summary(data[[reg]]$icd_F)
sd(data[[reg]]$icd_F)

sum(data[[reg]]$icd_I)
summary(data[[reg]]$icd_I)
sd(data[[reg]]$icd_I)

sum(data[[reg]]$icd_E)
summary(data[[reg]]$icd_E)
sd(data[[reg]]$icd_E)

sum(data[[reg]]$icd_K)
summary(data[[reg]]$icd_K)
sd(data[[reg]]$icd_K)

sum(data[[reg]]$icd_L)
summary(data[[reg]]$icd_L)
sd(data[[reg]]$icd_L)

```

Figure 1. Histograms, scatter plots and Correlation coefficients  betweens weather conditions and hospitalization
```{r}
require("psych")
#pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])
pairs.panels(data[[reg]][,c("hos","tmean","tmin","tmax","rh")])
corr.test(data[[reg]][,c("hos","tmean","tmin","tmax","rh", "rain")])

dev.off()
```
Figure 2. Time series plots of weather variables and all-cause hospitalization
```{r}
#Case and tmean
oldpar <- par(no.readonly=TRUE)
par(mex=0.8,mfrow=c(3,1))

# SUB-PLOT FOR all, WITH VERTICAL LINES DEFINING YEARS
plot(data[[reg]]$hos2~as.Date(data[[reg]]$date),type="l",main="Number of hospital admissions for all causes 2008-2013",
     ylab="Cases",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR mean temperature 
plot(data[[reg]]$tmean~as.Date(data[[reg]]$date),type="l",main="Average temperature 2008-2013",
     ylab="Temperature",xlab="Date")

abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
# rh and rain 
#oldpar <- par(no.readonly=TRUE)
#par(mex=0.8,mfrow=c(2,1))

# THE SAME FOR humidity
plot(data[[reg]]$rh~as.Date(data[[reg]]$date),type="l",main="Average humidity 2008-2013",
     ylab="Humidity",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)

# THE SAME FOR rain
plot(data[[reg]]$rain~as.Date(data[[reg]]$date),type="l",main="Average rainfall 2008-2012",
     ylab="Rainfall",xlab="Date")
abline(v=as.Date(data[[reg]]$date[grep("-01-01",data[[reg]]$date)]),col=grey(0.6),lty=2)

abline(v=as.Date(data[[reg]]$date[grep("-05-01",data[[reg]]$date)]),col="red",lty=2)
abline(v=as.Date(data[[reg]]$date[grep("-07-31",data[[reg]]$date)]),col="red",lty=2)
dev.off()
```

# ARGUMENTS AND LISTS FOR CROSS-BASIS DEFINITION
bound <- colMeans(ranges)
varknots <- equalknots(data[[reg]]$tmean,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots)
arglag <- list(fun="ns",knots=lagknots)

#vk <- equalknots(hos2.data$Temp,degree=2, nk=3)
#lk <- logknots(7,nk=3)
#cbglm1 <- crossbasis(hos2.data$Temp, lag=7, argvar=list(fun="bs",degree=2,
#knots=vk), arglag=list(knots=lk))

# BASIS FOR TEMPERATURE:
# - QUADRATIC SPLINE FOR PREDICTOR, WITH SPECIFIC KNOT SELECTION
# - NATURAL CUBIC SPLINE FOR LAG, WITH DF AT EQUALLY-SPACED LOG-VALUES
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
suppressWarnings(
  cb <- crossbasis(data[[reg]]$tmean,lag=21,argvar=argvar,arglag=arglag)
)
summary(cb)

# RUN THE MODEL
model <- glm(hos2 ~ cb + dow + ns(time,df=6*4)+ ns(rh,df=3),
             family=quasipoisson(),data[[reg]])


range<-quantile(data[[reg]]$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

# PREDICTION USING:
#   crosspred FOR BI-DIMENSIONAL RELATIONSHIP
#   crossreduce FOR UNI-DIMENSIONAL SUMMARIES
# (NB: CENTERING AT SPECIFIC TEMPERATURE VALUE)
# (NB: ALL THE ESTIMATES ARE ALREADY REPORTED BY crosspred ALONE)

cp <- crosspred(cb,model,from=bound[1],to=bound[2],by=1,cen=26)
crall <- crossreduce(cb,model,from=bound[1],to=bound[2],by=0.2,cen=26)
crlag <- crossreduce(cb,model,type="lag",value=3,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)
crvar <- crossreduce(cb,model,type="var",value=29,from=bound[1],to=bound[2],
                     bylag=0.2,cen=26)

##
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(hos2~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)



# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

predglm1$allRRfit[predglm1$predvar==32]
predglm1$allRRlow[predglm1$predvar==32]
predglm1$allRRhigh[predglm1$predvar==32]

#icd_AB
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_AB~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_J
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_J~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow

#icd_F
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_F~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_I
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_I~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_E
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_E~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_K
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_K~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
#icd_L
cbglm1 <- crossbasis(data[[reg]]$tmean, lag=21, argvar=list(fun="bs",degree=2,
                                                            knots=varknots,cen=26), arglag=arglag)
glm1 <- glm(icd_L~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)


# ESTIMATED EFFECTS AT EACH LAG
pred1 <- crosspred(cbglm1,glm1, at=27)
tablag1 <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
tablag1

#RR cua tung nhiet do (commulative lags)
pred1$allRRfit
pred1$allRRhigh
pred1$allRRlow
# PLOTS

plot(predglm1,"overall",col="red",ylab="RR",ylim=c(0.4,1.6),xlab="Temperature (C)",lwd=1.5,main="Temp-hos2pitalization overall")
plot(pred1,var=27,xlab="Lag (days)",ylab="RR",ylim=c(0.96,1.04),lwd=1.5,
     main="lag-specific association at temperature=27oC")

#pdf("figure1.pdf",height=6,width=8.5)
par(mar=c(1.5,1,0,0)+0.1,cex.axis=0.9,cex.lab=1)
layout(matrix(rep(1:4,each=2),2,4,byrow=TRUE))

# 3D PLOT WITH DIFFERENT NON-DEFAULT PERSPECTIVE AND GREY SURFACE LINES
d3 <- plot(cp,xlab="Temperature (C)",zlab="RR",phi=35,theta=205,ltheta=170,
           shade=0.4)

# LINES IN THE SURFACE CORRESPONDING TO THE EFFECTS IN THE PLOTS BELOW
lines(trans3d(x=28,y=0:21,z=cp$matRRfit[as.character(28),],
              pmat=d3),lwd=2)
lines(trans3d(x=29,y=0:21,z=cp$matRRfit[as.character(29),],
              pmat=d3),lwd=2,col=2)
lines(trans3d(x=cp$predvar,y=3,z=cp$matRRfit[,"lag3"],
              pmat=d3),lwd=2,col=2)

par(mar=c(5,4,1,1)+0.1,mgp=c(2.5,1,0))

# PLOTS FOR PREDICTOR-SPECIFIC, LAG-SPECIFIC AND OVERALL CUMULATIVE SUMMARIES
plot(crvar,xlab="Lag",ylab="RR",col=2,lwd=2)
mtext(text=paste("Predictor-specific association at temperature ",29,
                 "C",sep=""),cex=0.7)
plot(crlag,xlab="Temperature (C)",ylab="RR",col=2,ylim=c(.96,1.06),lwd=2)
mtext(text="Lag-specific association at lag 3",cex=0.7)
plot(crall,xlab="Temperature (C)",ylab="RR",ylim=c(.8,2),col=2,lwd=2)
mtext(text="Overall cumulative association",cex=0.7)

dev.off()
###
                   
# Hospitalization Coefs and SD (1oC above MMT==25)
```{r}
hospital_bp<-matrix(NA,nrow=36,ncol=3)
colnames(hospital_bp)<-c("RR","RRlow","RRhigh")
rownames(hospital_bp)<-c("all","icd_AB","icd_D","icd_E","icd_F", "icd_I","icd_J", "icd_K", "icd_L", "icd_N", 
                         "icd_O", "icd_P","d_0060","d_61",
                         
                         
                         "icd_AB_0060","icd_AB_61","icd_D_0060","icd_D_61","icd_E_0060","icd_E_61","icd_F_0060",
                         "icd_F_61","icd_I_0060","icd_I_61","icd_J_0060","icd_J_61", "icd_K_0060","icd_K_61", 
                         "icd_L_0060","icd_L_61", "icd_N_0060",  "icd_N_61",
                         "icd_O_0060","icd_O_61", "icd_P_0060","icd_P_61")

# All-cause
glm1 <- glm(hos2~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[1,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[1,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[1,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)

#"icd_AB"
glm1 <- glm(icd_AB~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[2,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[2,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[2,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_D",
glm1 <- glm(icd_D~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[3,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[3,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[3,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_E"
glm1 <- glm(icd_E~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[4,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[4,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[4,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_F"
glm1 <- glm(icd_F~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[5,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[5,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[5,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_I"
glm1 <- glm(icd_I~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[6,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[6,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[6,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_J"
glm1 <- glm(icd_J~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[7,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[7,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[7,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_K"
glm1 <- glm(icd_K~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[8,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[8,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[8,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_L"
glm1 <- glm(icd_L~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[9,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[9,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[9,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_N"
glm1 <- glm(icd_N~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[10,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[10,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[10,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_O"
glm1 <- glm(icd_O~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[11,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[11,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[11,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_P"
glm1 <- glm(icd_P~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[12,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[12,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[12,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)

#"d_0060"
glm1 <- glm(d_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[13,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[13,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[13,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"d_61"
glm1 <- glm(d_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[14,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[14,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[14,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)

###d_0060
#"icd_AB_0060",
glm1 <- glm(icd_AB_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[15,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[15,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[15,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_AB_61",
glm1 <- glm(icd_AB_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[16,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[16,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[16,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_D_0060",
glm1 <- glm(icd_D_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[17,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[17,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[17,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_D_61",
glm1 <- glm(icd_D_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[18,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[18,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[18,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_E_0060",
glm1 <- glm(icd_E_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[19,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[19,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[19,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_E_61",
glm1 <- glm(icd_E_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[20,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[20,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[20,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_F_0060",
glm1 <- glm(icd_F_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[21,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[21,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[21,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_F_61",
glm1 <- glm(icd_F_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[22,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[22,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[22,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_I_0060",
glm1 <- glm(icd_I_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[23,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[23,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[23,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_I_61",
glm1 <- glm(icd_I_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[24,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[24,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[24,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_J_0060",
glm1 <- glm(icd_J_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[25,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[25,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[25,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_J_61",
glm1 <- glm(icd_J_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[26,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[26,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[26,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_K_0060",
glm1 <- glm(icd_K_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[27,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[27,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[27,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_K_61", 
glm1 <- glm(icd_K_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[28,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[28,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[28,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_L_0060",
glm1 <- glm(icd_L_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[29,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[29,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[29,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_L_61", 
glm1 <- glm(icd_L_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[30,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[30,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[30,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_N_0060", 
glm1 <- glm(icd_N_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[31,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[31,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[31,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_N_61",
glm1 <- glm(icd_N_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[32,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[32,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[32,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_O_0060",
glm1 <- glm(icd_O_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[33,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[33,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[33,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_O_61", 
glm1 <- glm(icd_O_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[34,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[34,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[34,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_P_0060",
glm1 <- glm(icd_P_0060~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[35,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[35,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[35,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)
#"icd_P_61"
glm1 <- glm(icd_P_61~cbglm1+ns(time,6*4)+dow+ ns(rh,df=3),family=quasipoisson(),data[[reg]])
predglm1 <- crosspred(cbglm1,cumul=T,glm1,by=0.1, cen=26)

hospital_bp[36,1]<-round(predglm1$allRRfit[predglm1$predvar=="27"],3)
hospital_bp[36,2]<-round(predglm1$allRRlow[predglm1$predvar=="27"],3)
hospital_bp[36,3]<-round(predglm1$allRRhigh[predglm1$predvar=="27"],3)

write.csv(hospital_bp,"hospitalbp coefs.csv")

```
