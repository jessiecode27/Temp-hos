
####################################################################
# FIRST STAGE
# - DEFINE THE CROSS-BASIS MATRICES FOR THE 3 MODELS
# - BUILD OBJECTS TO STORE THE RESULTS
# - RUN THE POISSON TIME SERIES MODELS
# - REDUCE THE FITTED MAIN MODEL TO SUMMARIES
# - STORE THE RESULTS
# COMPUTING TIME IS ~40SEC (IN A 2.66GHz-4GBRAM PC WITH WINDOWS)
####################################################################
#


# LOAD PACKAGES (ASSUMED ALREADY INSTALLED)
library(dlnm) ; library(mvmeta) ; library(splines)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATASET
#data <- read.csv("data.csv",row.names=1)
library(readxl)
All2 <- read_excel("Documents/OneDrive - UMP/Projects/farmer/data/data2mien/All2.xlsx")
#View(All2)


data<-All2

dim(data)
head(data)

data$dow<-weekdays(data$date)
data$time <- seq(nrow(data))

data2<-data

# REGIONS
regions <- as.character(unique(data$regnames))

# CREATE A LIST WITH THE REGIONAL SERIES
data <- lapply(regions,function(x) data[data$regnames==x,])
names(data) <- regions
m <- length(regions)

# TEMPERATURE RANGES
ranges <- t(sapply(data, function(x) range(x$tmean,na.rm=T)))
ranges
####################################################################

# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

# TEST LAG DAY
####################################################################
# DEFINE THE CROSS-BASIS MATRICES
# NB: THE USER CAN MODIFY THE CHOICES BELOW TO RUN ALTERNATIVE MODELS

# MAIN MODEL
# - PREDICTOR SPACE: QUADRATIC SPLINE WITH SPECIFIC KNOT SELECTION
# - LAG SPACE: NATURAL CUBIC SPLINE WITH DF AT EQUALLY-SPACED LOG-VALUES
lag <- c(0,21)
bound <- colMeans(ranges)
varknots <- equalknots(bound,fun="bs",degree=2,df=3)
lagknots <- logknots(21,df=5,int=T)
argvar <- list(fun="bs",degree=2,knots=varknots,bound=bound)
arglag <- list(fun="ns",knots=lagknots)

####

cb <- crossbasis(data2$tmean,lag=21,argvar=argvar,arglag=arglag, group=NULL)

summary(cb)

# RUN THE MODEL
model <- glm(d_61 ~ cb + dow + ns(time,df=39*4)+ ns(rh,df=3),
             family=quasipoisson(),data2)


range<-quantile(data2$tmean,c(0.05,0.95),na.rm=TRUE)
predglm1 <- crosspred(cb,model,at=seq(range[1],range[2],0.1))
ot<-which.min(predglm1$allRRfit)
ot

range<-quantile(data2$tmean,c(0.025,0.975),na.rm=TRUE)
range

2.5%    97.5% 
  12.00000 31.59687

# ALTERNATIVE MODELS
# - IDENTICAL BASIS FOR PREDICTOR SPACE
# LAG SPACE: CONSTANT FOR LAG 0-3 AND LAG 0-21
lag2 <- c(0,3)

lagknots2 <- logknots(3,df=3,int=T)
arglag2 <- list(fun="ns",knots=lagknots2)

lag3 <- c(0,7)

lagknots3 <- logknots(7,df=5,int=T)
arglag3 <- list(fun="ns",knots=lagknots3)

lag4 <- c(0,14)

lagknots4 <- logknots(14,df=5,int=T)
arglag4 <- list(fun="ns",knots=lagknots4)

#arglag2 <- arglag3 <- list(fun="strata",df=1)

####################################################################
# BUILT OBJECTS WHERE RESULTS WILL BE STORED
#   y- IS THE MATRIX FOR THE OUTCOME PARAMETERS
#   S- IS THE LISTS OF (CO)VARIANCE MATRICES

# OVERALL CUMULATIVE SUMMARIES
yall <- matrix(NA,length(data),3,dimnames=list(regions,paste("b",seq(3),sep="")))
yall2 <- yall3 <- yall4<- yall

# PREDICTOR-SPECIFIC SUMMARIES FOR MAIN MODEL
yhot <- matrix(NA,length(data),5,dimnames=list(regions,paste("b",seq(5),sep="")))
ycold <- matrix(NA,length(data),5,dimnames=list(regions,paste("b",seq(5),sep="")))

# (CO)VARIANCE MATRICES
Sall <- vector("list",length(data))
names(Sall) <- regions
Shot <- Scold <- Sall2 <- Sall3 <- Sall4 <- Sall

# Q-AIC
qaic <- qaic2 <- qaic3<- qaic4 <- 0

####################################################################
# RUN THE MODEL FOR EACH CITY
#############################
# 1. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-1
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=11*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=11*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=11*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=11*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3) ; sum(qaic4)

# RESET WARNING
options(warn=0)

#
#############################
# 2. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-2
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3); sum(qaic4)

# RESET WARNING
options(warn=0)

#
#############################
# 3. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-3
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3); sum(qaic4)

# RESET WARNING
options(warn=0)

#
#############################
# 4. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-4
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=8*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=8*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=8*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=8*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3) ; sum(qaic4)

# RESET WARNING
options(warn=0)

#
#############################
# 5. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-5
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=4*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3); sum(qaic4)

# RESET WARNING
options(warn=0)

#
#############################
# 6. 
# WARNING FOR PREDICTION BEYOND BOUNDARIES SUPPRESSED
options(warn=-1)

# LOOP FOR CITIES
#for(i in seq(data)) {
i<-6
# PRINT
cat(i,"")

# LOAD
sub <- data[[i]]

# DEFINE THE CROSS-BASES
cb <- crossbasis(sub$tmean,lag=lag,argvar=argvar,arglag=arglag)
cb2 <- crossbasis(sub$tmean,lag=lag2,argvar=argvar,arglag=arglag2)
cb3 <- crossbasis(sub$tmean,lag=lag3,argvar=argvar,arglag=arglag3)
cb4 <- crossbasis(sub$tmean,lag=lag4,argvar=argvar,arglag=arglag4)

# SET THE FIRST 21 RECORDS FOR cb2 AS MISSING
# THIS MAKES THE 3 MODELS COMPARABLE THROUGH AIC (SAME OBS)
cb2[0:21,] <- NA

# RUN THE FIRST-STAGE MODELS
mfirst <- glm(d_61 ~ cb+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst2 <- glm(d_61 ~ cb2+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst3 <- glm(d_61 ~ cb3+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)
mfirst4 <- glm(d_61 ~ cb4+dow+ns(time,df=6*4)+ ns(rh,df=3),family=quasipoisson(),sub)

####################################################################
# REDUCTION TO SUMMARY ASSOCIATIONS

# TO OVERALL CUMULATIVE SUMMARY
# NB: CENTERING NOT REALLY NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE 
#   VAR SPACE DO NOT DEPEND ON CENTERING VALUE
crall <- crossreduce(cb,mfirst,cen=25.7)
crall2 <- crossreduce(cb2,mfirst2,cen=25.7)
crall3 <- crossreduce(cb3,mfirst3,cen=25.7)
crall4 <- crossreduce(cb4,mfirst4,cen=25.7)

# TO PREDICTOR-SPECIFIC SUMMARY FOR 22C AND 0C
# NB: CENTERING NEEDED HERE, AS COEF-VCOV (EXTRACTED BELOW) IN THE LAG SPACE
#   DO DEPEND ON CENTERING VALUE
crhot <- crossreduce(cb,mfirst,type="var",value=26.7,cen=25.7)
crcold <- crossreduce(cb,mfirst,type="var",value=0,cen=25.7)

####################################################################
# STORE THE RESULTS

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
yall[i,] <- coef(crall)
Sall[[i]] <- vcov(crall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
yall2[i,] <- coef(crall2)
yall3[i,] <- coef(crall3)
yall4[i,] <- coef(crall4)
Sall2[[i]] <- vcov(crall2)
Sall3[[i]] <- vcov(crall3)
Sall4[[i]] <- vcov(crall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
yhot[i,] <- coef(crhot)
Shot[[i]] <- vcov(crhot)
# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
ycold[i,] <- coef(crcold)
Scold[[i]] <- vcov(crcold)

# Q-AIC
qaic[i] <- fqaic(mfirst)
qaic2[i] <- fqaic(mfirst2)
qaic3[i] <- fqaic(mfirst3)
qaic4[i] <- fqaic(mfirst4)



####################################################################

# TEST: REDUCTION OF ALTERNATIVE MODELS TO THE SPACE OF THE PREDICTOR RETURNS
# THE SAME PARAMETERS APART FROM SCALING (SUMMED UPON 22 LAGS)
coef(crosspred(cb3,mfirst3,cen=25.7))
coef(crossreduce(cb3,mfirst3,cen=25.7))/29

# GRAND Q-AIC
sum(qaic) ; sum(qaic2) ; sum(qaic3); sum(qaic4)

# RESET WARNING
options(warn=0)


###############################################################################
# Updated version of the code for the analysis in:
#
#   "Reducing and meta-analyzing estimates from distributed lag non-linear models"
#   Gasparrini and Armstrong 
#   BMC Medical Research Methodology - 2013
#   http://www.ag-myresearch.com/2013_gasparrini_bmcmrm.html
#
# Update: 05 December 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   http://www.ag-myresearch.com/2013_gasparrini_bmcmrm.html
###############################################################################

####################################################################
# SECOND STAGE
# - RUN THE MULTIVARIATE META-ANALYTICAL MODELS WITH mvmeta
# - CREATE BASIS VARIABLES USING onebasis, TO BE USED FOR PREDICTION
# - OBTAIN PREDICTIONS THROUGH crosspred (dlnm)
####################################################################

####################################################################
# PERFORM MULTIVARIATE META-ANALYSIS

# LOAD THE PACKAGES (mvmeta PACKAGE IS ASSUMED TO BE INSTALLED)
library(mvmeta)

# SELECT THE ESTIMATION METHOD
method <- "reml"
# IN THE CURRENT VERSION, SET control=list(showiter=T) TO 
#   INSPECT THE OPTIMIZATION SEARCH

# OVERALL CUMULATIVE SUMMARY FOR THE MAIN MODEL
mvall <- mvmeta(yall~1,Sall,method=method)
summary(mvall)

# OVERALL CUMULATIVE SUMMARY FOR THE ALTERNATIVE MODELS
mvall2 <- mvmeta(yall2~1,Sall2,method=method)
summary(mvall2)
mvall3 <- mvmeta(yall3~1,Sall3,method=method)
summary(mvall3)
mvall4 <- mvmeta(yall4~1,Sall4,method=method)
summary(mvall4)

# PREDICTOR-SPECIFIC SUMMARY FOR 22C (MAIN MODEL)
mvhot <- mvmeta(yhot~1,Shot,method=method)
summary(mvhot)
# NOTE THE PROBLEM FOR ESTIMATED (CO)VARIANCE MATRIX

# PREDICTOR-SPECIFIC SUMMARY FOR 0C (MAIN MODEL)
mvcold <- mvmeta(ycold~1,Scold,method=method)
summary(mvcold)

####################################################################
# CREATE BASES FOR PREDICTION

# BASES OF TEMPERATURE AND LAG USED TO PREDICT, EQUAL TO THAT USED FOR ESTIMATION
# COMPUTED USING THE ATTRIBUTES OF THE CROSS-BASIS USED IN ESTIMATION
xvar <- seq(bound[1],bound[2],by=0.1)
bvar <- do.call("onebasis",c(list(x=xvar),attr(cb,"argvar")))
xlag <- 0:210/10
blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))

####################################################################
# REGION-SPECIFIC FIRST-STAGE SUMMARIES

regall <- lapply(seq(nrow(yall)),function(i) crosspred(bvar,coef=yall[i,],
                                                       vcov=Sall[[i]],model.link="log",cen=25.7))
reghot <- lapply(seq(nrow(yhot)),function(i) crosspred(blag,coef=yhot[i,],
                                                       vcov=Shot[[i]],model.link="log",cen=25.7))
regcold <- lapply(seq(nrow(ycold)),function(i) crosspred(blag,coef=ycold[i,],
                                                         vcov=Scold[[i]],model.link="log",cen=25.7))

####################################################################
# PREDICTION FOR A GRID OF TEMPERATURE AND LAG VALUES

# OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR MAIN MODEL
cpall <- crosspred(bvar,coef=coef(mvall),vcov=vcov(mvall),
                   model.link="log",by=0.1,from=bound[1],to=bound[2],cen=25.7)

# OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR ALTERNATIVE MODELS
cpall2 <- crosspred(bvar,coef=coef(mvall2),vcov=vcov(mvall2),
                    model.link="log",by=0.1,from=bound[1],to=bound[2],cen=25.7)
cpall3 <- crosspred(bvar,coef=coef(mvall3),vcov=vcov(mvall3),
                    model.link="log",by=0.1,from=bound[1],to=bound[2],cen=25.7)
cpall4 <- crosspred(bvar,coef=coef(mvall4),vcov=vcov(mvall4),
                    model.link="log",by=0.1,from=bound[1],to=bound[2],cen=25.7)

# PREDICTOR-SPECIFIC SUMMARIES FOR 22C (MAIN MODEL)
cphot <- crosspred(blag,coef=coef(mvhot),vcov=vcov(mvhot),
                   model.link="log",at=0:210/10,cen=25.7)

# PREDICTOR-SPECIFIC SUMMARIES FOR 22C (MAIN MODEL)
cpcold <- crosspred(blag,coef=coef(mvcold),vcov=vcov(mvcold),
                    model.link="log",at=0:210/10,cen=25.7)

#

###############################################################################
# Updated version of the code for the analysis in:
#
#   "Reducing and meta-analyzing estimates from distributed lag non-linear models"
#   Gasparrini and Armstrong 
#   BMC Medical Research Methodology - 2013
#   http://www.ag-myresearch.com/2013_gasparrini_bmcmrm.html
#
# Update: 05 December 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   http://www.ag-myresearch.com/2013_gasparrini_bmcmrm.html
###############################################################################

####################################################################
# OVERALL CUMULATIVE SUMMARY ASSOCIATION

# PLOT
#pdf("figure2.pdf",height=5,width=13)

par(mar=c(5,4,1,1)+0.1,cex.axis=0.9,mgp=c(2.5,1,0))
layout(matrix(1:2,ncol=2))

plot(cpall,type="n",ylab="RR",ylim=c(.0,2),xlab="Temperature (C)")
for(i in seq(regall)) lines(regall[[i]],ptype="overall",col=grey(0.5),lty=2)
abline(h=1)
lines(cpall,col=2,lwd=2)
mtext("Northern: first-stage and pooled estimates",cex=1)
legend ("top",c("Pooled (with 95%CI)","First-stage region-specific"),
        lty=c(1,2),lwd=1.5,col=c(2,grey(0.7)),bty="n",inset=0.1,cex=0.8)

plot(cpall,ylab="RR",col=2,lwd=2,ylim=c(.8,2),xlab="Temperature (C)")
lines(cpall2,col=3,lty=2,lwd=2)
lines(cpall3,col=4,lty=4,lwd=2)
lines(cpall4,col=5,lty=4,lwd=2)
mtext("Comparison of alternative models",cex=1)
legend ("top",c("B-spline of lag 0-21 (with 95%CI)","Constant of lag 0-3",
                "Constant of lag 0-21", "Constant of lag 0-21"),lty=c(1,2,4,4),lwd=1.5,col=2:5,bty="n",inset=0.05,
        cex=0.8,title="Function for the lag space:")

dev.off()

# POINT OF MINIMUM MORTALITY
cpall$predvar[which.min(cpall$allRRfit)]
round(sum(regEngWales$tmean<17.1)/nrow(regEngWales)*100,1)

# Q TEST AND I-SQUARE
(qall <- qtest(mvall))
round(((qall$Q-qall$df)/qall$Q)[1]*100,1)
(qall2 <- qtest(mvall2))
round(((qall2$Q-qall2$df)/qall2$Q)[1]*100,1)
(qall3 <- qtest(mvall3))
round(((qall3$Q-qall3$df)/qall3$Q)[1]*100,1)

####################################################################
# PREDICTOR-SPECIFIC SUMMARIES

# PLOT
pdf("figure3.pdf",height=5,width=13)

par(mar=c(5,4,1,1)+0.1,cex.axis=0.9,mgp=c(2.5,1,0))
layout(matrix(1:2,ncol=2))

plot(cphot,type="n",ylab="RR",ylim=c(.95,1.12),xlab="Lag")
for(i in seq(reghot)) lines(reghot[[i]],ptype="overall",col=grey(0.5),lty=2)
abline(h=1)
lines(cphot,col=2,lwd=2)
legend ("top",c("Pooled (with 95%CI)","First-stage region-specific"),
        lty=c(1,2),lwd=1.5,col=c(2,grey(0.7)),bty="n",inset=0.1,cex=0.8)
mtext(text=paste("Predictor-specific summary for temperature = ",22,
                 "C",sep=""),cex=1)

plot(cpcold,type="n",ylab="RR",ylim=c(.95,1.12),xlab="Lag")
for(i in seq(regcold)) lines(regcold[[i]],ptype="overall",col=grey(0.5),lty=2)
abline(h=1)
lines(cpcold,col=2,lwd=2)
legend ("top",c("Pooled (with 95%CI)","First-stage region-specific"),
        lty=c(1,2),lwd=1.5,col=c(2,grey(0.7)),bty="n",inset=0.1,cex=0.8)
mtext(text=paste("Predictor-specific summary for temperature = ",0,
                 "C",sep=""),cex=1)

dev.off()

# OVERALL EFFECTS AT THESE TWO PREDICTOR LEVELS
round(with(cpall,cbind(allRRfit,allRRlow,allRRhigh)["26.7",]),3)
round(with(cpall,cbind(allRRfit,allRRlow,allRRhigh)["32.4",]),3)
round(with(cpall,cbind(allRRfit,allRRlow,allRRhigh)["13",]),3)

# TESTS AND STATISTICS
(qhot <- qtest(mvhot))
(qcold <- qtest(mvcold))
round(((qhot$Q-qhot$df)/qhot$Q)[1]*100,1)
round(((qcold$Q-qcold$df)/qcold$Q)[1]*100,1)

####################################################################
# COMPARISON OF RANDOM VS. FIXED EFFECT MODEL FOR SUMMARY AT 22C

mvhot2 <- update(mvhot,method="fixed")
coef(mvhot); coef(mvhot2)

cphot2 <- crosspred(blag,coef=coef(mvhot2),vcov=vcov(mvhot2),
                    model.link="log",at=0:210/10)

plot(cphot,ci="lines",ylab="RR",ylim=c(.95,1.12),xlab="Lag",col=2)
lines(cphot2,ci="lines",col=4)
mtext(text=paste("Predictor-specific summary for temperature = ",22,
                 "C",sep=""),cex=1)
legend ("top",c("Random-effects model","Fixed-effects model"),
        lty=1,col=c(2,4),bty="n",inset=0.1,cex=0.8)


#


