library(colorRamps)
library(plyr)
require(spatstat)
require(xts)
library(stringi)
library(survival)
library(coxme)
library(viridis)

dat.raw <- read.csv("ip_plus_uniq2.csv")
# "ip_plus_IP-POSIX.uniq.csv" effectively merges the constituent discontinuous land parcels of each farm

# get rid of unnecessary whitespaces from strings; reduce random-length empty date string to ""
dat.raw$ReportDate <- stri_trim(dat.raw$ReportDate)
dat.raw$DateSlaughtered <- stri_trim(dat.raw$DateSlaughtered)
dat.raw$Date.Disposal.Finished <- stri_trim(dat.raw$Date.Disposal.Finished)
dat.raw$DisposalMethod <- stri_trim(dat.raw$DisposalMethod)

# turn "" to NA
dat.raw[ dat.raw == "" ] = NA

# remove NA
dat.cc <- dat.raw[complete.cases(dat.raw$ReportDate, dat.raw$DateSlaughtered,
                                 dat.raw$Date.Disposal.Finished),] 

# standarize date format
dat.cc$ReportDate <- as.POSIXct(strptime(dat.cc$ReportDate, "%m/%d/%y %H:%M", tz="GMT"))
dat.cc$DateSlaughtered <- as.POSIXct(strptime(dat.cc$DateSlaughtered, "%m/%d/%y %H:%M", tz="GMT"))
dat.cc$Date.Disposal.Finished <- as.POSIXct(strptime(dat.cc$Date.Disposal.Finished, "%m/%d/%y %H:%M", tz="GMT"))

dat.test <- dat.cc#[1:2004,]

# clean up dates (decimal days)
delay.1.err <- difftime(dat.cc$DateSlaughtered, dat.cc$ReportDate, units="days")
delay.2.err <- difftime(dat.test$Date.Disposal.Finished, dat.test$DateSlaughtered, units="days")
idx <- which(delay.1.err > 0 & delay.2.err > 0)
dat.der <- dat.test[idx,] #remove delay error from dataframe

# enumerate kept delays
delay.1 <- as.numeric(delay.1.err[idx])
delay.2 <- as.numeric(delay.2.err[idx])

# assigning only the dates, not times
report.date    <- as.Date(dat.der$ReportDate)
slaughter.date <- as.Date(dat.der$DateSlaughtered)
disposal.date  <- as.Date(dat.der$Date.Disposal.Finished)

date.min = min(report.date, slaughter.date, disposal.date)
date.max = max(report.date, slaughter.date, disposal.date)

# basic summary plots
plot(dat.der$DX,dat.der$DY,cex=delay.1/5,col=rgb(0,0,0,.25), main = "Delay to Culling", xlab="Easting", ylab="Northing")
plot(dat.der$DX,dat.der$DY,cex=delay.2/5,col=rgb(0,0,0,.25), main = "Delay to Disposal", xlab="Easting", ylab="Northing")

# set up basic dataframe
chdf <- data.frame(substr(dat.der$CPHNo, 1, 2)) #extract county numbers
names(chdf)[1] <- "County" # shorten column name
chdf$report <- report.date
chdf$slaughter <- slaughter.date
chdf$disposal <- disposal.date
chdf$delay.1 <- delay.1
chdf$delay.2 <- delay.2
chdf$censor <- 1

# NEW PLOT
graphics.off()
par("mar")
par(mar=c(4,4.5,2,1))
attach(mtcars)
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), heights=c(heights=c(1,2,2)))
#layout(matrix(c(1,1,2,2,3,3,4,5), 4, 2, byrow = TRUE), heights=c(heights=c(1,2,2,1)))
colors.m = magma(10, alpha=0.8)
colors.v = viridis(10, alpha=0.8)
colors.m.lt = magma(10, alpha=0.4)
colors.v.lt = viridis(10, alpha=0.4)

# ---- Fig 1. Reported cases time-series
td <- as.numeric(max(disposal.date)-min(report.date))
report.date.fill <- format(c(report.date, seq(min(report.date), max(disposal.date), 1)), format="%m-%d")
#plot(table(report.date.fill) - 1, col=colors.m[2], xlim=c(1,td), xlab='', ylab="Daily case reports", cex.lab=1.3)
plot(table(report.date.fill) - 1, col=colors.m[2], xlab='', ylab="Daily case reports", cex.lab=1.3)

#slaughter.date.fill <- format(c(slaughter.date, seq(min(report.date), max(disposal.date), 1)), format="%m-%d")
#plot(table(slaughter.date.fill) - 1, col=colors.m[2], xlim=c(1,td), xlab='', ylab="Daily cull count", cex.lab=1.3)

#disposal.date.fill <- format(c(disposal.date, seq(min(report.date), max(disposal.date), 1)), format="%m-%d")
#plot(table(disposal.date.fill) - 1, col=colors.m[2], xlim=c(1,td), xlab='', ylab="Daily disposal count", cex.lab=1.3)

# ---- Fig 2. Two Types of Delays over time
rank.num <- rank(report.date, ties.method = "first")
plot(report.date, rank.num, xaxt="n", xlim=c(date.min, date.max), type = "n", xlab='', ylab='Report date sequence', cex.lab=1.3)

segments(report.date, rank.num, slaughter.date, rank.num, col=colors.m[5], lwd=.3)
segments(slaughter.date, rank.num, disposal.date, rank.num, col=colors.v[7], lwd=.3)

axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d") # debugged from max(report.date)


# ---- Fig 3. Daily delay time-series
delay.1.daily <- aggregate(chdf$delay.1, list(chdf$report), mean)
delay.2.daily <- aggregate(chdf$delay.2, list(chdf$report), mean)
ylim.max <- max(delay.1.daily$x, delay.2.daily$x)

plot(delay.1.daily, col=colors.m.lt[5], xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', pch=16, cex.lab=1.3)
polygon(c(min(delay.1.daily$Group.1), delay.1.daily$Group.1, max(delay.1.daily$Group.1)), 
        c(0, delay.1.daily$x, 0), border=NA, col=colors.m.lt[5])

par(new=TRUE)

plot(delay.2.daily, col=colors.v.lt[7], xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='Report date', ylab='Mean daily response delay', pch=16, cex.lab=1.3)
polygon(c(min(delay.2.daily$Group.1), delay.2.daily$Group.1, max(delay.2.daily$Group.1)), 
        c(0, delay.2.daily$x, 0), border=NA, col=colors.v.lt[7])

axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")

# run separately
attach(mtcars)
layout(matrix(c(1,2), 1, 2, byrow = TRUE), heights=c(heights=c(0.5)))
# ---- Inset: Figs 4&5. Histograms of Delays (within 2 weeks)
hist(delay.1, main='Histogram of culling delay (days)', 
     col=colors.m[5], breaks=56, xlim=c(0,14), ylim=c(0,1000), xlab='', cex.lab=1.3, cex.main=1.3)
hist(delay.2, main='Histogram of disposal delay (days)', 
     col=colors.v[7], breaks=56, xlim=c(0,14), ylim=c(0,1000), xlab='', ylab='', cex.lab=1.3, cex.main=1.3)


# PRE-SURVIVAL ANALYSIS DATA CONFIGURATION
chdf$cattle  <- dat.der$Cattle/100
chdf$pigs    <- dat.der$Pigs/100
chdf$sheep   <- dat.der$Sheep/100
chdf$goats   <- dat.der$Goats/100
chdf$size <- rowSums(chdf[8:11]) # compute total livestock number
#chdf$dispmeth <- dat.der$DisposalMethod

# set up spatial computation (# of neighbors)
xy_only = data.frame("x"=dat.der$DX, "y"=dat.der$DY)
W <- ripras(xy_only) # estimate window from points alone
pp <- as.ppp(xy_only, W) #convert xy-coordinates to ppp with window
chdf$nnb5h <- applynbd(pp, R=500.,  function(Y, ...){npoints(Y)}, exclude=TRUE)/10
chdf$nnb1k <- applynbd(pp, R=1000., function(Y, ...){npoints(Y)}, exclude=TRUE)/10
chdf$nnb2k <- applynbd(pp, R=2000., function(Y, ...){npoints(Y)}, exclude=TRUE)/10
chdf$nnb3k <- applynbd(pp, R=3000., function(Y, ...){npoints(Y)}, exclude=TRUE)/10
chdf$nnb4k <- applynbd(pp, R=4000., function(Y, ...){npoints(Y)}, exclude=TRUE)/10
chdf$nnb5k <- applynbd(pp, R=5000., function(Y, ...){npoints(Y)}, exclude=TRUE)/10

# compute queue size
# e.g., for farm i, 
#       culling  queue size = all the infected but not yet (completely) culled farms
#       disposal queue size = all the culled but not yet (completely) disposed farms
# op.type 'operation type' is either 'c' or 'd'
queue <- function(op.type){
  chdf.n <- nrow(chdf)
  out <- rep(0, chdf.n)
  
  if (op.type=='c'){
    bound <- chdf$report
    for (i in 1:chdf.n){
      out[i] <- nrow(subset(chdf, report < bound[i] & slaughter >= bound[i]))
    }
  } else if (op.type=='d'){
    bound <- chdf$slaughter
    for (i in 1:chdf.n){
      out[i] <- nrow(subset(chdf, slaughter < bound[i] & disposal >= bound[i]))
    }
  }
  return(out)
}

chdf$dmd.c <-  queue('c')/10
chdf$dmd.d <-  queue('d')/10

# split dataframe for entries before April.1 and after April.1
chdf.A <- chdf[which(chdf$report<"2001-04-01"),]
chdf.B <- chdf[which(chdf$report>="2001-04-01"),]

# ---- DELAY-SPECIFIC DATAFRAMES 
cull.A <- chdf.A[,c(1, 5, 7, 12:19)]
cull.B <- chdf.B[,c(1, 5, 7, 12:19)]
cull.C <- chdf[,c(1, 5, 7, 12:19)]

disp.A <- chdf.A[,c(1, 6, 7, 12:18, 20)]
disp.B <- chdf.B[,c(1, 6, 7, 12:18, 20)]
disp.C <- chdf[,c(1, 6, 7, 12:18, 20)]


# SUPPLEMENTARY PLOTS
graphics.off()
bitmap("testplot.tiff", height=10, width=8, units='in', type="tifflzw", res=600)
par("mar")
par(mar=c(4,4.5,2,1))
attach(mtcars)
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(heights=c(2,2,2,2)))

# (a) daily size
size.daily <- aggregate(chdf$size, list(chdf$report), mean)
ylim.max <- max(size.daily$x * 1)

plot(size.daily$Group.1, size.daily$x, col='gray', xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', main='Mean daily infected farm size', pch=16, cex.main=1.3)
polygon(c(min(size.daily$Group.1), size.daily$Group.1, max(size.daily$Group.1)), 
        c(0, size.daily$x * 1, 0), border=NA, col='gray')
axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")

# (b) daily demand (on cull)
dmd.c.daily <- aggregate(chdf$dmd.c, list(chdf$report), mean)
ylim.max <- 16.76#167.6333 #same as (c) ##max(dmd.c.daily$x * 10)

plot(dmd.c.daily$Group.1, dmd.c.daily$x, col='gray', xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', main='Mean daily control (culling) demand', pch=16, cex.main=1.3)
polygon(c(min(dmd.c.daily$Group.1), dmd.c.daily$Group.1, max(dmd.c.daily$Group.1)), 
        c(0, dmd.c.daily$x * 1, 0), border=NA, col='gray')
axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")

# (c) daily demand (on disp)
dmd.d.daily <- aggregate(chdf$dmd.d, list(chdf$report), mean)
ylim.max <- max(dmd.d.daily$x * 1)

plot(dmd.d.daily$Group.1, dmd.d.daily$x, col='gray', xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', main='Mean daily control (disposal) demand', pch=16, cex.main=1.3)
polygon(c(min(dmd.d.daily$Group.1), dmd.d.daily$Group.1, max(dmd.d.daily$Group.1)), 
        c(0, dmd.d.daily$x * 1, 0), border=NA, col='gray')
axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")

# (d) daily nnb5k
nnb5k.daily <- aggregate(chdf$nnb5k, list(chdf$report), mean)
ylim.max <- max(nnb5k.daily$x * 1)

plot(nnb5k.daily$Group.1, nnb5k.daily$x, col='gray', xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='Report date', ylab='', main='Mean daily cluster around infected farms', pch=16, cex.lab=1.3, cex.main=1.3)
polygon(c(min(nnb5k.daily$Group.1), nnb5k.daily$Group.1, max(nnb5k.daily$Group.1)), 
        c(0, nnb5k.daily$x * 1, 0), border=NA, col='gray')
axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")
dev.off()
####################################################
####################################################
############### SURVIVAL ANALYSIS ##################
####################################################
####################################################

######### CULL ~ SIZE + DEMAND + 5k (SPATIAL FARM) CLUSTER
cull.A.m0 <- coxme(Surv(delay.1, censor) ~ (1|County), data=cull.A)
cull.A.m1 <- coxme(Surv(delay.1, censor) ~ size  + (1|County), data=cull.A)
cull.A.m2 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + (1|County), data=cull.A)
cull.A.m3 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + nnb5k + (1|County), data=cull.A)
anova(cull.A.m0, cull.A.m1, cull.A.m2, cull.A.m3)

cull.B.m0 <- coxme(Surv(delay.1, censor) ~ (1|County), data=cull.B)
cull.B.m1 <- coxme(Surv(delay.1, censor) ~ size + (1|County), data=cull.B)
cull.B.m2 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + (1|County), data=cull.B)
cull.B.m3 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + nnb5k + (1|County), data=cull.B)
anova(cull.B.m0, cull.B.m1, cull.B.m2, cull.B.m3)

cull.C.m0 <- coxme(Surv(delay.1, censor) ~ (1|County), data=cull.C)
cull.C.m1 <- coxme(Surv(delay.1, censor) ~ size + (1|County), data=cull.C)
cull.C.m2 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + (1|County), data=cull.C)
cull.C.m3 <- coxme(Surv(delay.1, censor) ~ size + dmd.c + nnb5k + (1|County), data=cull.C)
anova(cull.C.m0, cull.C.m1, cull.C.m2, cull.C.m3)


######### DISP ~ SIZE + DEMAND + 5k (SPATIAL FARM) CLUSTER
disp.A.m0 <- coxme(Surv(delay.2, censor) ~ (1|County), data=disp.A)
disp.A.m1 <- coxme(Surv(delay.2, censor) ~ size + (1|County), data=disp.A)
disp.A.m2 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + (1|County), data=disp.A)
disp.A.m3 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + nnb5k + (1|County), data=disp.A)
anova(disp.A.m0, disp.A.m1, disp.A.m2, disp.A.m3)

disp.B.m0 <- coxme(Surv(delay.2, censor) ~ (1|County), data=disp.B)
disp.B.m1 <- coxme(Surv(delay.2, censor) ~ size + (1|County), data=disp.B)
disp.B.m2 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + (1|County), data=disp.B)
disp.B.m3 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + nnb5k + (1|County), data=disp.B)
anova(disp.B.m0, disp.B.m1, disp.B.m2, disp.B.m3)

disp.C.m0 <- coxme(Surv(delay.2, censor) ~ (1|County), data=disp.C)
disp.C.m1 <- coxme(Surv(delay.2, censor) ~ size + (1|County), data=disp.C)
disp.C.m2 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + (1|County), data=disp.C)
disp.C.m3 <- coxme(Surv(delay.2, censor) ~ size + dmd.d + nnb5k + (1|County), data=disp.C)
anova(disp.C.m0, disp.C.m1, disp.C.m2, disp.C.m3)


####################################################
####################################################
################# STATS FITTING ####################
##################### BASIC ########################
####################################################
library(MASS)

# CULL: Gamma parameters (Method of moments)
d1.mean <- mean(cull.C$delay.1)
d1.var <- var(cull.C$delay.1)
d1.scale <- d1.var / d1.mean #theta
d1.shape <- d1.mean / d1.scale #alpha

hist(cull.C$delay.1, breaks=0:120, prob=TRUE)
curve(dgamma(x, shape=d1.shape, scale=d1.scale), col='red', add=TRUE)

# CULL: Gamma parameters (Maximum likelihood estimation)
d1.gamma.coef <- fitdistr(cull.C$delay.1, "gamma")

hist(cull.C$delay.1, breaks=0:120, prob=TRUE)
curve(dgamma(x, shape = d1.gamma.coef$estimate[1], rate = d1.gamma.coef$estimate[2]),
      from=1, to=120, n=120, col='green', add=TRUE)

# CULL: Exponential parameters (Maximum likelihood estimation)
d1.exp.coef <- fitdistr(cull.C$delay.1, "exponential")

hist(cull.C$delay.1, breaks=0:120, prob=TRUE)
curve(dexp(x, rate = d1.exp.coef$estimate[1]),
      from=1, to=120, n=120, col='blue', add=TRUE)

# DISP: Gamma parameters (Method of moments)
d2.mean <- mean(disp.C$delay.2)
d2.var <- var(disp.C$delay.2)
d2.scale <- d2.var / d2.mean #theta
d2.shape <- d2.mean / d2.scale #alpha

hist(disp.C$delay.2, breaks=0:50, prob=TRUE)
curve(dgamma(x, shape=d2.shape, scale=d2.scale), col='red', add=TRUE)

# DISP: Gamma parameters (Maximum likelihood estimation)
d2.gamma.coef <- fitdistr(disp.C$delay.2, "gamma")

hist(disp.C$delay.2, breaks=0:50, prob=TRUE)
curve(dgamma(x, shape = d2.gamma.coef$estimate[1], rate = d2.gamma.coef$estimate[2]),
      from=1, to=50, n=50, col='green', add=TRUE)

# DISP: Exponential parameters (Maximum likelihood estimation)
d2.exp.coef <- fitdistr(disp.C$delay.2, "exponential")

hist(disp.C$delay.2, breaks=0:50, prob=TRUE)
curve(dexp(x, rate = d2.exp.coef$estimate[1]),
      from=1, to=50, n=50, col='blue', add=TRUE)


####################################################
################ STATS & SIMULATION ################
####################################################
library("flexsurv")
# no support of clustered/random effects

####################################################
###### removed farm cluster from all analyses ######
####################################################
fit1.A.m1 <- flexsurvreg(Surv(delay.1, censor) ~ size + frailty(County),
                         data = cull.A, dist = "weibull")
fit1.A.m2 <- flexsurvreg(Surv(delay.1, censor) ~ size + dmd.c + frailty(County),
                         data = cull.A, dist = "weibull")
fit1.B.m1 <- flexsurvreg(Surv(delay.1, censor) ~ size + frailty(County),
                         data = cull.B, dist = "weibull")
fit1.B.m2 <- flexsurvreg(Surv(delay.1, censor) ~ size + dmd.c + frailty(County),
                         data = cull.B, dist = "weibull")
fit1.C.m1 <- flexsurvreg(Surv(delay.1, censor) ~ size + frailty(County),
                         data = cull.C, dist = "weibull")
fit1.C.m2 <- flexsurvreg(Surv(delay.1, censor) ~ size + dmd.c + frailty(County),
                         data = cull.C, dist = "weibull")

fit2.A.m1 <- flexsurvreg(Surv(delay.2, censor) ~ size + frailty(County),
                         data = disp.A, dist = "weibull")
fit2.A.m2 <- flexsurvreg(Surv(delay.2, censor) ~ size + dmd.d + frailty(County),
                         data = disp.A, dist = "weibull")
fit2.B.m1 <- flexsurvreg(Surv(delay.2, censor) ~ size + frailty(County),
                         data = disp.B, dist = "weibull")
fit2.B.m2 <- flexsurvreg(Surv(delay.2, censor) ~ size + dmd.d + frailty(County),
                         data = disp.B, dist = "weibull")
fit2.C.m1 <- flexsurvreg(Surv(delay.2, censor) ~ size + frailty(County),
                         data = disp.C, dist = "weibull")
fit2.C.m2 <- flexsurvreg(Surv(delay.2, censor) ~ size + dmd.d + frailty(County),
                         data = disp.C, dist = "weibull")


# baseline hazard: Weibull

# N = sample size    
# lambda = scale parameter in h0()
# rho = shape parameter in h0()
# beta = fixed effect parameter
# rateC = rate parameter of the exponential distribution of C

## need to sim. censor, else (ie, censor rate too low or all=1) convergence is poor

# ----------------  Temporary Model for Parametric Bootstrap --------------------
# samples covariates from the ranges of raw data to use as inputs; applied in Parametric Bootstrap to validate model
simulWeib.boot.m1 <- function(N, lambda, rho, 
                              beta1, raw1)
{
  # covariate
  x1 <- sample(x=seq(min(raw1), max(raw1), length.out=150), size=N, replace=TRUE)#FALSE)
  
  # Weibull latent event times
  v <- runif(n=N)
  # fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1))) ^ (1/rho)
  
  # censoring times
  C <- 42.
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x1=x1)
}


simulWeib.boot.m2 <- function(N, lambda, rho, 
                              beta1, beta2, raw1, raw2)
{
  # covariate
  x1 <- sample(x=seq(min(raw1), max(raw1), length.out=150), size=N, replace=TRUE)#FALSE)
  x2 <- sample(x=seq(min(raw2), max(raw2), length.out=150), size=N, replace=TRUE)#FALSE)
  
  # Weibull latent event times
  v <- runif(n=N)
  #fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1 + x2 * beta2))) ^ (1/rho)
  
  # censoring times
  C <- 42.
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x1=x1,
             x2=x2)
}


# ------------------------  General Model (x_i <- raw_i) --------------------------
simulWeib.m1 <- function(lambda, rho, 
                         beta1, raw1)
{
  N <- length(raw1)
  
  # covariate
  x1 <- raw1 #sample(x=raw1, size=N, replace=FALSE)
  
  # Weibull latent event times
  v <- runif(n=N)
  # fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1))) ^ (1/rho)
  
  # censoring times
  C <- 42.
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x1=x1)
}

simulWeib.m2 <- function(lambda, rho, 
                         beta1, beta2, raw1, raw2)
{
  N <- length(raw1)
  
  # covariate
  x1 <- raw1 #sample(x=raw1, size=N, replace=FALSE)
  x2 <- raw2 #sample(x=raw2, size=N, replace=FALSE)
  
  # Weibull latent event times
  v <- runif(n=N)
  #fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1 + x2 * beta2))) ^ (1/rho)
  
  # censoring times
  C <- 42
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x1=x1,
             x2=x2)
}

#################################################################
####  Validate close match between simulated and real delays ####
#################################################################
colors.i = inferno(10, alpha=0.05)

# Culling: time frame A (before 4/1)
hist(cull.A$delay.1, freq=FALSE, breaks=56, ylim=c(0,0.7))
for (i in 1:200){
  d1.A.m1 <- simulWeib.m1(lambda=fit1.A.m1$res[2], rho=fit1.A.m1$res[1], 
                          beta1=cull.A.m1$coef[1],
                          raw1=cull.A$size)
  lines(density(d1.A.m1$time), col=colors.i[8])
}
for (i in 1:200){
  d1.A.m2 <- simulWeib.m2(lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                          beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                          raw1=cull.A$size, raw2=cull.A$dmd.c)
  lines(density(d1.A.m2$time), col=colors.i[2])
}

# Culling: time frame B (on or after 4/1)
hist(cull.B$delay.1, freq=FALSE, breaks=56, ylim=c(0,0.7))
for (i in 1:200){
  d1.B.m1 <- simulWeib.m1(lambda=fit1.B.m1$res[2], rho=fit1.B.m1$res[1], 
                          beta1=cull.B.m1$coef[1],
                          raw1=cull.B$size)
  lines(density(d1.B.m1$time), col=colors.i[8])
}
for (i in 1:200){
  d1.B.m2 <- simulWeib.m2(lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                          beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                          raw1=cull.B$size, raw2=cull.B$dmd.c)
  lines(density(d1.B.m2$time), col=colors.i[2])
}



# Disposal: time frame A (before 4/1)
hist(disp.A$delay.2, freq=FALSE, breaks=56, ylim=c(0,0.5))
for (i in 1:200){
  d2.A.m1 <- simulWeib.m1(lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1], 
                          beta1=disp.A.m1$coef[1],
                          raw1=disp.A$size)
  lines(density(d2.A.m1$time), col=colors.i[8])
}
for (i in 1:200){
  d2.A.m2 <- simulWeib.m2(lambda=fit2.A.m2$res[2], rho=fit2.A.m2$res[1], 
                          beta1=disp.A.m2$coef[1], beta2=disp.A.m2$coef[2],
                          raw1=disp.A$size, raw2=disp.A$dmd.d)
  lines(density(d2.A.m2$time), col=colors.i[2])
}


# Disposal: time frame B (on or after 4/1)
hist(disp.B$delay.2, freq=FALSE, breaks=56, ylim=c(0,0.5))
for (i in 1:200){
  d2.B.m1 <- simulWeib.m1(lambda=fit2.B.m1$res[2], rho=fit2.B.m1$res[1], 
                          beta1=disp.B.m1$coef[1],
                          raw1=disp.B$size)
  lines(density(d2.B.m1$time), col=colors.i[8])
}
for (i in 1:200){
  d2.B.m2 <- simulWeib.m2(lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                          beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                          raw1=disp.B$size, raw2=disp.B$dmd.d)
  lines(density(d2.B.m2$time), col=colors.i[2])
}


#########################################################
################ Parametric Bootstrap ###################
#########################################################
# ----------------- functional form ---------------------
para.boot.m1 <- function(iter, N, lambda, rho,
                         beta1, raw1)
{
  betaHat1 <- rep(NA, iter)
  
  for(k in 1:iter)
  {
    dat <- simulWeib.boot.m1(N, lambda, rho, 
                             beta1,
                             raw1)
    
    fit <- coxph(Surv(time, status) ~ x1, data=dat)
    betaHat1[k] <- fit$coef[1]
  }
  return(c(mean(exp(betaHat1)), sd(exp(betaHat1))))
}


para.boot.m2 <- function(iter, N, lambda, rho,
                         beta1, beta2, raw1, raw2)
{
  betaHat1 <- rep(NA, iter)
  betaHat2 <- rep(NA, iter)
  
  for(k in 1:iter)
  {
    dat <- simulWeib.boot.m2(N, lambda, rho, 
                             beta1, beta2,
                             raw1, raw2)
    
    fit <- coxph(Surv(time, status) ~ x1 + x2, data=dat)
    betaHat1[k] <- fit$coef[1]
    betaHat2[k] <- fit$coef[2]

  }
  return(c(mean(exp(betaHat1)), sd(exp(betaHat1)), mean(exp(betaHat2)), sd(exp(betaHat2))))
}


# ---------------- Run ParaBoot Tests ---------------------
para.boot.m1(iter=2e3, N=1000,
             lambda=fit1.A.m1$res[2], rho=fit1.A.m1$res[1],
             beta1=cull.A.m1$coef[1],
             raw1=cull.A$size)
exp(cull.A.m1$coefficients)

para.boot.m2(iter=2e3, N=1000,
             lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1],
             beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
             raw1=cull.A$size, raw2=cull.A$dmd.c)
exp(cull.A.m2$coefficients)



para.boot.m1(iter=2e3, N=1000,
             lambda=fit1.B.m1$res[2], rho=fit1.B.m1$res[1],
             beta1=cull.B.m1$coef[1],
             raw1=cull.B$size)
exp(cull.B.m1$coefficients)

para.boot.m2(iter=2e3, N=1000,
             lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1],
             beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
             raw1=cull.B$size, raw2=cull.B$dmd.c)
exp(cull.B.m2$coefficients)



para.boot.m2(iter=2e3, N=1000,
             lambda=fit1.C.m2$res[2], rho=fit1.C.m2$res[1],
             beta1=cull.C.m2$coef[1], beta2=cull.C.m2$coef[2],
             raw1=cull.C$size, raw2=cull.C$dmd.c)
exp(cull.C.m2$coefficients)
    
# --------------------------------------------
para.boot.m1(iter=2e3, N=1000,
             lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1],
             beta1=disp.A.m1$coef[1],
             raw1=disp.A$size)
exp(disp.A.m1$coefficients)

para.boot.m2(iter=2e3, N=1000,
             lambda=fit2.A.m2$res[2], rho=fit2.A.m2$res[1],
             beta1=disp.A.m2$coef[1], beta2=disp.A.m2$coef[2],
             raw1=disp.A$size, raw2=disp.A$dmd.d)
exp(disp.A.m2$coefficients)



para.boot.m1(iter=2e3, N=1000,
             lambda=fit2.B.m1$res[2], rho=fit2.B.m1$res[1],
             beta1=disp.B.m1$coef[1],
             raw1=disp.B$size)
exp(disp.B.m1$coefficients)

para.boot.m2(iter=2e3, N=1000,
             lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1],
             beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
             raw1=disp.B$size, raw2=disp.B$dmd.d)
exp(disp.B.m2$coefficients)



para.boot.m2(iter=2e3, N=1000,
             lambda=fit2.C.m2$res[2], rho=fit2.C.m2$res[1],
             beta1=disp.C.m2$coef[1], beta2=disp.C.m2$coef[2],
             raw1=disp.C$size, raw2=disp.C$dmd.d)
exp(disp.C.m2$coefficients)

#########################################################
################ SIMS SCRIPT FOR MIKE ###################
#########################################################

# ---------------------  Lookup Table Model (x_i <- seq_i) -----------------------
simulWeib.look.m1 <- function(iter, lambda, rho, 
                              beta1, seq1, prefix, timeframe)
{
  # covariate
  x1 <- seq1  #size <- seq(0., 150., 1)
  
  names(x1) <- paste0("size", x1)
  N <- length(x1)
  
  for (i in 1:iter){
    # Weibull latent event times
    v <- runif(n=N)
    # fixed
    lambdaStar <- (1/lambda) ^ rho
    Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1))) ^ (1/rho)
    
    # censoring times
    C <- 42.
    
    # follow-up times and event indicators
    time <- pmin(Tlat, C)
    
    #plot(rowMeans(time), xlab='size', ylab='response time')
    #plot(colMeans(time), xlab='demand', ylab='response time')
    
    # data set
    df <- as.data.frame(time)
    
    # output the rownames in the output data
    colnames(df) <- NULL
    
    # Write as space-delimited file:
    write.table(df, file = paste0(prefix, "-size-", timeframe, "_", i), sep = "    ", quote = FALSE, row.names = FALSE) # removed extension
  }
}



simulWeib.look.m2 <- function(iter, lambda, rho, 
                              beta1, beta2, seq1, seq2, prefix, timeframe)
{
  # covariate
  x1 <- seq1  #size <- seq(0., 150., 1)
  x2 <- seq2  #queue for both cull and disp <- seq(0, 10, 0.1)
  
  names(x1) <- paste0("size", x1)
  names(x2) <- paste0("demand", x2)
  N <- length(x1) * length(x2)

  for (i in 1:iter){
    # Weibull latent event times
    v <- matrix(runif(n=N), nrow=dim(outer(x1, x2))[1]) # v <- runif(n=N) in matrix form
    # fixed
    lambdaStar <- (1/lambda) ^ rho
    Tlat <- (- log(v) / (lambdaStar * exp(outer(x1 * beta1, x2 * beta2, '+')))) ^ (1 / rho)
    
    # censoring times
    C <- 42.
    
    # follow-up times and event indicators
    time <- pmin(Tlat, C)
    
    #plot(rowMeans(time), xlab='size', ylab='response time')
    #plot(colMeans(time), xlab='demand', ylab='response time')
    
    # data set
    df <- as.data.frame(time)
    
    # output the rownames in the output data
    colnames(df) <- NULL
    
    # Write as space-delimited file:
    write.table(df, file = paste0(prefix, "-size+dmd-", timeframe, "_", i), sep = "    ", quote = FALSE, row.names = FALSE) # removed extension
  }
}

#########################################################
################ SIMS SCRIPT FOR MIKE ###################  
###################### PLOT TEST ########################
#########################################################
simulWeib.lookpt.m1 <- function(lambda, rho, 
                                beta1, seq1,
                                prefix)
{
  # covariate
  x1 <- seq1  #size <- seq(0., 150., 1)
  
  names(x1) <- paste0("size", x1)
  N <- length(x1)
  
  # Weibull latent event times
  v <- runif(n=N)
  # fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1))) ^ (1/rho)
  
  # censoring times
  C <- 42.
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  
  if (prefix=="cull") {
    cc <- 'red'
  } else if (prefix=="disposal") {
    cc <- 'black'
  }
  
  plot(x1*100, time, col=cc, ylim=c(0, 40.), xlab='size', ylab='response time')
}



simulWeib.lookpt.m1(lambda=fit1.A.m1$res[2], rho=fit1.A.m1$res[1], 
                    beta1=cull.A.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='cull')
par(new=TRUE)
simulWeib.lookpt.m1(lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1], 
                    beta1=disp.A.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='disposal')


simulWeib.lookpt.m1(lambda=fit1.B.m1$res[2], rho=fit1.B.m1$res[1], 
                    beta1=cull.B.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='cull')
par(new=TRUE)
simulWeib.lookpt.m1(lambda=fit2.B.m1$res[2], rho=fit2.B.m1$res[1], 
                    beta1=disp.B.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='disposal')


simulWeib.lookpt.m1(lambda=fit1.C.m1$res[2], rho=fit1.C.m1$res[1], 
                    beta1=cull.C.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='cull')
par(new=TRUE)
simulWeib.lookpt.m1(lambda=fit2.C.m1$res[2], rho=fit2.C.m1$res[1], 
                    beta1=disp.C.m1$coef[1],
                    seq1=seq(1., 150., 1), prefix='disposal')


# ------------------------------------
simulWeib.lookpt.m2 <- function(lambda, rho, 
                                beta1, beta2, seq1, seq2,
                                prefix, cov)
{
  # covariate
  x1 <- seq1  #size <- seq(0., 150., 1)
  x2 <- seq2  #queue for both cull and disp <- seq(0, 10, 0.1)
  
  names(x1) <- paste0("size", x1)
  names(x2) <- paste0("demand", x2)
  N <- length(x1) * length(x2)
  
  
  # Weibull latent event times
  v <- matrix(runif(n=N), nrow=dim(outer(x1, x2))[1]) # v <- runif(n=N) in matrix form
  # fixed
  lambdaStar <- (1/lambda) ^ rho
  Tlat <- (- log(v) / (lambdaStar * exp(outer(x1 * beta1, x2 * beta2, '+')))) ^ (1 / rho)
  
  # censoring times
  C <- 42.
    
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  
  if (prefix=="cull") {
    cc <- 'red'
  } else if (prefix=="disposal") {
    cc <- 'black'
  }

  if (cov=="size"){
    plot(x1*100, rowMeans(time), col=cc, ylim=c(0, 40.), xlab='size', ylab='response time')
  } else if (cov=="demand") {
    plot(x2*10, colMeans(time), col=cc, ylim=c(0, 40.), xlab='demand', ylab='response time')

  }
}


simulWeib.lookpt.m2(lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                    beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='size')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.A.m2$res[2], rho=fit2.A.m2$res[1], 
                    beta1=disp.A.m2$coef[1], beta2=disp.A.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='size')


simulWeib.lookpt.m2(lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                   beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                   seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='demand')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.A.m2$res[2], rho=fit2.A.m2$res[1], 
                   beta1=disp.A.m2$coef[1], beta2=disp.A.m2$coef[2],
                   seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='demand')




simulWeib.lookpt.m2(lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                    beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='size')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                    beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='size')


simulWeib.lookpt.m2(lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                    beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='demand')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                    beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='demand')




simulWeib.lookpt.m2(lambda=fit1.C.m2$res[2], rho=fit1.C.m2$res[1], 
                    beta1=cull.C.m2$coef[1], beta2=cull.C.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='size')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.C.m2$res[2], rho=fit2.C.m2$res[1], 
                    beta1=disp.C.m2$coef[1], beta2=disp.C.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='size')


simulWeib.lookpt.m2(lambda=fit1.C.m2$res[2], rho=fit1.C.m2$res[1], 
                    beta1=cull.C.m2$coef[1], beta2=cull.C.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', cov='demand')
par(new=TRUE)
simulWeib.lookpt.m2(lambda=fit2.C.m2$res[2], rho=fit2.C.m2$res[1], 
                    beta1=disp.C.m2$coef[1], beta2=disp.C.m2$coef[2],
                    seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disposal', cov='demand')


#########################################################
################ SIMS SCRIPT FOR MIKE ###################
##################### GENERATE ##########################
#########################################################


##########  >> Context A (farm size & demand, full timeline) << ############
#       > 1. culling delay | (farm size, demand, all report_dates)
simulWeib.look.m2(iter=500, lambda=fit1.C.m2$res[2], rho=fit1.C.m2$res[1], 
                  beta1=cull.C.m2$coef[1], beta2=cull.C.m2$coef[2],
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', timeframe='t3')

#       > 2. disposal delay | (farm size, demand, all report_dates)
simulWeib.look.m2(iter=500, lambda=fit2.C.m2$res[2], rho=fit2.C.m2$res[1], 
                  beta1=disp.C.m2$coef[1], beta2=disp.C.m2$coef[2],
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disp', timeframe='t3')


########## >> Context B (farm size & demand, split timeline) << ############
#       > 3. culling delay | (farm size, demand, report_date < 1 April)
simulWeib.look.m2(iter=500, lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                  beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', timeframe='t1')

#       > 4. culling delay | (farm size, demand, report_date >= 1 April)
simulWeib.look.m2(iter=500, lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                  beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='cull', timeframe='t2')

#       > 5. disposal delay | (farm size, demandNULL, report_date < 1 April) **note the use of m1
simulWeib.look.m2(iter=500, lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1], 
                  beta1=disp.A.m1$coef[1], beta2=0.0,
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disp', timeframe='t1')

#       > 6. disposal delay | (farm size, demand, report date >= 1 April)
simulWeib.look.m2(iter=500, lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                  beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                  seq1=seq(1., 150., 1), seq2=seq(0.0, 20.0, 0.1), prefix='disp', timeframe='t2')






#########################################################
############## SIMS SCRIPT TO REPRODUCE #################  
############# EMPIRICAL DELAY TIME-SERIES ###############
#########################################################
ts.repro.m1 <- function(iter, lambda, rho,
                        beta1, raw1)
{
  res <- data.frame(matrix(NA, nrow=length(raw1), ncol=iter))
  
  for(k in 1:iter)
  {
    dat <- simulWeib.m1(lambda, rho, 
                        beta1, raw1)
    res[k] <- dat$time
  }
  return(rowMeans(res))
}


ts.repro.m2 <- function(iter, lambda, rho,
                        beta1, beta2, raw1, raw2)
{
  res <- data.frame(matrix(NA, nrow=length(raw1), ncol=iter))
  
  for(k in 1:iter)
  {
    dat <- simulWeib.m2(lambda, rho, 
                        beta1, beta2, raw1, raw2)
    res[k] <- dat$time
  }
  return(rowMeans(res))
}


ts1.A.m2 <- ts.repro.m2(iter=5, lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                        beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                        raw1=cull.A$size, raw2=cull.A$dmd.c)
ts1.A.m2.daily <- aggregate(ts1.A.m2, list(chdf.A$report), mean)

ts1.B.m2 <- ts.repro.m2(iter=5, lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                        beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                        raw1=cull.B$size, raw2=cull.B$dmd.c)
ts1.B.m2.daily <- aggregate(ts1.B.m2, list(chdf.B$report), mean)

ts1.C.m2.daily <- rbind(ts1.A.m2.daily, ts1.B.m2.daily)


# -----------
ts2.A.m2 <- ts.repro.m1(iter=5, lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1], 
                        beta1=disp.A.m1$coef[1], 
                        raw1=disp.A$size)
ts2.A.m2.daily <- aggregate(ts2.A.m2, list(chdf.A$report), mean)

ts2.B.m2 <- ts.repro.m2(iter=5, lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                        beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                        raw1=disp.B$size, raw2=disp.B$dmd.d)
ts2.B.m2.daily <- aggregate(ts2.B.m2, list(chdf.B$report), mean)

ts2.C.m2.daily <- rbind(ts2.A.m2.daily, ts2.B.m2.daily)


# ---- reproduce Fig 3. Daily delay time-series
graphics.off()
par("mar")
par(mar=c(4,4.5,2,1))
attach(mtcars)
layout(matrix(c(1), 1, 1, byrow = TRUE), heights=c(heights=c(2)))
colors.m = magma(10, alpha=0.8)
colors.v = viridis(10, alpha=0.8)
colors.m.lt = magma(10, alpha=0.4)
colors.v.lt = viridis(10, alpha=0.4)

ylim.max <- max(delay.1.daily$x, delay.2.daily$x)

plot(ts1.C.m2.daily, col=colors.m.lt[5], xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', pch=16, cex.lab=1.3)
polygon(c(min(ts1.C.m2.daily$Group.1), ts1.C.m2.daily$Group.1, max(ts1.C.m2.daily$Group.1)), 
        c(0, ts1.C.m2.daily$x, 0), border=NA, col=colors.m.lt[5])

par(new=TRUE)


plot(ts2.C.m2.daily, col=colors.v.lt[7], xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='Report date', ylab='Mean daily response delay', pch=16, cex.lab=1.3)
polygon(c(min(ts2.C.m2.daily$Group.1), ts2.C.m2.daily$Group.1, max(ts2.C.m2.daily$Group.1)), 
        c(0, ts2.C.m2.daily$x, 0), border=NA, col=colors.v.lt[7])

axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")












####################################################
####################################################
#################### SUPPL FIG #####################
####################################################
####################################################
plot(aggregate(chdf$delay.2, list(chdf$report), mean))
mean(chdf$delay.2[chdf$report<"2001-04-15"])


# ---- ** binary hazard plot
#cull.bh <- cull.C
#cull.bh$size.bh <- cull.C$size
#cull.bh <- cull.C[,-4] # don't copy the raw size column
cull.bh <- cull.C
disp.bh <- disp.C
cull.bh$size.bh <- cull.C$size <= 5.
disp.bh$size.bh <- disp.C$size <= 5.

cull.mbh.size <- coxph(Surv(delay.1, censor) ~ 
                       nnb5k + dmd.c + size.bh,  # assuming all counties equivalent
                       data=cull.bh)
disp.mbh.size <- coxph(Surv(delay.2, censor) ~ 
                       nnb5k + dmd.d + size.bh,  # assuming all counties equivalent
                       data=disp.bh)


cull.bh.size <- with(cull.bh, data.frame(size.bh=c(0,1), 
                                         nnb5k=rep(mean(nnb5k), 2), 
                                         dmd.c=rep(mean(dmd.c), 2))
)
disp.bh.size <- with(disp.bh, data.frame(size.bh=c(0,1), 
                                         nnb5k=rep(mean(nnb5k), 2), 
                                         dmd.d=rep(mean(dmd.d), 2))
)

png("bh.size.cull.png", width=4, height=3, units='in', res=400)
plot(survfit(cull.mbh.size, newdata=cull.bh.size), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days since report", ylab="proportion not yet culled", cex.lab=0.7, cex.axis=0.7)
legend("topright", legend=c(expression("farm size">"5"), expression("farm size"<="5")), lty=c(1,2),inset=0.02, cex=0.7)
dev.off()

png("bh.size.disp.png", width=4, height=3, units='in', res=400)
plot(survfit(disp.mbh.size, newdata=disp.bh.size), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days since cull", ylab="proportion not yet disposed of", cex.lab=0.7, cex.axis=0.7)
legend("topright", legend=c(expression("farm size">"5"), expression("farm size"<="5")), lty=c(1,2),inset=0.02, cex=0.7)
dev.off()
#######################
cull.bh$dmd.c.bh <- cull.C$dmd.c <= 1
disp.bh$dmd.d.bh <- disp.C$dmd.d <= 1

cull.mbh.dmd.c <- coxph(Surv(delay.1, censor) ~ 
                        nnb5k + dmd.c.bh + size,  # assuming all counties equivalent
                        data=cull.bh)
disp.mbh.dmd.d <- coxph(Surv(delay.2, censor) ~ 
                        nnb5k + dmd.d.bh + size,  # assuming all counties equivalent
                        data=disp.bh)

cull.bh.dmd.c <- with(cull.bh, data.frame(dmd.c.bh=c(0,1), 
                                          nnb5k=rep(mean(nnb5k), 2), 
                                          size=rep(mean(size), 2))
)
disp.bh.dmd.d <- with(disp.bh, data.frame(dmd.d.bh=c(0,1), 
                                          nnb5k=rep(mean(nnb5k), 2), 
                                          size=rep(mean(size), 2))
)

png("bh.dmd.cull.png", width=4, height=3, units='in', res=400)
plot(survfit(cull.mbh.dmd.c, newdata=cull.bh.dmd.c), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days since report", ylab="proportion not yet culled", cex.lab=0.7, cex.axis=0.7)
legend("topright", legend=c(expression("control demand">"1"), expression("control demand"<="1")), lty=c(1,2),inset=0.02, cex=0.7)
dev.off()

png("bh.dmd.disp.png", width=4, height=3, units='in', res=400)
plot(survfit(disp.mbh.dmd.d, newdata=disp.bh.dmd.d), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days since cull", ylab="proportion not yet disposed of", cex.lab=0.7, cex.axis=0.7)
legend("topright", legend=c(expression("control demand">"1"), expression("control demand"<="1")), lty=c(1,2),inset=0.02, cex=0.7)
dev.off()
#######################
cull.bh$nnb5k.bh <- cull.C$nnb5k <= 1
disp.bh$nnb5k.bh <- disp.C$nnb5k <= 1

cull.mbh.nnb5k <- coxph(Surv(delay.1, censor) ~ 
                        nnb5k.bh + dmd.c + size,  # assuming all counties equivalent
                        data=cull.bh)
disp.mbh.nnb5k <- coxph(Surv(delay.2, censor) ~ 
                        nnb5k.bh + dmd.d + size,  # assuming all counties equivalent
                        data=disp.bh)

cull.bh.nnb5k <- with(cull.bh, data.frame(nnb5k.bh=c(0,1), 
                                          size=rep(mean(size), 2), 
                                          dmd.c=rep(mean(dmd.c), 2))
)
disp.bh.nnb5k <- with(disp.bh, data.frame(nnb5k.bh=c(0,1), 
                                          size=rep(mean(size), 2), 
                                          dmd.d=rep(mean(dmd.d), 2))
)

plot(survfit(cull.mbh.nnb5k, newdata=cull.bh.nnb5k), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days", ylab="proportion not yet culled")
legend("topright", legend=c(expression("farm cluster">"1"), expression("farm cluster"<="1")), lty=c(1,2),inset=0.02)

plot(survfit(disp.mbh.nnb5k, newdata=disp.bh.nnb5k), conf.int=TRUE, 
     xlim=c(0,7),lty=c(1,2),
     xlab="days", ylab="proportion not yet disposed of")
legend("topright", legend=c(expression("farm cluster">"1"), expression("farm cluster"<="1")), lty=c(1,2),inset=0.02)


####################################################
################ PLOT MIKE'S RESULT ################
####################################################

# setwd to the correct directory: Mikes output/xxx/
setwd('~/Dropbox/Research/Projects/Ongoing/FMD_farm/Mikes output/03.21.19/')

nodelay <- read.table('data_nodelay')
nodelay$ctrl <- 'nodelay'
fixed <- read.table('data_fixed_delay')
fixed$ctrl <- 'fixed'
fixed4 <- read.table('data_fixeddel_postmar')
fixed4$ctrl <- 'fixed4'
empr <- read.table('data_empirical_delay')
empr$ctrl <- 'empr'
context <- read.table('data_contextdelay')
context$ctrl <- 'context'

comp <- rbind.data.frame(nodelay[c(8,11,14)], 
                         fixed[c(8,11,14)], 
                         fixed4[c(8,11,14)], 
                         empr[c(8,11,14)], 
                         context[c(8,11,14)])
colnames(comp)[c(1,2)] <- c('dur', 'ncull')

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum <- ddply(data, groupnames, .fun=summary_func, varname)
  data_sum <- rename(data_sum, c('mean'=varname))
  return(data_sum)
}

library(ggplot2)
df.dur <- data_summary(comp, varname='dur', groupnames='ctrl')
# Standard deviation of the mean as error bar
p <- ggplot(df.dur, aes(x=ctrl, y=dur)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=dur-sd, ymax=dur+sd), width=.1)
p + scale_fill_grey() + xlab("delay model") + ylab("duration") + theme_classic() 

df.ncull <- data_summary(comp, varname='ncull', groupnames='ctrl')
# Standard deviation of the mean as error bar
p <- ggplot(df.ncull, aes(x=ctrl, y=ncull)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=ncull-sd, ymax=ncull+sd), width=.1)
p + scale_fill_grey() + xlab("delay model") + ylab("# livestock culled") + theme_classic() 




####################################################
############## DC/SOS Reclass. compute #############
####################################################
dat.dc.raw <- read.csv("dc.csv")
dat.dc.raw.fmdno <-dat.dc.raw[complete.cases(dat.dc.raw[,1]), ]
dc.fmdno <- dat.dc.raw.fmdno$FMDNo
nrow(dat.der[dat.der$FMDNo %in% dc.fmdno,])

dat.sos.raw <- read.csv("sos.csv")
dat.sos.raw.fmdno <-dat.sos.raw[complete.cases(dat.sos.raw[,1]), ]
sos.fmdno <- dat.sos.raw.fmdno$FMDNo
nrow(dat.der[dat.der$FMDNo %in% sos.fmdno,])












###################################### 
################ MISC ################
######################################

# ---- ** constraint on culling
# country-level no. of culls per day
hist((as.vector(table(chdf$slaughter))), xlab = 'daily # of farms culled') 
# check max using raw (unfiltered) data
max(table(as.Date(strptime(dat.raw$DateSlaughtered, "%m/%d/%y %H:%M", tz="GMT"))))


# ---- returns the interpolated density of any input delay (in days)
ap <- approxfun(density(cull$delay.1))
ap(3) # example

# ----- test supp plot (redesign)
# (b) daily demand (on cull)
dmd.c.daily <- aggregate(chdf$dmd.c, list(chdf$slaughter), mean)
ylim.max <- 167.6333 #same as (c) ##max(dmd.c.daily$x * 10)

plot(dmd.c.daily, col='gray', xaxt='n', type='l', xlim=c(date.min, date.max), ylim=c(0, ylim.max),
     xlab='', ylab='', main='Mean daily control demand on culling', pch=16, cex.main=1.3)
polygon(c(min(dmd.c.daily$Group.1), dmd.c.daily$Group.1, max(dmd.c.daily$Group.1)), 
        c(0, dmd.c.daily$x * 10, 0), border=NA, col='gray')
axis.Date(side=1, at=seq(min(report.date), max(disposal.date), 1), format="%m-%d")



# check equation works as expected
simulWeib.test <- function(N, lambda, rho, beta, rateC)
{
  # covariate --> N Bernoulli trials
  x <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # Weibull latent event times
  v <- runif(n=N)
  Tlat <- (- log(v) / (lambda * exp(x * beta))) ^ (1 / rho)
  #Tlat <- (- log(v)) ^ (1/rho) / (lambda * exp(x * beta))
  # censoring times
  C <- rexp(n=N, rate=rateC)
  
  # follow-up times and event indicators
  time <- pmin(Tlat, C)
  status <- as.numeric(Tlat <= C)
  
  # data set
  data.frame(id=1:N,
             time=time,
             status=status,
             x=x)
}

set.seed(1234)
betaBar <- rep(NA, 1e3)
for(k in 1:1e3)
{
  dat.test <- simulWeib.test(N=100, lambda=0.01, rho=2, beta=-0.6, rateC=0.001)
  fit.test <- coxph(Surv(time, status) ~ x, data=dat.test)
  betaBar[k] <- fit.test$coef
}

mean(betaBar)



dat.test <- simulWeib.test(N=1000, lambda=0.075, rho=4, beta=-0.6, rateC=0.001)
fit.test <- flexsurvreg(Surv(time, status) ~ x,
            data = dat.test, dist = "weibull")
fit.test$res

# other potential variables
# 1. number of still-infected farms on ID date (globally)
# 2. number of still-infected farms on ID date (county-level)
# 3. number of farms being culled on ID start-cull date
# 4. number of farms being disposed on ID start-disposal date
# 5. number of neighboring farms within 1k (5k) radius
# 6. number of neighboring still-infected farms within 1k (5k) radius
# 7. number of neighboring being-culled farms within 1k (5k) radius
# 8. number of neighboring being-disposed farms within 1k (5k) radius
# 9. days since the start of the epidemic
# 10. days since the peak of the epidemic
 
# other potential delay measure
# 1. date disposal finished - preliminary disinfection date
# 2. preliminary disinfection date - date C & D completed (cull & disinfection?)