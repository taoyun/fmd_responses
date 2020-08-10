# testing and plotting Mike's new data from 12/2019
# this version 2 also re-design Kevin's plot to have demand on the x-axis, with farm size grouped


#setwd('~/Downloads/')
setwd('~/Dropbox/Research/Projects/Ongoing/FMD Responses/Mikes output/20.05.10/single_sim_details/')

df.fixed.raw1 <- read.table('spatialdata_24_48')
df.fixed.raw2 <- read.table('spatialdata_24_48_2')
df.fixed.raw2$V9 <- df.fixed.raw2$V9 + 100
df.fixed.raw <- rbind(df.fixed.raw1, df.fixed.raw2)

df.empirical.raw1 <- read.table('spatialdata_empirical')
df.empirical.raw2 <- read.table('spatialdata_empirical_2')
df.empirical.raw2$V9 <- df.empirical.raw2$V9 + 100
df.empirical.raw <- rbind(df.empirical.raw1, df.empirical.raw2)

df.context.1.raw1 <- read.table('spatialdata_context')
df.context.1.raw2 <- read.table('spatialdata_context_2')
df.context.1.raw2$V9 <- df.context.1.raw2$V9 + 100
df.context.1.raw <- rbind(df.context.1.raw1, df.context.1.raw2)

df.context.2.raw1 <- read.table('spatialdata_context_two_phase')
df.context.2.raw2 <- read.table('spatialdata_context_two_phase_2')
df.context.2.raw2$V9 <- df.context.2.raw2$V9 + 100
df.context.2.raw <- rbind(df.context.2.raw1, df.context.2.raw2)


#df.fixed.raw <- read.table('spatialdata_24_48') 
#df.empirical.raw <- read.table('spatialdata_empirical')
#df.context.1.raw <- read.table('spatialdata_context_dependent')
#df.context.2.raw <- read.table('spatialdata_context_two_phase')


colnames(df.fixed.raw) <- c('time', 'farm.id', 'x', 'y', 'state', 'farmI.id', 'county', 'ignore', 'sim')

# > unique(df.fixed$state)
# 1  - infected
# 4  - culled as IP
# 5  - culled DC (known)
# 6  - culled DC (risk)
# 11 - disposal

# ** applied to old data with single sim **
# find the farms without disposal record, ie. state=11
list.a<-sort(unique(df.fixed.raw[which(df.fixed.raw$state==11),]$farm.id))
list.b<-sort(unique(df.fixed.raw$farm.id))
farm.id.rm <- c(setdiff(list.b, list.a), setdiff(list.a, list.b))
df.fixed <- df.fixed.raw[-which(df.fixed.raw$farm.id %in% farm.id.rm),] #subset to omit the two farms above


# above script as a clean-up function for SINGLE SIM data
clean <- function(df.raw){
  colnames(df.raw) <- c('time', 'farm.id', 'x', 'y', 'state', 'farmI.id', 'county', 'ignore', 'sim')
  list.a<-sort(unique(df.raw[which(df.raw$state==11),]$farm.id))
  list.b<-sort(unique(df.raw$farm.id))
  farm.id.rm <- c(setdiff(list.b, list.a), setdiff(list.a, list.b))
  if (length(farm.id.rm) > 0) {
    df.out <- df.raw[-which(df.raw$farm.id %in% farm.id.rm),]
  } else {
    df.out <- df.raw
  }
  return(df.out)
}


# above script as a clean-up function for ALL SIMS(=200) data
clean.all <- function(df.raw){
  colnames(df.raw) <- c('time', 'farm.id', 'x', 'y', 'state', 'farmI.id', 'county', 'ignore', 'sim')
  out = NULL
  for (i in 1:200){
    df.raw.i <- subset(x = df.raw,
                       subset = sim == i)
    list.a<-sort(unique(df.raw.i[which(df.raw.i$state==11),]$farm.id))
    list.b<-sort(unique(df.raw.i$farm.id))
    farm.id.rm <- c(setdiff(list.b, list.a), setdiff(list.a, list.b))
    if (length(farm.id.rm) > 0) {
      df.out <- df.raw.i[-which(df.raw.i$farm.id %in% farm.id.rm),]
    } else {
      df.out <- df.raw.i
    }
    out = rbind(out, df.out)
  }
  return(out)
}


df.fixed <- clean.all(df.fixed.raw)
df.fixed.s1 <- subset(x = df.fixed, subset = sim == 123)
#df.fixed <- clean(df.fixed.raw)

# compute disposal delay distribution for SINGLE SIM data
delay.2.compute <- function(df){

  farm.id.unique <- unique(df$farm.id)
  farm.id.n <- length(farm.id.unique)

  delay.2 <- rep(-99, farm.id.n)
  slaughter.list <- rep(-99, farm.id.n)
  for (i in 1:farm.id.n){
    extract <- df[which(df$farm.id==farm.id.unique[i]),]
    slaughter <- extract[which((extract$state >=4) & (extract$state <=6)),]$time
    disposal <- extract[which((extract$state ==11)),]$time
  
  #print(c('farm.id.n', i))
  #print(c('          farm.id', farm.id.unique[i]))
  #print(c('          slaughter time', slaughter))
  #print(c('          disposal time', disposal))
  slaughter.list[i] <- slaughter
  delay.2[i] <- min(disposal - slaughter) #some farms (eg. 132155) have two disposal dates!?
  }
  return(list(slaughter.list, delay.2))
}



# compute disposal delay distribution for ALL SIMS(=10 due to computational time) data
delay.2.compute.all <- function(df){
  
  out.slaughter.list = NULL
  out.delay.2 = NULL
  for (i in 1:10){
    df.i <- subset(x = df,
                   subset = sim == i)
    farm.id.unique <- unique(df.i$farm.id)
    farm.id.n <- length(farm.id.unique)
    
    delay.2 <- rep(-99, farm.id.n)
    slaughter.list <- rep(-99, farm.id.n)
    for (i in 1:farm.id.n){
      extract <- df.i[which(df.i$farm.id==farm.id.unique[i]),]
      slaughter <- extract[which((extract$state >=4) & (extract$state <=6)),]$time
      disposal <- extract[which((extract$state ==11)),]$time
      
      slaughter.list[i] <- slaughter
      delay.2[i] <- min(disposal - slaughter)
    }
    out.slaughter.list = c(out.slaughter.list, slaughter.list)
    out.delay.2 = c(out.delay.2, delay.2)
  }

  return(list(out.slaughter.list, out.delay.2))
}



#mean(unlist(delay.2.compute(clean(df.fixed.raw))[2]))
#plot(unlist(delay.2.compute(clean(df.fixed.raw))[1]), unlist(delay.2.compute(clean(df.fixed.raw))[2]), 
#     main='fixed', xlim=c(0,420), ylim=c(0,45), xlab='cull time', ylab='disposal delay', col = alpha('black', 0.5))
mean(unlist(delay.2.compute.all(clean.all(df.fixed.raw))[2]))

#mean(unlist(delay.2.compute(clean(df.empirical.raw))[2]))
#plot(unlist(delay.2.compute(clean(df.empirical.raw))[1]), unlist(delay.2.compute(clean(df.empirical.raw))[2]), 
#     main='empirical', xlim=c(0,420), ylim=c(0,45), xlab='cull time', ylab='disposal delay', col = alpha('black', 0.5))
mean(unlist(delay.2.compute.all(clean.all(df.empirical.raw))[2]))

#mean(unlist(delay.2.compute(clean(df.context.1.raw))[2]))
#plot(unlist(delay.2.compute(clean(df.context.1.raw))[1]), unlist(delay.2.compute(clean(df.context.1.raw))[2]), 
#     main='context-dep', xlim=c(0,420), ylim=c(0,45), xlab='cull time', ylab='disposal delay', col = alpha('black', 0.5))
mean(unlist(delay.2.compute.all(clean.all(df.context.1.raw))[2]))

#mean(unlist(delay.2.compute(clean(df.context.2.raw))[2]))
#plot(unlist(delay.2.compute(clean(df.context.2.raw))[1]), unlist(delay.2.compute(clean(df.context.2.raw))[2]), 
#     main='context-dep + policy date', xlim=c(0,420), ylim=c(0,45), xlab='cull time', ylab='disposal delay', col = alpha('black', 0.5))
mean(unlist(delay.2.compute.all(clean.all(df.context.2.raw))[2]))



# compute epidemic curve: incidence based on infection date for SINGLE SIM data
incidence.compute <- function(df){
  
  df <- df[df$state ==1,]
  
  farm.id.unique <- unique(df$farm.id)
  farm.id.n <- length(farm.id.unique)
  
  infection.list <- rep(-99, farm.id.n)
  for (i in 1:farm.id.n){
    extract <- df[which(df$farm.id==farm.id.unique[i]),]
    infection <- extract[which((extract$state ==1)),]$time
    
    #print(c('farm.id.n', i))
    #print(c('          farm.id', farm.id.unique[i]))
    #print(c('          infection time', infection))
    infection.list[i] <- infection
  }
  return(infection.list)
}

# compute epidemic curve: incidence based on infection date for ALL SIMS(=200) data
# adds plotting
incidence.compute.all <- function(df, ref.plot, lc='k'){
  
  tmax <- 666 # max time = 666 across all control settings
  
  xlim_max <- 577
  if (ref.plot==TRUE){
    par(mfrow = c(1, 1))
    plot(NA, xlim = c(23, xlim_max), ylim = c(0, 85), 
         xlab='Simulation time steps (days)', ylab='Daily infections', cex.lab=0.9, cex.axis=0.9)
  }
  
  # loop through simulations
  for (i in 1:200){
    print(c('sim #', i))
    
    df.i <- subset(x = df,
                   subset = sim == i)
    
    df.i <- df.i[df.i$state ==1,]
    
    farm.id.unique <- unique(df.i$farm.id)
    farm.id.n <- length(farm.id.unique)
    
    infection.list <- rep(-99, farm.id.n)
    
    # loop through all unique farms with infection entry
    for (j in 1:farm.id.n){
      extract <- df.i[which(df.i$farm.id==farm.id.unique[j]),]
      infection <- extract[which((extract$state ==1)),]$time
      
      print(c('farm.id.n', j, infection))
      infection.list[j] <- infection
    }
    
    # pad sequential list
    infection.list <- c(infection.list, seq(23, tmax, 1))
    # unpad and remove zeroth built-in values
    infection.list.correct <- (table(infection.list)-1)[-1]
    
    # plot colors
    if (lc=='b'){
      cc <- alpha(rgb(0,0.2,1), 0.01)
    } else if (lc=='r'){
      cc <- alpha(rgb(1,0,0.4), 0.01)
    } else if (lc=='g'){
      cc <- alpha(rgb(0,1,0), 0.01)
    } else {
      cc <- alpha(rgb(0,0,0), 0.03)
    }
    #return(infection.list.correct)
    lines(as.numeric(infection.list.correct), xlim = c(23, xlim_max), ylim = c(0, 85), type="l", col = cc)
  }
}

library(scales)

tiff("incidence_context.2-fixed.tiff", width=2500, height=1600, res=400)
incidence.compute.all(clean.all(df.context.2.raw), TRUE)
incidence.compute.all(clean.all(df.fixed.raw), FALSE, lc='g')
dev.off()

tiff("incidence_context.2-empr.tiff", width=2500, height=1600, res=400)
incidence.compute.all(clean.all(df.context.2.raw), TRUE)
incidence.compute.all(clean.all(df.empirical.raw), FALSE, lc='b')
dev.off()

tiff("incidence_context.2-context.1.tiff", width=2500, height=1600, res=400)
incidence.compute.all(clean.all(df.context.2.raw), TRUE)
incidence.compute.all(clean.all(df.context.1.raw), FALSE, lc='r')
dev.off()




#plot(table(incidence.compute(clean(df.fixed.raw))), 
#     main='fixed', xlim=c(0,420), ylim=c(0,70), xlab='infection time', ylab='incidence')

#plot(table(incidence.compute(clean(df.empirical.raw))), 
#     main='empirical', xlim=c(0,420), ylim=c(0,70), xlab='infection time', ylab='incidence')

#plot(table(incidence.compute(clean(df.context.1.raw))), 
#     main='context-dep', xlim=c(0,420), ylim=c(0,70), xlab='infection time', ylab='incidence')

#plot(table(incidence.compute(clean(df.context.2.raw))), 
#     main='context-dep + policy date', xlim=c(0,420), ylim=c(0,70), xlab='infection time', ylab='incidence')














# plot the comparative results
setwd('~/Dropbox/Research/Projects/Ongoing/FMD Responses/Mikes output/20.05.10/final_sims_output/')

colRename <- function(x){
  colnames(x) <- c('IP#', 'DC#.risk', 'CP#', 'DC#.known', 'VaxFarm#', 'VaxAnim#', 'VaxDose',
                   'Dur', 'VaxRad.inn', 'VaxRad.out', 'CullAnim#', 'RingCullFarm#', 'RingCullRad')
  return(x)
}

out.fixed <- rbind(read.table('finaldata_24_48'), read.table('finaldata_24_48_2'))
out.empirical <- rbind(read.table('finaldata_empirical'), read.table('finaldata_empirical_2'))
out.context.1 <- rbind(read.table('finaldata_context'), read.table('finaldata_context_2'))
out.context.2 <- rbind(read.table('finaldata_context_two_phase'), read.table('finaldata_context_two_phase_2'))

out.fixed <- colRename(out.fixed)
out.empirical <- colRename(out.empirical)
out.context.1 <- colRename(out.context.1)
out.context.2 <- colRename(out.context.2)

out.fixed$IPDC.tot <- out.fixed$`IP#` + out.fixed$`DC#.known`
out.empirical$IPDC.tot <- out.empirical$`IP#` + out.empirical$`DC#.known`
out.context.1$IPDC.tot <- out.context.1$`IP#` + out.context.1$`DC#.known`
out.context.2$IPDC.tot <- out.context.2$`IP#` + out.context.2$`DC#.known`

out.fixed$Response <- 'Ideal.'
out.empirical$Response <- 'Approx.'
out.context.1$Response <- 'OS1'
out.context.2$Response <- 'OS2'

out.comp <- rbind.data.frame(out.fixed[c('Dur', 'CullAnim#', 'IPDC.tot', 'Response')], 
                             out.empirical[c('Dur', 'CullAnim#', 'IPDC.tot', 'Response')], 
                             out.context.1[c('Dur', 'CullAnim#', 'IPDC.tot', 'Response')], 
                             out.context.2[c('Dur', 'CullAnim#', 'IPDC.tot', 'Response')])

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

sum.dur <- data_summary(out.comp, varname='Dur', groupnames='Response')
sum.can <- data_summary(out.comp, varname='CullAnim#', groupnames='Response')
sum.idt <- data_summary(out.comp, varname='IPDC.tot', groupnames='Response')

library(ggplot2)

## no longer used
# Standard deviation of the mean as error bar
p <- ggplot(sum.dur, aes(x=Response, y=Dur)) + 
     geom_bar(stat="identity") +
     geom_errorbar(aes(ymin=Dur-sd, ymax=Dur+sd), width=.1)
p + scale_fill_grey() + xlab("Delay model") + ylab("Duration") + theme_classic() 

p <- ggplot(sum.can, aes(x=Response, y=`CullAnim#`)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=`CullAnim#`-sd, ymax=`CullAnim#`+sd), width=.1)
p + scale_fill_grey() + xlab("Delay model") + ylab("# livestock culled") + theme_classic() 

p <- ggplot(sum.idt, aes(x=Response, y=`IPDC.tot`)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=`IPDC.tot`-sd, ymax=`IPDC.tot`+sd), width=.1)
p + scale_fill_grey() + xlab("Delay model") + ylab("# IP + DC") + theme_classic() 


library(gridExtra)
# Violin plot
dp1 <- ggplot(out.comp, aes(x=Response, y=Dur)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") +
  geom_boxplot(width=0.1) +
  labs(x="", y="Epidemic duration") +
  scale_y_continuous(labels = function(x) format(x, width = 7))
dp1 <- dp1 + theme_classic() + theme(legend.position="none", axis.title.y=element_text(size=10), axis.text.x=element_blank())

dp2 <- ggplot(out.comp, aes(x=Response, y=`CullAnim#`/1000000)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") +
  geom_boxplot(width=0.1) +
  labs(x="", y="Total # of animals culled (mil)") +
  scale_y_continuous(labels = function(x) format(x, width = 9))
dp2 <- dp2 + theme_classic() + theme(legend.position="none", axis.title.y=element_text(size=10), axis.text.x=element_blank())

dp3 <- ggplot(out.comp, aes(x=Response, y=`IPDC.tot`)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") +
  geom_boxplot(width=0.1) +
  labs(x="Responses", y="Total # of farms culled") +
  scale_y_continuous(labels = function(x) format(x, width = 6))
dp3 <- dp3 + theme_classic() + theme(legend.position="none", axis.title.y=element_text(size=10), axis.title.x=element_text(size=10), axis.text.x.bottom = element_text(size=9))

dp.all <- grid.arrange(dp1, dp2, dp3, ncol=1, nrow =3)

#tiff("final.tiff", pointsize=10, width=1600, height=2500, units='px', res=400)
#png("final.png", pointsize=5, width=1600, height=2500, units='px', res=600)

plot(dp.all)
ggsave("final2.eps", plot=dp.all, width=5, height=8, units='in')
#dev.off()



## test
c1 <- rgb(173,216,230, max=255, alpha=80, names='lt.blue')
c2 <- rgb(255,192,203, max=255, alpha=80, names='lt.pink')

A <- out.empirical$Dur
B <- out.fixed$Dur

b <- min(c(A,B))
e <- max(c(A,B))
ax <- pretty(b:e, n=120)

hgA <- hist(A, breaks=ax, plot=FALSE)
hgB <- hist(B, breaks=ax, plot=FALSE)
plot(hgA, col=c1)
plot(hgB, col=c2, add=TRUE)











##################################
####### Kevin's plot #############
##################################
setwd('~/Dropbox/Research/Projects/Ongoing/FMD Responses/Deck/')
library(tidyverse)

by_dmd <- function(mydata){
    select(mydata, c(delay, size, dmd)) %>%
    group_by(dmd) %>%
    summarize(averaged.delay = mean(delay))
}


#by_dmd.array <- function(mydata){
#  for (i in 1:nrow(mydata)){
#  select(mydata, c(delay, size, dmd)) %>%
#    group_by(dmd) %>%
#    summarize(averaged.delay = mean(delay))
#  }
#}


#dmd bounds
#bound <- 10.
bound <- 5.
## Generate sim data ####

simulWeib.m2.kl <- function(iter, lambda, rho, 
                            beta1, beta2, raw1, raw2)
{
  N <- length(raw1)
  
  # covariate
  x1 <- raw1 #sample(x=raw1, size=N, replace=FALSE)
  x2 <- raw2 #sample(x=raw2, size=N, replace=FALSE)
  
  timeAll <- data.frame(matrix(NA, nrow=iter, ncol=N))
  for (i in 1:iter){
    
    # Weibull latent event times
    v <- runif(n=N)
    #fixed
    lambdaStar <- (1/lambda) ^ rho
    Tlat <- (- log(v) / (lambdaStar * exp(x1 * beta1 + x2 * beta2))) ^ (1/rho)
  
    # censoring times
    C <- 42
    
    # follow-up times and event indicators
    time <- pmin(Tlat, C)
    timeAll[i,] <- time
  }
  
  timeMeans <- colMeans(timeAll)
  # data set
  data.frame(id=1:N,
             time=timeMeans,
             x1=x1,
             x2=x2)
}


# ** run main scripts first

graphics.off()
png("dataVsim_ac.png", width=5, height=10, units='in', res=400)
par("mar")
par(mar=c(4,4.5,2,1))
attach(mtcars)
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(heights=c(2,1,2,1)))

############ subfig A: Cull pre-April ##############
####################################################

# simulated data
Mcull.A <- simulWeib.m2.kl(iter=1000, lambda=fit1.A.m2$res[2], rho=fit1.A.m2$res[1], 
                           beta1=cull.A.m2$coef[1], beta2=cull.A.m2$coef[2],
                           raw1=cull.A$size, raw2=cull.A$dmd.c)
colnames(Mcull.A) <- c('id', 'delay.1', 'size', 'dmd.c')

Mcull.A.sL <- Mcull.A[which(Mcull.A$size<=bound),]
Mcull.A.sH <- Mcull.A[which(Mcull.A$size> bound),]

cALs <- data.frame(Mcull.A.sL$delay.1, Mcull.A.sL$size, round(Mcull.A.sL$dmd.c, 1))
colnames(cALs) <- c('delay', 'size', 'dmd')
cALs.out <- by_dmd(cALs)
plot(cALs.out$dmd, cALs.out$averaged.delay, col='blue', pch=1, type="b", lty=1, lwd=2.5,
     main = 'Cases reported before 1 April', xlab='', ylab='Culling delay', 
     xlim=c(.1,7.9), ylim=c(.1, 9), cex.main=1.8, cex.lab=1.5)

cAHs <- data.frame(Mcull.A.sH$delay.1, Mcull.A.sH$size, round(Mcull.A.sH$dmd.c, 1))
colnames(cAHs) <- c('delay', 'size', 'dmd')
cAHs.out <- by_dmd(cAHs)
lines(cAHs.out$dmd, cAHs.out$averaged.delay, col='red', pch=1, type="b", lty=1, lwd=2.5)
abline(h = 1, lwd=2, lty=2)

# observed data
cull.A.sL <- cull.A[which(cull.A$size<=bound),]
cull.A.sH <- cull.A[which(cull.A$size> bound),]

cALo <- data.frame(cull.A.sL$delay.1, cull.A.sL$size, round(cull.A.sL$dmd.c, 1))
print(paste('cALo N=', nrow(cALo)))
colnames(cALo) <- c('delay', 'size', 'dmd')
cALo.out <- by_dmd(cALo)
points(cALo.out$dmd, cALo.out$averaged.delay, col=rgb(0,0,1,0.6), pch=20)

cAHo <- data.frame(cull.A.sH$delay.1, cull.A.sH$size, round(cull.A.sH$dmd.c, 1))
print(paste('cAHo N=', nrow(cAHo)))
colnames(cAHo) <- c('delay', 'size', 'dmd')
cAHo.out <- by_dmd(cAHo)
points(cAHo.out$dmd, cAHo.out$averaged.delay, col=rgb(1,0,0,0.6), pch=20)

hist(cull.A$dmd.c, breaks=20, xlim=c(.1,7.9), main='', xlab='Control demand', col='wheat', cex.lab=1.5)


############ subfig C: Disp pre-April ##############
####################################################

# simulated data
#Mdisp.A <- simulWeib.m2.kl(iter=1000, lambda=fit2.A.m2$res[2], rho=fit2.A.m2$res[1], 
#                           beta1=disp.A.m2$coef[1], beta2=disp.A.m2$coef[2],
#                           raw1=disp.A$size, raw2=disp.A$dmd.d)
Mdisp.A <- simulWeib.m2.kl(iter=1000, lambda=fit2.A.m1$res[2], rho=fit2.A.m1$res[1], 
                           beta1=disp.A.m1$coef[1], beta2=0.0,
                           raw1=disp.A$size, raw2=disp.A$dmd.d)
colnames(Mdisp.A) <- c('id', 'delay.2', 'size', 'dmd.d')

Mdisp.A.sL <- Mdisp.A[which(Mdisp.A$size<=bound),]
Mdisp.A.sH <- Mdisp.A[which(Mdisp.A$size> bound),]

dALs <- data.frame(Mdisp.A.sL$delay.2, Mdisp.A.sL$size, round(Mdisp.A.sL$dmd.d, 1))
colnames(dALs) <- c('delay', 'size', 'dmd')
dALs.out <- by_dmd(dALs)
plot(dALs.out$dmd, dALs.out$averaged.delay, col='blue', pch=1, type="b", lty=1, lwd=2.5, 
     xlab='', ylab='Disposal delay',
     xlim=c(.1,17.6), ylim=c(.1, 10), cex.main=1.8, cex.lab=1.5)

dAHs <- data.frame(Mdisp.A.sH$delay.2, Mdisp.A.sH$size, round(Mdisp.A.sH$dmd.d, 1))
colnames(dAHs) <- c('delay', 'size', 'dmd')
dAHs.out <- by_dmd(dAHs)
lines(dAHs.out$dmd, dAHs.out$averaged.delay, col='red', pch=1, type="b", lty=1, lwd=2.5)
abline(h = 1, lwd=2, lty=2)

# observed data
disp.A.sL <- disp.A[which(disp.A$size<=bound),]
disp.A.sH <- disp.A[which(disp.A$size> bound),]

dALo <- data.frame(disp.A.sL$delay.2, disp.A.sL$size, round(disp.A.sL$dmd.d, 1))
print(paste('dALo N=', nrow(dALo)))
colnames(dALo) <- c('delay', 'size', 'dmd')
dALo.out <- by_dmd(dALo)
points(dALo.out$dmd, dALo.out$averaged.delay, col=rgb(0,0,1,0.6), pch=20)

dAHo <- data.frame(disp.A.sH$delay.2, disp.A.sH$size, round(disp.A.sH$dmd.d, 1))
print(paste('dAHo N=', nrow(dAHo)))
colnames(dAHo) <- c('delay', 'size', 'dmd')
dAHo.out <- by_dmd(dAHo)
points(dAHo.out$dmd, dAHo.out$averaged.delay, col=rgb(1,0,0,0.6), pch=20)

hist(disp.A$dmd.d, breaks=20, xlim=c(0.1,17.6), main='', xlab='Control demand', col='wheat', cex.lab=1.5)
dev.off()







graphics.off()
png("dataVsim_bd.png", width=5, height=10, units='in', res=400)
par("mar")
par(mar=c(4,4.5,2,1))
attach(mtcars)
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(heights=c(2,1,2,1)))

############ subfig B: Cull post-April ##############
####################################################

# simulated data
Mcull.B <- simulWeib.m2.kl(iter=1000, lambda=fit1.B.m2$res[2], rho=fit1.B.m2$res[1], 
                           beta1=cull.B.m2$coef[1], beta2=cull.B.m2$coef[2],
                           raw1=cull.B$size, raw2=cull.B$dmd.c)
colnames(Mcull.B) <- c('id', 'delay.1', 'size', 'dmd.c')

Mcull.B.sL <- Mcull.B[which(Mcull.B$size<=bound),]
Mcull.B.sH <- Mcull.B[which(Mcull.B$size> bound),]

cBLs <- data.frame(Mcull.B.sL$delay.1, Mcull.B.sL$size, round(Mcull.B.sL$dmd.c, 1))
colnames(cBLs) <- c('delay', 'size', 'dmd')
cBLs.out <- by_dmd(cBLs)
plot(cBLs.out$dmd, cBLs.out$averaged.delay, col='blue', pch=1, type="b", lty=1, lwd=2.5,
     main = 'Cases reported on/after 1 April', xlab='', ylab='Culling delay', 
     xlim=c(.1,5.8), ylim=c(.1, 5), cex.main=1.8, cex.lab=1.5)

cBHs <- data.frame(Mcull.B.sH$delay.1, Mcull.B.sH$size, round(Mcull.B.sH$dmd.c, 1))
colnames(cBHs) <- c('delay', 'size', 'dmd')
cBHs.out <- by_dmd(cBHs)
lines(cBHs.out$dmd, cBHs.out$averaged.delay, col='red', pch=1, type="b", lty=1, lwd=2.5)
abline(h = 1, lwd=2, lty=2)

# observed data
cull.B.sL <- cull.B[which(cull.B$size<=bound),]
cull.B.sH <- cull.B[which(cull.B$size> bound),]

cBLo <- data.frame(cull.B.sL$delay.1, cull.B.sL$size, round(cull.B.sL$dmd.c, 1))
print(paste('cBLo N=', nrow(cBLo)))
colnames(cBLo) <- c('delay', 'size', 'dmd')
cBLo.out <- by_dmd(cBLo)
points(cBLo.out$dmd, cBLo.out$averaged.delay, col=rgb(0,0,1,0.6), pch=20)

cBHo <- data.frame(cull.B.sH$delay.1, cull.B.sH$size, round(cull.B.sH$dmd.c, 1))
print(paste('cBHo N=', nrow(cBHo)))
colnames(cBHo) <- c('delay', 'size', 'dmd')
cBHo.out <- by_dmd(cBHo)
points(cBHo.out$dmd, cBHo.out$averaged.delay, col=rgb(1,0,0,0.6), pch=20)

hist(cull.B$dmd.c, breaks=20, xlim=c(.1,5.8), main='', xlab='Control demand', col='wheat', cex.lab=1.5)


############ subfig D: Disp post-April ##############
######################################################

# simulated data
Mdisp.B <- simulWeib.m2.kl(iter=1000, lambda=fit2.B.m2$res[2], rho=fit2.B.m2$res[1], 
                           beta1=disp.B.m2$coef[1], beta2=disp.B.m2$coef[2],
                           raw1=disp.B$size, raw2=disp.B$dmd.d)
colnames(Mdisp.B) <- c('id', 'delay.2', 'size', 'dmd.d')

Mdisp.B.sL <- Mdisp.B[which(Mdisp.B$size<=bound),]
Mdisp.B.sH <- Mdisp.B[which(Mdisp.B$size> bound),]

dBLs <- data.frame(Mdisp.B.sL$delay.2, Mdisp.B.sL$size, round(Mdisp.B.sL$dmd.d, 1))
colnames(dBLs) <- c('delay', 'size', 'dmd')
dBLs.out <- by_dmd(dBLs)
plot(dBLs.out$dmd, dBLs.out$averaged.delay, col='blue', pch=1, type="b", lty=1, lwd=2.5, 
     xlab='', ylab='Disposal delay',
     xlim=c(0.1,17.6), ylim=c(.1, 9), cex.main=1.8, cex.lab=1.5)

dBHs <- data.frame(Mdisp.B.sH$delay.2, Mdisp.B.sH$size, round(Mdisp.B.sH$dmd.d, 1))
colnames(dBHs) <- c('delay', 'size', 'dmd')
dBHs.out <- by_dmd(dBHs)
lines(dBHs.out$dmd, dBHs.out$averaged.delay, col='red', pch=1, type="b", lty=1, lwd=2.5)
abline(h = 1, lwd=2, lty=2)

# observed data
disp.B.sL <- disp.B[which(disp.B$size<=bound),]
disp.B.sH <- disp.B[which(disp.B$size> bound),]

dBLo <- data.frame(disp.B.sL$delay.2, disp.B.sL$size, round(disp.B.sL$dmd.d, 1))
print(paste('dBLo N=', nrow(dBLo)))
colnames(dBLo) <- c('delay', 'size', 'dmd')
dBLo.out <- by_dmd(dBLo)
points(dBLo.out$dmd, dBLo.out$averaged.delay, col=rgb(0,0,1,0.6), pch=20)

dBHo <- data.frame(disp.B.sH$delay.2, disp.B.sH$size, round(disp.B.sH$dmd.d, 1))
print(paste('dBHo N=', nrow(dBHo)))
colnames(dBHo) <- c('delay', 'size', 'dmd')
dBHo.out <- by_dmd(dBHo)
points(dBHo.out$dmd, dBHo.out$averaged.delay, col=rgb(1,0,0,0.6), pch=20)

hist(disp.B$dmd.d, breaks=20, xlim=c(0.1,17.6), main='', xlab='Control demand', col='wheat', cex.lab=1.5)
dev.off()

