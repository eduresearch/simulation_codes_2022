library('stats4')
library('lattice')
library('mirt')
library('psych')
library('lavaan')
library('Gifi')
library('sirt')
library('GDINA')
library('modelr')
library('tidyverse')
library('data.table')

#set a global seed for data simualtion
set.seed(20220710)
# parameter setting for all combinations 324
number.factors <- c(2, 4, 6)
number.items <- c(60, 72, 84)#24 36 48
number.participants <- c(500, 1000, 1500)
missing.percentage <- c(0, 0.05) 

combinations <- tidyr::crossing(number.factors, number.items, 
                                number.participants, missing.percentage)
nrow(combinations)#54 combinations
combinations[1,]

# create simulation parameter setting
tw.simupara.setting <- function(number.factors,  number.items, 
                                  number.participants, missing.percentage){
  if(number.factors==2){
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'MHRM'
    
    
    mirt.simu.d <- data.frame(rnorm(number.items)) #set difficulty parameter
    mirt.simu.d <- as.matrix(mirt.simu.d)
    mirt.simu.g1 <- rep(c(1,0), each=(number.items*(1/2)))
    mirt.simu.g2 <- rep(c(0,1), each=(number.items*(1/2)))
    mirt.simu.f1 <- rep(c(1,0), each=(number.items*(1/2)))
    mirt.simu.f2 <- rep(c(0,1), each=(number.items*(1/2)))
    mirt.simu.a <- data.frame(mirt.simu.g1,  mirt.simu.g2, mirt.simu.f1, mirt.simu.f2)
    mirt.simu.a <- as.matrix(mirt.simu.a) #set slop parameter
   
    dim <- number.factors+2
    mirt.simu.cov <- diag(1, dim)
    
  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <- '2PL'
    cmirt.esti.method <- 'MHRM'
    
    mirt.simu.d <- data.frame(rnorm(number.items))
    mirt.simu.d <- as.matrix(mirt.simu.d)
    mirt.simu.g1 <- rep(c(1,0), each=(number.items*(1/2)))
    mirt.simu.g2 <- rep(c(0,1), each=(number.items*(1/2)))
    mirt.simu.f1 <- rep(c(1,0,0,0), each=(number.items*(1/4)))
    mirt.simu.f2 <- rep(c(0,1,0,0), each=(number.items*(1/4)))
    mirt.simu.f3 <- rep(c(0,0,1,0), each=(number.items*(1/4)))
    mirt.simu.f4 <- rep(c(0,0,0,1), each=(number.items*(1/4)))
    mirt.simu.a <- data.frame(mirt.simu.g1,  mirt.simu.g2,
                              mirt.simu.f1, mirt.simu.f2, mirt.simu.f3, mirt.simu.f4)
    mirt.simu.a <- as.matrix(mirt.simu.a)
  
    dim <- number.factors+2
    mirt.simu.cov <- diag(1, dim)
    
  }
  else{
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'MHRM'
    
    mirt.simu.d <- data.frame(rnorm(number.items))
    mirt.simu.d <- as.matrix(mirt.simu.d)
    mirt.simu.g1 <- rep(c(1,0), each=(number.items*(1/2)))
    mirt.simu.g2 <- rep(c(0,1), each=(number.items*(1/2)))
    mirt.simu.f1 <- rep(c(1,0,0,0,0,0), each=(number.items*(1/6)))
    mirt.simu.f2 <- rep(c(0,1,0,0,0,0), each=(number.items*(1/6)))
    mirt.simu.f3 <- rep(c(0,0,1,0,0,0), each=(number.items*(1/6)))
    mirt.simu.f4 <- rep(c(0,0,0,1,0,0), each=(number.items*(1/6)))
    mirt.simu.f5 <- rep(c(0,0,0,0,1,0), each=(number.items*(1/6)))
    mirt.simu.f6 <- rep(c(0,0,0,0,0,1), each=(number.items*(1/6)))
    mirt.simu.a <- data.frame(mirt.simu.g1, mirt.simu.g2,
                              mirt.simu.f1, mirt.simu.f2, 
                              mirt.simu.f3, mirt.simu.f4,
                              mirt.simu.f5, mirt.simu.f6)
    mirt.simu.a <- as.matrix(mirt.simu.a)
   
    dim <- number.factors+2
    mirt.simu.cov <- diag(1, dim)
   
  }
  return(list(factors, items, samplesize, 
              missing.per, cmirt.modeltype, cmirt.esti.method,
              mirt.simu.d, mirt.simu.a,mirt.simu.cov))
}


# parameter generation
tw.simupara <- mapply(tw.simupara.setting,
                      combinations$number.factors,
                      combinations$number.items,
                      combinations$number.participants,
                      combinations$missing.percentage)
rownames(tw.simupara)<- c('factors','items','sample.size', 
                          'missing', 'cmirt.modeltype', 'cmirt.esti.method',
                          'mirt.simu.d', 'mirt.simu.a', 'mirt.simu.cov') 

all.simupara <- t(tw.simupara)
nrow(all.simupara) 
all.simupara[1, c('mirt.simu.d', 'mirt.simu.a', 'mirt.simu.cov')]


# use parallel to generate more dataset
datalist = list()
total.simudata = list()
for (k in 1:54){
  for (i in 1:100) {
    simu.data <- simdata(a=all.simupara[[k,c('mirt.simu.a')]], #slope
                            d=all.simupara[[k,c('mirt.simu.d')]], #difficulty
                            N=all.simupara[[k,c('sample.size')]], #sample size
                            sigma = all.simupara[[k,c('mirt.simu.cov')]], #correlation matrix
                            itemtype = all.simupara[[k,c('cmirt.modeltype')]])
    simudata.full <- data.frame(simu.data)
    #generate dataset with missing values
    simudata <-  missForest::prodNA(simudata.full, noNA = all.simupara[[k,c('missing')]] ) #missing percentage
    simudata$i <- i
    datalist[[i]] <- simudata
    big.data = do.call(dplyr::bind_rows, datalist)
    names(big.data)[names(big.data) == "i"] <- "simu"
    by_simu <- big.data %>% group_by(simu) %>% nest()
    total.simudata[[k]] <- by_simu #here total.simudata is a list to save nested dataset
  }
}

total.simudata[[47]]
total.simudata[[47]]$data[[1]]
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/tw84_mirt_simudata_100.rds')
