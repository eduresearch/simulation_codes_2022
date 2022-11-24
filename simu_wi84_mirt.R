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
set.seed(20220799)
# parameter setting for all combinations 324
number.factors <- c(2, 4, 6)
number.items <- c(60, 72, 84)
correlation.factors <- c(0.2, 0.4, 0.6)
number.participants <- c(500, 1000, 1500)
missing.percentage <- c(0, 0.05) 

combinations <- tidyr::crossing(number.factors, number.items, correlation.factors,
                                number.participants, missing.percentage)
nrow(combinations)#162 combinations
combinations[1,]

# create simulation parameter setting
wi.simupara.setting <- function(number.factors,  number.items, correlation.factors,
                                  number.participants, missing.percentage){
  if(number.factors==2){
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'EM'
    
    mirt.simu.d <- data.frame(rnorm(number.items)) #set difficulty parameter
    mirt.simu.d <- as.matrix(mirt.simu.d)
    # 2 factors: 1/2 items are related to 2 factors
    simu.f1 <- rep(c(1,0), c(number.items*(3/4),number.items*(1/4)))
    simu.f2 <- rep(c(0,1), c(number.items*(1/4),number.items*(3/4)))
    simu.a <- data.frame(simu.f1, simu.f2)
    simu.a <- as.matrix(simu.a) 
   
    dim <- number.factors
    mirt.simu.cov <- diag(1, dim)
    mirt.simu.cov[mirt.simu.cov==0] <- correlation.factors #set correlation matrix for factors
    
  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <- '2PL'
    cmirt.esti.method <- 'MHRM'
    
    mirt.simu.d <- data.frame(rnorm(number.items))
    mirt.simu.d <- as.matrix(mirt.simu.d)
    # 4 factors: 1/4 items are related to 4 factors
    simu.f1 <- rep(c(1,0,1,0), c(number.items*(1/4), number.items*(1/4), number.items*(1/4), number.items*(1/4)))
    simu.f2 <- rep(c(0,1,1,0), c(number.items*(1/4), number.items*(1/4), number.items*(1/4), number.items*(1/4)))
    simu.f3 <- rep(c(0,0,1,0), c(number.items*(1/4), number.items*(1/4), number.items*(1/4), number.items*(1/4)))
    simu.f4 <- rep(c(0,0,1,1), c(number.items*(1/4), number.items*(1/4), number.items*(1/4), number.items*(1/4)))
    simu.a <- data.frame(simu.f1, simu.f2, simu.f3, simu.f4)
    simu.a <- as.matrix(simu.a)
  
    dim <- number.factors
    mirt.simu.cov <- diag(1, dim)
    mirt.simu.cov[mirt.simu.cov==0] <- correlation.factors
    
  }
  else{
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'MHRM'
    mirt.simu.d <- data.frame(rnorm(number.items))
    mirt.simu.d <- as.matrix(mirt.simu.d)
    # 6 factors: 1/6 items are related to 6 factors
    simu.f1 <- rep(c(1,0,0,0,1,0), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.f2 <- rep(c(0,1,0,0,1,0), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.f3 <- rep(c(0,0,1,0,1,0), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.f4 <- rep(c(0,0,0,1,1,0), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.f5 <- rep(c(0,0,0,0,1,0), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.f6 <- rep(c(0,0,0,0,1,1), c(number.items*(1/6), number.items*(1/6), 
                                     number.items*(1/6), number.items*(1/6),
                                     number.items*(1/6), number.items*(1/6)))
    simu.a <- data.frame(simu.f1, simu.f2, 
                         simu.f3, simu.f4,
                         simu.f5, simu.f6)
    simu.a <- as.matrix(simu.a)
   
    dim <- number.factors
    mirt.simu.cov <- diag(1, dim)
    mirt.simu.cov[mirt.simu.cov==0] <- correlation.factors
    
  }
  return(list(factors, items, correlation.factors,samplesize, 
              missing.per, cmirt.modeltype, cmirt.esti.method,
              mirt.simu.d, simu.a,mirt.simu.cov))
}


# parameter generation
wi.simupara <- mapply(wi.simupara.setting,
                      combinations$number.factors,
                      combinations$number.items,
                      combinations$correlation.factors,
                      combinations$number.participants,
                      combinations$missing.percentage)
rownames(wi.simupara)<- c('factors','items','correlation.factors','sample.size', 
                          'missing', 'cmirt.modeltype', 'cmirt.esti.method',
                          'mirt.simu.d', 'mirt.simu.a', 'mirt.simu.cov') 

all.simupara <- t(wi.simupara)
nrow(all.simupara) #162 parameter settings for 162 combinations
all.simupara[162, c('mirt.simu.d', 'mirt.simu.a', 'mirt.simu.cov')]


# use parallel to generate more dataset
datalist = list()
total.simudata = list()
for (k in 1:162){
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
total.simudata[[161]]$data[[1]]
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/wi84_mirt-new_simudata_100.rds')
