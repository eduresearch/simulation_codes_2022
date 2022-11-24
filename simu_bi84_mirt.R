# install.packages(c('tidyr','tidyverse','mirt'))
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


set.seed(20220715)

number.factors <- c(2, 4, 6)
number.items <- c(60, 72, 84)#24 36 48
number.participants <- c(500, 1000, 1500)
missing.percentage <- c(0, 0.05) 

combinations <- tidyr::crossing(number.factors, number.items, 
                                number.participants, missing.percentage)
nrow(combinations)#54 combinations
combinations[1,]

# create simulation parameter setting
simudata.para.setting <- function(number.factors,  number.items,
                                  number.participants, missing.percentage){
  if(number.factors==2){
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'EM'
    
    bi.simu.d <- data.frame(rnorm(number.items)) #set difficulty parameter
    bi.simu.d <- as.matrix(bi.simu.d)
    bi.simu.bi <- rep(1, each=(number.items))
    bi.simu.f1 <- rep(c(1,0), each=(number.items*(1/2)))
    bi.simu.f2 <- rep(c(0,1), each=(number.items*(1/2)))
    bi.simu.a <- data.frame(bi.simu.bi, bi.simu.f1, bi.simu.f2)
    bi.simu.a <- as.matrix(bi.simu.a) #set slop parameter
    dim <- number.factors+1
    bi.simu.cov <- diag(1, dim) 
    #specific/grouping factors are not allowed to be correlated
    
    
  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <- '2PL'
    cmirt.esti.method <- 'MHRM'
    
    bi.simu.d <- data.frame(rnorm(number.items))
    bi.simu.d <- as.matrix(bi.simu.d)
    
    bi.simu.bi <- rep(1, each=(number.items))
    bi.simu.f1 <- rep(c(1,0,0,0), each=(number.items*(1/4)))
    bi.simu.f2 <- rep(c(0,1,0,0), each=(number.items*(1/4)))
    bi.simu.f3 <- rep(c(0,0,1,0), each=(number.items*(1/4)))
    bi.simu.f4 <- rep(c(0,0,0,1), each=(number.items*(1/4)))
    bi.simu.a <- data.frame(bi.simu.bi, bi.simu.f1, bi.simu.f2, bi.simu.f3, bi.simu.f4)
    bi.simu.a <- as.matrix(bi.simu.a)
    dim <- number.factors+1
    bi.simu.cov <- diag(1, dim)
    
  }
  else{
    factors <- number.factors
    items <- number.items
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'MHRM'
    bi.simu.d <- data.frame(rnorm(number.items))
    bi.simu.d <- as.matrix(bi.simu.d)
    
    bi.simu.bi <- rep(1, each=(number.items))
    bi.simu.f1 <- rep(c(1,0,0,0,0,0), each=(number.items*(1/6)))
    bi.simu.f2 <- rep(c(0,1,0,0,0,0), each=(number.items*(1/6)))
    bi.simu.f3 <- rep(c(0,0,1,0,0,0), each=(number.items*(1/6)))
    bi.simu.f4 <- rep(c(0,0,0,1,0,0), each=(number.items*(1/6)))
    bi.simu.f5 <- rep(c(0,0,0,0,1,0), each=(number.items*(1/6)))
    bi.simu.f6 <- rep(c(0,0,0,0,0,1), each=(number.items*(1/6)))
    bi.simu.a <- data.frame(bi.simu.bi,
                            bi.simu.f1, bi.simu.f2, 
                            bi.simu.f3, bi.simu.f4,
                            bi.simu.f5, bi.simu.f6)
    bi.simu.a <- as.matrix(bi.simu.a)
    dim <- number.factors+1
    bi.simu.cov <- diag(1, dim)
    
  }
  return(list(factors, items, samplesize, 
              missing.per, cmirt.modeltype, cmirt.esti.method,
              bi.simu.d, bi.simu.a, bi.simu.cov))
}


bi.simupara <- mapply(simudata.para.setting,
                      combinations$number.factors,
                      combinations$number.items,
                      combinations$number.participants,
                      combinations$missing.percentage)
rownames(bi.simupara)<- c('factors','items','sample.size', 
                          'missing', 'cmirt.modeltype', 'cmirt.esti.method',
                          'bi.simu.d', 'bi.simu.a', 'cov.matrix') 
all.simupara <- t(bi.simupara)
nrow(all.simupara) 
all.simupara[1,]


# simulate datasets
datalist = list()
total.simudata = list()
for (k in 1:54){
  for (i in 1:100) {
    be.simu.data <- mirt::simdata(a=all.simupara[[k,c('bi.simu.a')]], #slope
                            d=all.simupara[[k,c('bi.simu.d')]], #difficulty
                            N=all.simupara[[k,c('sample.size')]], #sample size
                            sigma = all.simupara[[k,c('cov.matrix')]], #correlation matrix
                            itemtype = all.simupara[[k,c('cmirt.modeltype')]])
    simudata.full <- data.frame(be.simu.data)
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
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/bi84_mirt_simudata_100.rds')
