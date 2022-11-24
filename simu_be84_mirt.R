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
# parameter setting for all combinations 324
number.factors <- c(2, 4, 6)
number.items <- c(60, 72, 84)#24 36 48
correlation.factors <- c(0.2, 0.4, 0.6)
number.participants <- c(500, 1000, 1500)
missing.percentage <- c(0, 0.05) 

combinations <- tidyr::crossing(number.factors, number.items, correlation.factors,
                                number.participants, missing.percentage)
nrow(combinations)#162 combinations
combinations[1,]

# create simulation parameter setting
simudata.para.setting <- function(number.factors,  number.items, correlation.factors,
                                  number.participants, missing.percentage){
  if(number.factors==2){
    #randomly number from standard normal distribution
    #set seed by paste the number together with specific position?
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'EM'
    
    be.simu.d <- data.frame(rnorm(number.items)) #set difficulty parameter
    be.simu.d <- as.matrix(be.simu.d)
    be.simu.f1 <- rep(c(1,0), each=(number.items*(1/2)))
    be.simu.f2 <- rep(c(0,1), each=(number.items*(1/2)))
    be.simu.a <- data.frame(be.simu.f1, be.simu.f2)
    be.simu.a <- as.matrix(be.simu.a) #set slop parameter
    dim <- number.factors
    be.simu.cov <- diag(1, dim)
    be.simu.cov[be.simu.cov==0] <- correlation.factors #set correlation matrix for factors
    
  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <- '2PL'
    cmirt.esti.method <- 'MHRM'
    be.simu.d <- data.frame(rnorm(number.items))
    be.simu.d <- as.matrix(be.simu.d)
    be.simu.f1 <- rep(c(1,0,0,0), each=(number.items*(1/4)))
    be.simu.f2 <- rep(c(0,1,0,0), each=(number.items*(1/4)))
    be.simu.f3 <- rep(c(0,0,1,0), each=(number.items*(1/4)))
    be.simu.f4 <- rep(c(0,0,0,1), each=(number.items*(1/4)))
    be.simu.a <- data.frame(be.simu.f1, be.simu.f2, be.simu.f3, be.simu.f4)
    be.simu.a <- as.matrix(be.simu.a)
    dim <- number.factors
    be.simu.cov <- diag(1, dim)
    be.simu.cov[be.simu.cov==0] <- correlation.factors
    
  }
  else{
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    cmirt.modeltype <-'2PL'
    cmirt.esti.method <- 'MHRM'
    be.simu.d <- data.frame(rnorm(number.items))
    be.simu.d <- as.matrix(be.simu.d)
    be.simu.f1 <- rep(c(1,0,0,0,0,0), each=(number.items*(1/6)))
    be.simu.f2 <- rep(c(0,1,0,0,0,0), each=(number.items*(1/6)))
    be.simu.f3 <- rep(c(0,0,1,0,0,0), each=(number.items*(1/6)))
    be.simu.f4 <- rep(c(0,0,0,1,0,0), each=(number.items*(1/6)))
    be.simu.f5 <- rep(c(0,0,0,0,1,0), each=(number.items*(1/6)))
    be.simu.f6 <- rep(c(0,0,0,0,0,1), each=(number.items*(1/6)))
    be.simu.a <- data.frame(be.simu.f1, be.simu.f2, 
                            be.simu.f3, be.simu.f4,
                            be.simu.f5, be.simu.f6)
    be.simu.a <- as.matrix(be.simu.a)
    dim <- number.factors
    be.simu.cov <- diag(1, dim)
    be.simu.cov[be.simu.cov==0] <- correlation.factors
    
  }
  return(list(factors, items, correlation.factors,samplesize, 
              missing.per, cmirt.modeltype, cmirt.esti.method,
              be.simu.d, be.simu.a, be.simu.cov))
}

# test1 <- simudata.para.setting(number.factors = 6, number.items = 48,correlation.factors = 0.6) 
# install.packages('flow')
# library(flow)
# flow_view(simudata.para.setting)

be.simupara <- mapply(simudata.para.setting,
                      combinations$number.factors,
                      combinations$number.items,
                      combinations$correlation.factors,
                      combinations$number.participants,
                      combinations$missing.percentage)
rownames(be.simupara)<- c('factors','items','correlation.factors','sample.size', 
                          'missing', 'cmirt.modeltype', 'cmirt.esti.method',
                          'be.simu.d', 'be.simu.a', 'cov.matrix') 
all.simupara <- t(be.simupara)
nrow(all.simupara) #162 parameter settings for 162 combinations
all.simupara[1, ]

# using mapply function to handle all combinations 
# create simulation data parameter setting for each combination

# create 500 simulation datasets for each combinations (486 combinations in total)
# use parallel to generate more dataset
start.time <- Sys.time()
datalist = list()
total.simudata = list()
for (k in 1:162){
  for (i in 1:10) {
    be.simu.data <- simdata(a=all.simupara[[k,c('be.simu.a')]], #slope
                            d=all.simupara[[k,c('be.simu.d')]], #difficulty
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

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #Time difference of 23.70223 mins for 162*100, so 162*500 -> could be 24*5=120 mins

total.simudata[[47]]
total.simudata[[47]]$data[[100]]
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/itec_Phd_windows/confirmatory methods/codes_data/data_simulated/be84_mirt_simudata_10.rds')
#162*100 -> 361MB, so 162*500 -> could be over 30 GB

