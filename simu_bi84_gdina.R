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
set.seed(20220821)
# parameter setting for all combinations 324
number.factors <- c(2, 4, 6)
number.items <- c(60, 72, 84)#24 36 48
correlation.factors <- NA
number.participants <- c(500, 1000, 1500)
missing.percentage <- c(0, 0.05) 

combinations <- tidyr::crossing(number.factors, number.items, correlation.factors,
                                number.participants, missing.percentage)
nrow(combinations)#54 combinations
combinations[1,]

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
    
    #model syntax
    cdm.bi.f1 <- rep(1, each=(number.items))
    cdm.bi.f2 <- rep(c(1,0), each=(number.items*(1/2)))
    cdm.bi.f3 <- rep(c(0,1), each=(number.items*(1/2)))
    cdm.bi.qm <- data.frame(cdm.bi.f1, cdm.bi.f2, cdm.bi.f3)
    cdm.bi.qm <- as.matrix(cdm.bi.qm) #set slop parameter

  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    
    #model syntax
    cdm.bi.f1 <- rep(1, each=(number.items))
    cdm.bi.f2 <- rep(c(1,0,0,0), each=(number.items*(1/4)))
    cdm.bi.f3 <- rep(c(0,1,0,0), each=(number.items*(1/4)))
    cdm.bi.f4 <- rep(c(0,0,1,0), each=(number.items*(1/4)))
    cdm.bi.f5 <- rep(c(0,0,0,1), each=(number.items*(1/4)))
    cdm.bi.qm <- data.frame(cdm.bi.f1, cdm.bi.f2, cdm.bi.f3,
                            cdm.bi.f4, cdm.bi.f5)
    cdm.bi.qm <- as.matrix(cdm.bi.qm)
  }
  else{
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    
    #model syntax
    cdm.bi.f1 <- rep(1, each=(number.items))
    cdm.bi.f2 <- rep(c(1,0,0,0,0,0), each=(number.items*(1/6)))
    cdm.bi.f3 <- rep(c(0,1,0,0,0,0), each=(number.items*(1/6)))
    cdm.bi.f4 <- rep(c(0,0,1,0,0,0), each=(number.items*(1/6)))
    cdm.bi.f5 <- rep(c(0,0,0,1,0,0), each=(number.items*(1/6)))
    cdm.bi.f6 <- rep(c(0,0,0,0,1,0), each=(number.items*(1/6)))
    cdm.bi.f7 <- rep(c(0,0,0,0,0,1), each=(number.items*(1/6)))
    cdm.bi.qm <- data.frame(cdm.bi.f1, cdm.bi.f2, 
                            cdm.bi.f3, cdm.bi.f4,
                            cdm.bi.f5, cdm.bi.f6, cdm.bi.f7)
    cdm.bi.qm <- as.matrix(cdm.bi.qm)
  }
  return(list(factors, items, correlation.factors,samplesize, 
              missing.per, cdm.bi.qm))
}

model.simupara <- mapply(simudata.para.setting,
                         combinations$number.factors,
                         combinations$number.items,
                         combinations$correlation.factors,
                         combinations$number.participants,
                         combinations$missing.percentage)
rownames(model.simupara)<- c('factors','items','correlation.factors','sample.size', 
                             'missing', 'gdina.qm') 
all.simupara <- t(model.simupara)
nrow(all.simupara)
all.simupara[22, c('gdina.qm')]

datalist = list()
total.simudata = list()
for (k in 1:54){
  for (i in 1:100) {
    J <- nrow(all.simupara[[k,c('gdina.qm')]])
    gs <- data.frame(guess=rep(0.05,J),slip=rep(0.05,J))
    
    simu <- GDINA::simGDINA(N=all.simupara[[k,c('sample.size')]],
                            Q=all.simupara[[k,c('gdina.qm')]],
                            gs.parm = gs,
                            model = "GDINA")
    # simulated data
    simu.data <- GDINA::extract(simu,what = "dat")
    simu.data <- as.data.frame(simu.data)
    col <- 1:J
    new.name <- paste('Item_', col, sep = '')
    colnames(simu.data) <- new.name 
    #generate dataset with missing values
    simudata <-  missForest::prodNA(simu.data, noNA = all.simupara[[k,c('missing')]] ) #missing percentage
    simudata$i <- i
    datalist[[i]] <- simudata
    big.data = do.call(dplyr::bind_rows, datalist)
    names(big.data)[names(big.data) == "i"] <- "simu"
    by_simu <- big.data %>% group_by(simu) %>% nest()
    total.simudata[[k]] <- by_simu #here total.simudata is a list to save nested dataset
  }
}

total.simudata[[54]]
total.simudata[[12]]$data[[1]]
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/bi84_gdina_simudata_100.rds')





