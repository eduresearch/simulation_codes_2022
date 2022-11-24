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
set.seed(20220814)
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
    cfa.syntax <- paste('Item_', 1:number.items, sep = '')
    cfa.bi <- paste('bi =~', 'Item_1', paste('+', cfa.syntax[2:number.items], sep = '', collapse=' '))
    cfa.mul2.f1 <- paste('f1 =~', 'Item_1', paste('+', cfa.syntax[2:(number.items/2)], sep = '', collapse=' '))
    cfa.f2.1st <- paste('Item_', (number.items/2)+1, sep = '')
    cfa.mul2.f2 <- paste('f2 =~', cfa.f2.1st, paste('+', cfa.syntax[((number.items/2)+2):number.items], sep = '', collapse=' '))
    cfa.mul2.cov <- 'bi ~~ 0*f1 \n bi ~~ 0*f2 
                 f1 ~~ 0*f2' #fix covariance to zero
    cfa.mul.bi <- paste(cfa.bi, cfa.mul2.f1, cfa.mul2.f2, cfa.mul2.cov, sep = '\n') #set line break to each factor
    #set line break to each factor

  } 
  else if(number.factors==4){
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    
    #model syntax
    cfa.syntax <- paste('Item_', 1:number.items, sep = '')
    cfa.bi <- paste('bi =~', 'Item_1', paste('+', cfa.syntax[2:number.items], sep = '', collapse=' '))
    cfa.mul4.f1 <- paste('f1 =~', 'Item_1', 
                         paste('+', cfa.syntax[2:(number.items*(1/4))], sep = '', collapse=' '))
    
    cfa.f2.1st <- paste('Item_', (number.items*(1/4))+1, sep = '')
    cfa.f3.1st <- paste('Item_', (number.items*(2/4))+1, sep = '')
    cfa.f4.1st <- paste('Item_', (number.items*(3/4))+1, sep = '')
    
    cfa.mul4.f2 <- paste('f2 =~', cfa.f2.1st, 
                         paste('+', cfa.syntax[((number.items*(1/4))+2):(number.items*(2/4))], sep = '', collapse=' '))
    cfa.mul4.f3 <- paste('f3 =~', cfa.f3.1st, 
                         paste('+', cfa.syntax[((number.items*(2/4))+2):(number.items*(3/4))], sep = '', collapse=' '))
    cfa.mul4.f4 <- paste('f4 =~', cfa.f4.1st, 
                         paste('+', cfa.syntax[((number.items*(3/4))+2):number.items], sep = '', collapse=' '))
    
    cfa.mul4.cov <- 'bi ~~ 0*f1 \n bi ~~ 0*f2 \n bi ~~ 0*f3 \n bi ~~ 0*f4
                     f1 ~~ 0*f2 \n f1 ~~ 0*f3 \n f1 ~~ 0*f4 
                     f2 ~~ 0*f3 \n f2 ~~ 0*f4 
                     f3 ~~ 0*f4'
    cfa.mul.bi <- paste(cfa.bi, cfa.mul4.f1, cfa.mul4.f2, 
                        cfa.mul4.f3, cfa.mul4.f4, cfa.mul4.cov,  sep = '\n')
  }
  else{
    factors <- number.factors
    items <- number.items
    correlation.factors <- correlation.factors
    samplesize <- number.participants
    missing.per <- missing.percentage
    
    #model syntax
    cfa.syntax <- paste('Item_', 1:number.items, sep = '')
    cfa.bi <- paste('bi =~', 'Item_1', paste('+', cfa.syntax[2:number.items], sep = '', collapse=' '))
    
    cfa.mul6.f1 <- paste('f1 =~', 'Item_1', 
                         paste('+', cfa.syntax[2:(number.items*(1/6))], sep = '', collapse=' '))
    
    cfa.f2.1st <- paste('Item_', (number.items*(1/6))+1, sep = '')
    cfa.f3.1st <- paste('Item_', (number.items*(2/6))+1, sep = '')
    cfa.f4.1st <- paste('Item_', (number.items*(3/6))+1, sep = '')
    cfa.f5.1st <- paste('Item_', (number.items*(4/6))+1, sep = '')
    cfa.f6.1st <- paste('Item_', (number.items*(5/6))+1, sep = '')
    
    
    cfa.mul6.f2 <- paste('f2 =~', cfa.f2.1st, 
                         paste('+', cfa.syntax[((number.items*(1/6))+2):(number.items*(2/6))], sep = '', collapse=' '))
    cfa.mul6.f3 <- paste('f3 =~', cfa.f3.1st, 
                         paste('+', cfa.syntax[((number.items*(2/6))+2):(number.items*(3/6))], sep = '', collapse=' '))
    cfa.mul6.f4 <- paste('f4 =~', cfa.f4.1st, 
                         paste('+', cfa.syntax[((number.items*(3/6))+2):(number.items*(4/6))], sep = '', collapse=' '))
    cfa.mul6.f5 <- paste('f5 =~', cfa.f5.1st, 
                         paste('+', cfa.syntax[((number.items*(4/6))+2):(number.items*(5/6))], sep = '', collapse=' '))
    cfa.mul6.f6 <- paste('f6 =~', cfa.f6.1st, 
                         paste('+', cfa.syntax[((number.items*(5/6))+2):number.items], sep = '', collapse=' '))
    
    cfa.mul6.cov <- 'bi ~~ 0*f1 \n bi ~~ 0*f2 \n bi ~~ 0*f3 \n bi ~~ 0*f4 \n bi ~~ 0*f5 \n bi ~~ 0*f6
                     f1 ~~ 0*f2 \n f1 ~~ 0*f3 \n f1 ~~ 0*f4 \n f1 ~~ 0*f5 \n f1 ~~ 0*f6 
                     f2 ~~ 0*f3 \n f2 ~~ 0*f4 \n f2 ~~ 0*f5 \n f2 ~~ 0*f6
                     f3 ~~ 0*f4 \n f3 ~~ 0*f5 \n f3 ~~ 0*f6
                     f4 ~~ 0*f5 \n f4 ~~ 0*f6
                     f5 ~~ 0*f6'
    
    cfa.mul.bi <- paste(cfa.bi, cfa.mul6.f1, cfa.mul6.f2, cfa.mul6.f3,
                        cfa.mul6.f4, cfa.mul6.f5, cfa.mul6.f6,  cfa.mul6.cov,
                        sep = '\n') #set line break to each factor
  }
  return(list(factors, items, correlation.factors,samplesize, 
              missing.per, cfa.mul.bi))
}

model.simupara <- mapply(simudata.para.setting,
                         combinations$number.factors,
                         combinations$number.items,
                         combinations$correlation.factors,
                         combinations$number.participants,
                         combinations$missing.percentage)
rownames(model.simupara)<- c('factors','items','correlation.factors','sample.size', 
                             'missing', 'cfa.model') 
all.simupara <- t(model.simupara)
nrow(all.simupara)
all.simupara[22, c('cfa.model')]

datalist = list()
total.simudata = list()
for (k in 1:54){
  for (i in 1:100) {
    simu.data <-lavaan::simulateData(model=all.simupara[[k,c('cfa.model')]], 
                         sample.nobs=all.simupara[[k,c('sample.size')]], 
                         model.type = "cfa", 
                         std.lv=T, standardized = T,
                         return.type='data.frame')
    #dichotomize data, the mean of 
    simu.data[simu.data < 0] <- 0
    simu.data[simu.data > 0] <- 1
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

total.simudata[[47]]
total.simudata[[54]]$data[[1]]
saveRDS(total.simudata, file = 'C:/Users/u0124272/OneDrive - KU Leuven/Desktop/bi84_cfa_simudata_100.rds')




