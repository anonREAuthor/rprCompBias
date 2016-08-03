##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with APM data                   #
#                                                                                        #
##########################################################################################

##########################################################################################
# General purpose analytical functions working on all method types                       #
##########################################################################################

### Generic data tidying engine ----------------------------------------------------------

apmGenTidyer <- function(yield.results,
                         geo.level){
  
 ## Extract results
  
  geo.results <- yield.results[[which(names(yield.results) == geo.level)]]
  
  # House
  house <- geo.results$house$stsDF
  house$type <- 'house'
  house$geo.level <- geo.level
  
  if(geo.level == 'Global') {
    house$spaceName <- 'Global'
  } 
  
  # Unit
  unit <- geo.results$unit$stsDF
  unit$type <- 'unit'
  unit$geo.level <- geo.level
  
  if(geo.level == 'Global') {
    unit$spaceName <- 'Global'
  }
  
 ## Return Values
  
  return(rbind(house, unit))

}

### Function to wrap the generic tidyer over one geo level -------------------------------

apmGenTidyerGeoWrap <- function(yield.results,
                                geo.levels=apmOptions$geo.levels,
                                method.type='spag'){
   
 ## Make geo list
  
   tidy.list <- lapply(as.list(geo.levels), FUN = apmGenTidyer,
                       yield.results=yield.results)
   tidy.df <- rbind.fill(tidy.list)
  
 ## Add method type
   
   tidy.df$method <- method.type
   
 ## Fix remaining columns 
   
   if(method.type == 'spag'){
     names(tidy.df)[1:2] <- c('time', 'geo')
   } else {
     tidy.df$price <- 0
     tidy.df$rent <- 0
     names(tidy.df)[1:3] <- c('time', 'yield', 'geo')
   }
 
 ## Return Values   
     
  return(tidy.df)  
}
 
 
### Swap yield information from imputations and matching methods -------------------------

apmYieldSwap <- function(trans.data,       # Transaction data
                         match.data        # Set of matched data from apmSaleRentMatch()
)
{
  
  ##  Split into sales and rent sets
  
  transSales <- trans.data[trans.data$transType == 'sale', ]
  transRents <- trans.data[trans.data$transType == 'rent', ]
  
  ## Apply the imputed yields to the match data  
  
  match.data$imp.saleyield <- transSales$imp.saleyield[match(match.data$saleID,
                                                               transSales$UID)]
  match.data$imp.rentyield <- transRents$imp.rentyield[match(match.data$rentID,
                                                                  transRents$UID)]
  match.data$imp.saleyieldX <- transSales$imp.yield[match(match.data$saleID,
                                                          transSales$UID)]
  match.data$imp.rentyieldX <- transRents$imp.yield[match(match.data$rentID,
                                                          transRents$UID)]
  
  ## Resolve situations where properties have more than one matched yield  
  
  matchSales <- tapply2DF(xData=match.data$srm.saleyield, byField=match.data$saleID, 
                          xFunc=median)
  matchRents <- tapply2DF(xData=match.data$srm.rentyield, byField=match.data$rentID, 
                          xFunc=median) 
  
  ## Add matched yields to the transaction data  
  
  transSales$srm.saleyield <- matchSales$Var[match(transSales$UID, matchSales$ID)]
  transSales$srm.rentyield <- NA
  transRents$srm.rentyield <- matchRents$Var[match(transRents$UID, matchRents$ID)]
  transRents$srm.saleyield <- NA
  
  ## Recombine trans.data
  
  return(list(trans.data=rbind(transSales, transRents),
              match.data=match.data))
  
}  

## Compare the differences between the matched observations and all observations ---------

apmCompareSamples <- function(trans.data, match.data)
{  
  
  ## Create a match trans showing only those observations used in the match data  
  
  match.trans <- trans.data[!is.na(trans.data$srm.saleyield) | 
                             !is.na(trans.data$srm.rentyield),]
  
  ## De trend sales prices and rents of all transactions
  
  # Create separate data sets
  c.unit.rent <- subset(trans.data, transType=='rent' & PropertyType == 'Unit')
  c.house.rent <- subset(trans.data, transType=='rent' & PropertyType == 'House')
  c.unit.sale <- subset(trans.data, transType=='sale' & PropertyType == 'Unit')
  c.house.sale <- subset(trans.data, transType=='sale' & PropertyType == 'House')
  
  # Create an index for detrending
  unitrentsTrender <- tapply(c.unit.rent$transValue, c.unit.rent$transQtr, median)
  unitrentsTrender <- data.frame(qtr=1:20,
                                 adj=1/(unitrentsTrender/unitrentsTrender[1]))
  unitsalesTrender <- tapply(c.unit.sale$transValue, c.unit.sale$transQtr, median)
  unitsalesTrender <- data.frame(qtr=1:20,
                                 adj=1/(unitsalesTrender/unitsalesTrender[1]))
  houserentsTrender <- tapply(c.house.rent$transValue, c.house.rent$transQtr, median)
  houserentsTrender <- data.frame(qtr=1:20,
                                  adj=1/(houserentsTrender/houserentsTrender[1]))
  housesalesTrender <- tapply(c.house.sale$transValue, c.house.sale$transQtr, median)
  housesalesTrender <- data.frame(qtr=1:20,
                                  adj=1/(housesalesTrender/housesalesTrender[1]))
  
  # Apply detrending adjustments
  c.unit.rent$xValue <- (c.unit.rent$transValue * 
                           unitrentsTrender$adj[match(c.unit.rent$transQtr,
                                                      unitrentsTrender$qtr)])
  c.house.rent$xValue <- (c.house.rent$transValue * 
                            houserentsTrender$adj[match(c.house.rent$transQtr,
                                                        houserentsTrender$qtr)])
  c.unit.sale$xValue <- (c.unit.sale$transValue * 
                           unitsalesTrender$adj[match(c.unit.sale$transQtr,
                                                      unitsalesTrender$qtr)])
  c.house.sale$xValue <- (c.house.sale$transValue * 
                            housesalesTrender$adj[match(c.house.sale$transQtr,
                                                        housesalesTrender$qtr)])
  
  ## Detrend sales and rents of matched trans
  
  # Subset data
  m.unit.rent <- subset(match.trans, transType=='rent' & PropertyType == 'Unit')
  m.house.rent <- subset(match.trans, transType=='rent' & PropertyType == 'House')
  m.unit.sale <- subset(match.trans, transType=='sale' & PropertyType == 'Unit')
  m.house.sale <- subset(match.trans, transType=='sale' & PropertyType == 'House')
  
  # Detrend values
  m.unit.rent$xValue <- (m.unit.rent$transValue * 
                           unitrentsTrender$adj[match(m.unit.rent$transQtr,
                                                      unitrentsTrender$qtr)])
  m.house.rent$xValue <- (m.house.rent$transValue * 
                            houserentsTrender$adj[match(m.house.rent$transQtr,
                                                        houserentsTrender$qtr)])
  m.unit.sale$xValue <- (m.unit.sale$transValue * 
                           unitsalesTrender$adj[match(m.unit.sale$transQtr,
                                                      unitsalesTrender$qtr)])
  m.house.sale$xValue <- (m.house.sale$transValue * 
                            housesalesTrender$adj[match(m.house.sale$transQtr,
                                                        housesalesTrender$qtr)])
  
  ## Calculate differences in the de-trended values  
  
  ur <- t.test(c.unit.rent$xValue, m.unit.rent$xValue)
  us <- t.test(c.unit.sale$xValue, m.unit.sale$xValue)
  hr <- t.test(c.house.rent$xValue, m.house.rent$xValue)
  hs <- t.test(c.house.sale$xValue, m.house.sale$xValue)
  
  ## Return Values
  return(list(match.trans = match.trans,
              house.sale=list(all=c.house.sale,
                              match=m.house.sale),
              unit.sale=list(all=c.unit.sale,
                             match=m.unit.sale),
              house.rent=list(all=c.house.rent,
                              match=m.house.rent),
              unit.rent=list(all=c.unit.rent,
                             match=m.unit.rent),
              house.sale.test = hs,
              unit.sale.test = us,
              house.rent.test = hr,
              unit.rent.test = ur))
  
}

##########################################################################################
#  Wrapper Function that runs all data analysis                                          #
##########################################################################################

apmFullDataAnalysis <- function(trans.data,
                                data.path,
                                writeout=TRUE,
                                return.values=TRUE){
  
  if(verbose) cat('Create raw price and rent indexes at each level and geography\n')
  
  index.data <- indexLevelWrap(trans.data, wrap.function='indexGeoWrap')
  
  ## Create the impute regression values
  
  if(verbose) cat('Creating Imputed Regression values\n')
  
  hedimp.data <- hedImpFullEstimation(trans.data)
  
  ## Create a set of matched data (adjusted with global time indexes)
  if(verbose) cat('Building Matched Data\n')
  match.data <- srmMatcher(trans.data=trans.data, index.obj=index.data,
                           index.geo='suburb',
                           match.field='AddressID', sale.field='transValue',
                           rent.field='transValue', time.field='transQtr')
  
  ## Add impute data to  and vice versa
  
  if(verbose) cat('Swapping Impute and Matched Yields\n')
  swappedData <- apmYieldSwap(trans.data=hedimp.data,
                              match.data=match.data)
  trans.data <- swappedData$trans.data
  match.data <- swappedData$match.data
  
### Create indices from each of the methods ----------------------------------------------

  ## Via the Index method  
  if(verbose) cat('Index analysis\n')
  index.results <- indexLevelWrap(index.data, wrap.function='indexTYGeoWrap')
  
  ## via the hedonic impute regression
  if(verbose) cat('Hedimp analysis\n')
  hedimp.results <- hedimpYieldWrap(hedimp.data, yield.field='imp.actyield', verbose)  
  
  ## Apply match method
  if(verbose) cat('Match analysis\n')
  match.results <- srmYieldWrap(match.data, verbose)
  
  ### Tidy up the data ---------------------------------------------------------------------
  
  ## Tidy each type  
  if(verbose) cat('Tidying Data\n')
  index.tidy <- indexLevelWrap(index.results, wrap.function='indexTidyerGeoWrap')
  srm.tidy <- apmGenTidyerGeoWrap(match.results, method.type='srm')
  hedimp.tidy <- apmGenTidyerGeoWrap(hedimp.results, method.type='hedimp')

  # Combine
  
  yield.data <- rbind(index.tidy, hedimp.tidy, srm.tidy)  
  
  if(writeout){  
    if(verbose) cat('Writing Data\n')
    save(trans.data, match.data, index.data, yield.data, 
         index.results, hedimp.results, match.results,
         file=paste0(data.path, 'yieldResults.RData'))  
    
    write.csv(trans.data, paste0(data.path, 'imputedYields.csv'))
    write.csv(match.data, paste0(data.path, 'matchedYields.csv'))
  }
  
 ## Return data  
  
  if(return.values){
  return(list(yield.data=yield.data,
              impute.data=trans.data,
              match.data=match.data,
              index.data=index.data,
              results=list(index=index.results,
                           hedimp=hedimp.results,
                           match=match.results)))  
  }
}

### Function to calculate bias from one method to other methods --------------------------

apmCalcBias <- function(geo.level,
                        yield.data,
                        comp.method='Match'){
  
  geo.data <- yield.data[yield.data$geo.level == geo.level, ]
  geo.data$uid <- paste0(geo.data$time, "..", geo.data$geo)
  
  geo.h <- geo.data[geo.data$type == 'Houses', ]
  geo.u <- geo.data[geo.data$type == 'Units', ]
  
  geo.hnm <- geo.h[geo.h$method != comp.method, ]
  geo.hm <- geo.h[geo.h$method == comp.method, ]
  names(geo.hm)[which(names(geo.hm) == 'yield')] <- 'comp.yield'
  
  geo.unm <- geo.u[geo.u$method != comp.method, ]
  geo.um <- geo.u[geo.u$method == comp.method, ]
  names(geo.um)[which(names(geo.um) == 'yield')] <- 'comp.yield'
  
  geo.hh <- merge(geo.hnm, geo.hm[,c('uid', 'comp.yield')])
  geo.hh$Bias <- geo.hh$yield - geo.hh$comp.yield
  
  geo.uu <- merge(geo.unm, geo.um[,c('uid', 'comp.yield')])
  geo.uu$Bias <- geo.uu$yield - geo.uu$comp.yield
  
  return(rbind(geo.hh, geo.uu))
  
} 


### Calculate the differences between methods --------------------------------------------

calcDifEngine <- function(yield.data
                          )
  {
  
  # ARGUMENTS
  #
  # yield.data = yield data
  
 ## Prep data  
  
  # Create unique id
  yield.data$UID <- paste0(yield.data$geo, "..", yield.data$time)
  
  # Split data by method
  x.split <- split(yield.data, as.factor(yield.data$method))
  
  # Merge each pair of methods
  x.ind_mat <- merge(x.split$Index, x.split$Match[,c('UID', 'yield')], by='UID')
  x.ind_imp <- merge(x.split$Index, x.split$Impute[,c('UID', 'yield')], by='UID')
  x.imp_mat <- merge(x.split$Impute, x.split$Match[,c('UID', 'yield')], by='UID')
  
 ## Calculate differences  
  
  x.ind_mat$meth.dif <- x.ind_mat$yield.x - x.ind_mat$yield.y
  x.ind_mat$comp.method <- 'Index - Match'
  
  x.ind_imp$meth.dif <- x.ind_imp$yield.x - x.ind_imp$yield.y
  x.ind_imp$comp.method <- 'Index - Impute'
  
  x.imp_mat$meth.dif <- x.imp_mat$yield.x - x.imp_mat$yield.y
  x.imp_mat$comp.method <- 'Impute - Match'
  
 ## Return Values  
  
  return(rbind(x.ind_mat, x.ind_imp, x.imp_mat))
  
}  

### Wrapper to apply difference calculate across an entire geo.level ---------------------

calcDifWrap <- function(x.data, 
                        h.appr, 
                        u.appr, 
                        geo.level='Global'
                        )
  {
  
  # ARGUMENTS
  #
  # x.data = yield data as a list
  # h.appr = appreciation rates per time period
  # u.appr = unit appreciation rates per time period
  # geo.level = geographic level to analyze
  
 ## Select out data
  
  x.data <- x.data[[which(names(x.data) == geo.level)]]
  
 ## Split into house and unit  
  
  h.data <- x.data[x.data$type == 'house', ]
  u.data <- x.data[x.data$type == 'unit', ]
  
 ## Calculate differences
  
  # Houses
  h.dif <- calcDifEngine(h.data)
  h.dif$geo.level <- geo.level
  h.dif$appr.rate <- h.appr$app.rate[match(h.dif$time, h.appr$time)]
  
  # Units
  u.dif <- calcDifEngine(u.data)
  u.dif$geo.level <- geo.level
  u.dif$appr.rate <- u.appr$app.rate[match(u.dif$time, u.appr$time)]
  
 ## Return Values  
  
  return(list(houses=h.dif,
              units=u.dif))
  
} 

### Calculate differences across all geo levels ------------------------------------------

calcDifGeoWrap <- function(x.data, 
                           h.appr, 
                           u.appr, 
                           geo.levels=c('Global', 'lga', 'suburb')
                           )
  {
  
  # ARGUMENTS
  #
  # x.data = yield data as a list
  # h.appr = appreciation rates per time period
  # u.appr = unit appreciation rates per time period
  # geo.level = geographic level to analyze
  
 ## Apply across all geo.levels
  
  geo.data <- lapply(geo.levels, 
                     FUN=calcDifWrap, h.appr=h.appr, u.appr=u.appr,
                     x.data=x.data)
  
 ## Extract data and combine by unit type  
  
  getHouse <- function(x) x$houses
  getUnit <- function(x) x$units
  geo.houses <- rbind.fill(lapply(geo.data, getHouse))
  geo.units <- rbind.fill(lapply(geo.data, getUnit))

 ## Return Values  
    
  return(list(houses=geo.houses,
              units=geo.units))
  
}  


##########################################################################################
# Function that calculate the appr rates and differences between methods                 #
##########################################################################################

testSampleDif <- function(trans.data, 
                          match.data,
                          h.index=index.data$Global$Global$house.sale,
                          u.index=index.data$Global$Global$unit.sale,
                          geo.field, 
                          geo.value=NULL){
  
  if(geo.field != 'Global'){
    trans.data <- trans.data[trans.data[,geo.field] == geo.value,]
  }
  
  sh.data <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'House', ]
  su.data <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'Unit', ]
  rh.data <- trans.data[trans.data$transType == 'rent' & 
                          trans.data$PropertyType == 'House', ]
  ru.data <- trans.data[trans.data$transType == 'rent' & 
                          trans.data$PropertyType == 'Unit', ]
  
  mrh.data <- match.data[match.data$PropertyType == 'House',]
  mru.data <- match.data[match.data$PropertyType == 'Unit',]
  
  h.beds <- try(t.test(sh.data$Bedrooms, rh.data$Bedrooms), silent=TRUE)
  h.baths <- try(t.test(sh.data$Baths, rh.data$Baths), silent=TRUE)
  h.area <- try(t.test(sh.data$AreaSize, rh.data$AreaSize), silent=TRUE)
  
  u.beds <- try(t.test(su.data$Bedrooms, ru.data$Bedrooms), silent=TRUE)
  u.baths <- try(t.test(su.data$Baths, ru.data$Baths), silent=TRUE)
  
  ## prices
  h.index <- data.frame(time=1:length(h.index),
                        value=h.index)
  
  u.index <- data.frame(time=1:length(u.index),
                        value=u.index)
  
  sh.data$adj <- (h.index$value[match(sh.data$transQtr, h.index$time)]/100)
  su.data$adj <- (u.index$value[match(su.data$transQtr, u.index$time)]/100)
  mrh.data$adj <- (h.index$value[match(mrh.data$saleTime, h.index$time)]/100)
  mru.data$adj <- (u.index$value[match(mru.data$saleTime, u.index$time)]/100)
  
  sh.data$a.value <- sh.data$transValue / sh.data$adj
  su.data$a.value <- su.data$transValue / su.data$adj
  mrh.data$a.value <- mrh.data$saleValue / mrh.data$adj
  mru.data$a.value <- mru.data$saleValue / mru.data$adj
  
  h.price <- try(t.test(sh.data$a.value, mrh.data$a.value), silent=TRUE)
  u.price <- try(t.test(su.data$a.value, mru.data$a.value), silent=TRUE)
  
  return(list(h.price,u.price))
  
}  

testSampleDifGeoWrap <- function(trans.data, match.data, geo.field){
  
  geo.list <- names(table(trans.data[,geo.field]))
  geo.data <- lapply(geo.list, testSampleDif, trans.data=trans.data, match.data=match.data,
                     geo.field=geo.field, h.index=index.data$Global$Global$house.sale,
                     u.index=index.data$Global$Global$unit.sale)
  
  return(geo.data)
  
}