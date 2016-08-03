##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with hedonic imputation method  #
#                                                                                        #
##########################################################################################

##########################################################################################
# Functions for creating price and rent imputed values at each location                        #
##########################################################################################

### Basic engine to run the impute regression on a set of sale and rental data -----------

hedimpEngine <- function(trans.data,             
                         reg.spec, 
                         geo.level='Global',
                         verbose = FALSE        
){
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # reg.spec = regression specification to be used
  # verbose = show progress?
  
  ## Split data
  
  if(verbose) cat('...Separating sales and rentals\n')
  sale.data <- subset(trans.data, transType == 'sale')
  rent.data <- subset(trans.data, transType == 'rent')
  
  ## Estimate models and make new predictions
  
  # Fix equation
  reg.spec <- update(reg.spec, . ~ . -as.factor(postCode))
  
  if(geo.level == 'Global'){
    reg.spec <- update(reg.spec, . ~ . +as.factor(lga))
  }
  
  if(geo.level == 'lga'){
    reg.spec <- update(reg.spec, . ~ . +as.factor(suburb))
  }
  
  ## Limit data to those with all fixed effects
  
  if(geo.level == 'Global'){
   s.fe <- names(table(sale.data$lga))
   r.fe <- names(table(rent.data$lga))
   a.fe <- intersect(s.fe, r.fe)
   if(length(a.fe) == 1) reg.spec <- update(reg.spec, . ~ . -as.factor(lga))
   sale.data <- sale.data[sale.data$lga %in% a.fe,]
   rent.data <- rent.data[rent.data$lga %in% a.fe,]
  }
  
  if(geo.level == 'lga'){
    s.fe <- names(table(sale.data$suburb))
    r.fe <- names(table(rent.data$suburb))
    a.fe <- intersect(s.fe, r.fe)
    if(length(a.fe) == 1) reg.spec <- update(reg.spec, . ~ . -as.factor(suburb))
    sale.data <- sale.data[sale.data$suburb %in% a.fe,]
    rent.data <- rent.data[rent.data$suburb %in% a.fe,]
  }
  
  
  # Esimate models
  if(verbose) cat('......Estimating sale and rent models\n')
  sale.model <- lm(reg.spec, data=sale.data)
  rent.model <- lm(reg.spec, data=rent.data)
  
  # Make predictions of imputed values
  if(verbose) cat('......Imputing values\n')
  hedimp.price <- exp(predict(sale.model, newdata=rent.data))
  hedimp.rent <- exp(predict(rent.model, newdata=sale.data))
  
  ## Building data compatability 
  
  if(verbose) cat('......Stacking observed and imputed values\n')
  
  sale.data$Price <- sale.data$transValue
  sale.data$hedimp.price <- round(exp(sale.model$fitted.values), 0)
  sale.data$Rent <- rep(0, nrow(sale.data))
  sale.data$hedimp.rent <- hedimp.rent
  
  rent.data$Price <- rep(0, nrow(rent.data))
  rent.data$hedimp.price <- hedimp.price
  rent.data$Rent <- rent.data$transValue
  rent.data$hedimp.rent <- round(exp(rent.model$fitted.values), 0)
  
  # Combine data back together
  if(verbose) cat('......Merging data\n')
  all.data <- rbind(sale.data, rent.data)
  
  ## Extract model information
  
  sale.model.info <- list(coef=summary(sale.model)$coefficients,
                          r2=summary(sale.model)$r.squared,
                          sigma=summary(sale.model)$r.squared,
                          resid=summary(sale.model)$residuals,
                          baseValue=median(sale.data$transValue[which(as.factor(
                            sale.data$transQtr) == 1)]))
  
  rent.model.info <- list(coef=summary(rent.model)$coefficients,
                          r2=summary(rent.model)$r.squared,
                          sigma=summary(rent.model)$r.squared,
                          resid=summary(rent.model)$residuals,
                          baseValue=median(rent.data$transValue[which(as.factor(
                            rent.data$transQtr) == 1)]))
  
  ## Return values
  
  return(list(results = all.data[ ,c('UID', 'Price', 'hedimp.price',
                                     'Rent', 'hedimp.rent')],
              sale.model = sale.model.info,
              rent.model = rent.model.info))
  
}

### Apply house and unit impute engine to a given geo ------------------------------------

hedimpModeler <- function(geo.value, 
                          geo.field, 
                          trans.data, 
                          verbose)
  {

  # ARGUMENTS
  #
  # geo.value = specific geo to analyze
  # geo.field = geographic field in which geo.value is found
  # trans.data = transaction data
  # verbose = show progress?
  
 ## Split house and unit data  
  
  if(geo.field == 'Global'){
    
    h.idx <- which(trans.data$PropertyType == 'House')
    u.idx <- which(trans.data$PropertyType == 'Unit')

  } else {
    
    h.idx <- which(trans.data[ ,geo.field] == geo.value & 
                    trans.data$PropertyType == 'House')
    u.idx <- which(trans.data[ ,geo.field] == geo.value & 
                     trans.data$PropertyType == 'Unit')
  }
  
 ## Identify rent and sale  
  
  hr.idx <- which(trans.data$transType[h.idx] == 'rent')
  ur.idx <- which(trans.data$transType[u.idx] == 'rent')
  hs.idx <- which(trans.data$transType[h.idx] == 'sale')
  us.idx <- which(trans.data$transType[u.idx] == 'sale')
  
 ## Run house impute  
  
  if(length(table(trans.data$transQtr[h.idx[hs.idx]])) == 20 &
     length(table(trans.data$transQtr[h.idx[hr.idx]])) == 20){
    house.results <- hedimpEngine(trans.data[h.idx,], 
                                  apmOptions$houseEquation, 
                                  geo.level=geo.field,
                                  verbose = FALSE)
  } else {
    house.results <- NULL
  }
  
 ## Run unit impute  
  
  if(length(table(trans.data$transQtr[u.idx[ur.idx]])) == 20 &
     length(table(trans.data$transQtr[u.idx[us.idx]])) == 20){
    unit.results <- hedimpEngine(trans.data[u.idx,], 
                                 apmOptions$unitEquation, 
                                 geo.level=geo.field,
                                 verbose = FALSE)
  } else{
    unit.results <- NULL
  }
  
 ## Return Values  
  
  return(list(house=house.results,
              unit=unit.results))
  
}

### impute for all geo in a given geo.field ----------------------------------------------

hedImpGeoWrap <- function(geo.field,
                          trans.data,
                          verbose=FALSE)
{
  
  # ARGUMENTS
  #
  # geo.field = geogrphic field for which to calculate all of the indexes
  # trans.data = transaction data
  # time.field = field containing the time period analyzed
  
  ## Get the list of geographies to use
  
  if(verbose) cat('Getting list of geographic areas\n')
  
  if(geo.field != 'Global'){
    geo.list <- levels(as.factor(trans.data[, geo.field]))
  } else {
    geo.list <- 'Global'
  }
  ## Apply geo index method across all
  
  if(verbose) cat('Calculating Indexes across all geographies\n')
  
  ind.list <- lapply(geo.list, FUN=hedimpModeler, trans.data=trans.data,
                     geo.field=geo.field, verbose=verbose)
  
  ## Name list items
  
  names(ind.list) <- geo.list
  
  ## Return values
  
  all.imp <- rbind.fill(lapply(ind.list, hedImpGetResults))
  all.imp$geo.level <- geo.field
  
  return(all.imp)
  
}    

### Wrap the imputation over all geographic levels ---------------------------------------

hedImpLevelWrap <- function(trans.data
                            )
  {
 
  # ARGUMENTS
  #
  # trans.data = transaction data
  
  all.levels <- as.list(apmOptions$geo.levels)
  all.list <- lapply(all.levels, hedImpGeoWrap, trans.data=trans.data)

  ## Return values
  
  return(all.list)
}

##########################################################################################
# Functions for creating yields, price and rent index over all geos                      #
##########################################################################################

### Assign the yields to the trans data --------------------------------------------------

hedimpAssignYields <- function(trans.data,            
                               hedimp.data            
)
{
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # hedimp.data = imputed regression results from hedimpModeler()
  
  # Calculate the ratio
  hedimp.data$imp.yield <- (hedimp.data$hedimp.rent * 52) / hedimp.data$hedimp.price
  
  # Calculate the mixed ratios
  idS <- which(hedimp.data$Rent == 0)
  idR <- which(hedimp.data$Price == 0)
  
  hedimp.data$imp.saleyield <- 0
  hedimp.data$imp.saleyield[idS] <- ((hedimp.data$hedimp.rent[idS] * 52) / 
                                       hedimp.data$Price[idS])
  hedimp.data$imp.rentyield <- 0
  hedimp.data$imp.rentyield[idR] <- ((hedimp.data$Rent[idR] * 52) / 
                                        hedimp.data$hedimp.price[idR])
  
  # Add Ratio to full dataset 
  trans.data$imp.yield <- hedimp.data$imp.yield[match(trans.data$UID, hedimp.data$UID)]
  trans.data$imp.saleyield <- hedimp.data$imp.saleyield[match(trans.data$UID,
                                                               hedimp.data$UID)]
  trans.data$imp.rentyield <- hedimp.data$imp.rentyield[match(trans.data$UID, 
                                                               hedimp.data$UID)]
  trans.data$imp.actyield <- ifelse(trans.data$imp.saleyield == 0, 
                                    trans.data$imp.rentyield, 
                                    trans.data$imp.saleyield)
  
  # Remove those with missing values
  imp.trans <- subset(trans.data, !is.na(imp.yield)) 
  
  ## Return  transactions  
  
  return(imp.trans)
}  

### Analyze the yield data by geo level, etc. --------------------------------------------

hedimpYieldWrap <- function(trans.data, 
                            yield.field=imp.yield,  
                            verbose=FALSE
)
{
  
  # ARGUMENTS
  #
  # trans.data = transaction data with sales and rentals
  # yield.field = field containing the yield value to be analyzed
  
  ## Metro
  
  if(verbose) cat('...Analyze at Global Level\n')
  
  hedimpMetroH <- spaceTimeShard(trans.data[trans.data$PropertyType == 'House', ],
                                 metric=c(paste0('Global.', yield.field)),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  hedimpMetroU <- spaceTimeShard(trans.data[trans.data$PropertyType == 'Unit', ],
                                 metric=c(paste0('Global.', yield.field)),
                             spaceField='all', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  ## LGA
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  hedimpLgaH <- spaceTimeShard(trans.data[trans.data$PropertyType == 'House', ],
                               metric=c(paste0('lga.', yield.field)),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  hedimpLgaU <- spaceTimeShard(trans.data[trans.data$PropertyType == 'Unit', ],
                               metric=c(paste0('lga.', yield.field)),
                           spaceField='lga', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median')) 
  
  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')

  # By Use
  hedimpSuburbH <- spaceTimeShard(trans.data[trans.data$PropertyType == 'House', ],
                                  metric=c(paste0('suburb.', yield.field)),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  hedimpSuburbU <- spaceTimeShard(trans.data[trans.data$PropertyType == 'Unit', ],
                                  metric=c(paste0('suburb.', yield.field)),
                              spaceField='suburb', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  

  ## Combine Results  
  
  hedimpResults <- list(Global=list(house=hedimpMetroH, unit=hedimpMetroU),
                        lga=list(house=hedimpLgaH, unit=hedimpLgaU),
                        suburb=list(house=hedimpSuburbH, unit=hedimpSuburbU))
  
  ## Return Results
  
  return(hedimpResults)  
  
}

### Adds all yield values from various geo levels into one data.frame --------------------

hedImpCompressYields <- function(yield.obj
                                 )
  {
  
 # ARGUMENTS
 #
 # yield.obj = list of imputations from hedImpLevelWrap()
  
 ## Preliminary   
  
  ## Determine length of obj
  
  y.len <- length(yield.obj)
  
  ## Extract first yield df
  
  x.data <- yield.obj[[1]]
  
  ## Find location of unique id
  
  idU <- which(names(x.data) == 'UID')
  
  ## Loop though all geo levels and append columns
  
  for(iy in 2:y.len){
    
    y.data <- yield.obj[[iy]]
    idX <- grep('yield', colnames(y.data))
    x.data <- merge(x.data, y.data[,c(idU, idX)], by='UID', all.x=TRUE)
    
  }
  
 ## Return values
  
  return(x.data)

}

##########################################################################################
#  Small helper functions                                                                 #
##########################################################################################

### Small function to extract imputation results from lists ------------------------------

hedImpGetResults <- function (x){
  y <- x$house$results
  z <- x$unit$results
  a <- rbind(y, z)
  return(a)
}

### Add geo.level prefixs to imputed yields ----------------------------------------------

hedImpAddName <- function(geo.level, 
                          x.data){
  idx <- grep('yield', colnames(x.data))
  names(x.data)[idx] <- paste0(geo.level, '.', names(x.data)[idx])
  return(x.data)
}

##########################################################################################
#  Imputation master function                                                            #
##########################################################################################

hedImpFullEstimation <- function(trans.data,
                                 verbose=FALSE)
  {
  
 ## Calculate imputed prices and rents for all properties at all levels  
  
  hedimp.data <- hedImpLevelWrap(trans.data)
  
 ## Calculate and assign yields to trans data
  
  hedimp.data <- lapply(hedimp.data, hedimpAssignYields, trans.data=trans.data)
  
  # Add correct names
  hedimp.data <- mapply(hedImpAddName, 
                        geo.level=apmOptions$geo.levels, 
                        x.data=hedimp.data, 
                        SIMPLIFY=FALSE)
  
  # Compress into a single data.frame
  hedimp.data <- hedImpCompressYields(hedimp.data)
  
 ## Return Values  
  
  return(hedimp.data)
  
}

