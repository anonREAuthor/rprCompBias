##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with Index based method         #
#                                                                                        #
##########################################################################################

##########################################################################################
# Functions for creating price and rent indexes at each geography                        #
##########################################################################################

### Base function for creating time index from a set of reg. coefficients  ---------------

indexEngine <- function(ref.coefs,                     
                        time.field=apmOptions$time.field      
)
{
  
  # ARGUMENTS
  #
  # reg.coefs = full set of coefficients from the regression model
  # time.field = field containing the time breakdown
  
  ## Turn Coefficients into a data.frame
  
  coefs.df <- as.data.frame(ref.coefs) 
  names(coefs.df)[1] <- 'Estimate'
  
  ## Extract relavent coefs and turn into a series  
  
  coefs.time <- c(0, coefs.df$Estimate[grep(time.field, rownames(coefs.df))])
  
  ## Convert to an index, base 100  
  
  coefs.index <- 100 * (1 + (exp(c(0, coefs.time[-1])) - 1))
  
  ## Return Values  
  
  return(coefs.index)
}

### Modeling function to derive coefficients and index from regression model -------------

indexModeler <- function(x.data, 
                         use.type='house',
                         req.nbr=60,
                         srs.model=FALSE,
                         srs.limit=100
                         )
{

 ## Set Equation  
  
  if(use.type=='house'){
    model.eq <- apmOptions$houseEquation
  } else {
    model.eq <- apmOptions$unitEquation
  }
  
  # Remove postcode fixed effects
  model.eq <- update(model.eq, . ~ . -as.factor(postCode))

 ## Test to see if SRS will work  
  
  if(srs.model){
     srs.data <- indexMakeRepSales(x.data, 
                                   time.field=apmOptions$time.field, 
                                   price.field='transValue', 
                                   id.prop='AddressID', 
                                   id.sale='UID')
    
     srs.good <- ifelse(nrow(srs.data) >= srs.limit, TRUE, FALSE)
     
     x.model <- NULL
     x.check <- try(x.index <- indexSrsSeries(srs.data)$raw, silent=TRUE)
     if(class(x.check) != 'try-error'){
       x.raw <- ((x.index / 100) * 
                 median(x.data$transValue[x.data[, apmOptions$time.field] == 1]))
       if(length(x.index) != length(table(x.data[,apmOptions$time.field]))){
         srs.good <- FALSE
       }
       
     } else {
       srs.good <- FALSE
     }
  }  else {
    
    srs.good <- FALSE
  
  }

  ## If no srs
  
  if(!srs.good){
  
  
    if(nrow(x.data) > req.nbr){
  
      x.model <- lm(model.eq, data=x.data)
      x.index <- indexEngine(x.model$coef)
      x.raw <- ((x.index / 100) * 
                  median(x.data$transValue[x.data[, apmOptions$time.field] == 1]))
    } else {
    
      x.index <- 'NA'
      x.raw <- 'NA'
    
    }
  
    if(length(x.index) < apmOptions$time.periods){
      x.index <- 'NA'
      x.raw <- 'NA'
    }
  }
    
  return(list(index=x.index,
              raw=x.raw))
}


### Model wrapper to calculate indexes across house/unit and sale/rent dimensions --------

indexModelWrap <- function(geo.value, 
                           geo.field, 
                           x.data, 
                           time.field=apmOptions$time.field,
                           verbose=FALSE
)
{
  
  # ARGUMENTS
  #
  # geo.value = a particular geographic identifier
  # geo.field = field containing the geo.value
  # trans.data = transaction data
  # time.field = field containing the time period analyzed

  ## Isolate data geographically
  
  if(verbose) cat('Isolating data for: ', geo.value, '\n')
  
  # If global take all data
  if(geo.field == 'Global') {
    geo.data <- x.data
  } else {
    
    # Otherwise limit to that specific geography  
    geo.data <- x.data[x.data[, geo.field] == geo.value, ]
  }
  
  ## Split into house and units
  
  if(verbose) cat('Split into house and units\n')
  
  house.data <- geo.data[geo.data$PropertyType == 'House', ]
  unit.data <- geo.data[geo.data$PropertyType == 'Unit', ]
  
 ## Calculate indexes for Houses   
  
  if(verbose) cat('Calculate house price and rent indexes\n')

  house.sales <- subset(house.data, transType == 'sale')
  house.rents <- subset(house.data, transType == 'rent')

  hs.values <- indexModeler(house.sales, use.type='house', 
                            srs.model=apmOptions$srs.model,
                            srs.limit=apmOptions$srs.limit)
  hr.values <- indexModeler(house.rents, use.type='house', 
                            srs.model=apmOptions$srs.model,
                            srs.limit=apmOptions$srs.limit)
  
 ## Calculate indexes for Units  

  unit.sales <- subset(unit.data, transType == 'sale')
  unit.rents <- subset(unit.data, transType == 'rent')
    
  us.values <- indexModeler(unit.sales, use.type='unit', 
                            srs.model=apmOptions$srs.model,
                            srs.limit=apmOptions$srs.limit)
  ur.values <- indexModeler(unit.rents, use.type='unit', 
                            srs.model=apmOptions$srs.model,
                            srs.limit=apmOptions$srs.limit)
  
  ## Return values
  
  return(list(house.sale=hs.values$index,
              house.rent=hr.values$index,
              unit.sale=us.values$index,
              unit.rent=ur.values$index,
              raw=list(house.sale=hs.values$raw,
                       house.rent=hr.values$raw,
                       unit.sale=us.values$raw,
                       unit.rent=ur.values$raw)))
}


### Geo Level Wrapper to calc h, u, s and r indexs for all areas in a geo level ----------

indexGeoWrap <- function(geo.field,
                         x.data,
                         verbose=FALSE
                         )
{
  
  # ARGUMENTS
  #
  # geo.field = geogrphic field for which to calculate all of the indexes
  # x.data = transaction data
  # time.field = field containing the time period analyzed
  
  ## Get the list of geographies to use
  
  if(verbose) cat('Getting list of geographic areas\n')
  
  if(geo.field != 'Global'){
    geo.list <- levels(as.factor(x.data[, geo.field]))
  } else {
    geo.list <- 'Global'
  }
  ## Apply geo index method across all
  
  if(verbose) cat('Calculating Indexes across all geographies\n')
  
  ind.list <- lapply(geo.list, FUN=indexModelWrap, x.data=x.data,
                     geo.field=geo.field, verbose=verbose)
  
  ## name list items
  
  names(ind.list) <- geo.list
  
  ## return values
  
  return(ind.list)
  
}    

##########################################################################################
# Functions for turning price indexes into yield indexes                                 #
##########################################################################################

### Basic engine for turning yields into an index ----------------------------------------

indexToYield <- function(series.obj, 
                         series.name="Global"
                         ){
  
  ## Fix equations 
  
  raw.series <- series.obj[[which(names(series.obj) == series.name)]]$raw
  
  
  if(raw.series$house.rent[1] != "NA" & raw.series$house.sale[1] != 'NA' &
     length(raw.series$house.rent) == length(raw.series$house.sale)){
    house.series <- (raw.series$house.rent * 52) / raw.series$house.sale
  } else {
    house.series <- "NA"
  }
  
  if(raw.series$unit.rent[1] != "NA" & raw.series$unit.sale[1] != 'NA' &
     length(raw.series$unit.rent) == length(raw.series$unit.sale)){
    unit.series <- (raw.series$unit.rent * 52) / raw.series$unit.sale
  } else {
    unit.series <- "NA"
  }
  
  ## Return values
  
  return(list(house.yields=house.series,
              unit.yields=unit.series,
              house.price=raw.series$house.sale,
              house.rent=raw.series$house.rent,
              unit.price=raw.series$unit.sale,
              unit.rent=raw.series$unit.rent))
}

### Wrapper to apply index to yield across all geos in a field ---------------------------

indexTYGeoWrap <- function(geo.field, 
                           x.data, 
                           verbose=FALSE
                           ){
  
  ## Get the list of geographies to use
  
  geo.data <- x.data[[which(names(x.data) == geo.field)]]
  geo.list <- names(geo.data)
  
  ## Apply geo index method across all
  
  ind.list <- lapply(geo.list, FUN=indexToYield, series.obj=geo.data)
  
  ## name list items
  
  names(ind.list) <- geo.list
  
  ## return values
  
  return(ind.list)
  
}

##########################################################################################
# Functions for tidying up index method data                                             #
##########################################################################################

### Tidying engine that converts to a data.frame -----------------------------------------

indexTidyer <- function(x.data,
                        geo.name='Global'
                        ){
  # ARGUMENTS
  #
  # x.data = index data to be tidyied transaction data
  # geo.name = specific geographic name
  
 ## Set length to test for  
  
  len <- apmOptions$time.periods
  
 ## Limit to the specific geography  
  
  index.results <- x.data[[which(names(x.data) == geo.name)]]
  
 ## Set null values
  
  hu <- NULL
  type <- NULL
  yield <- NULL
  price <- NULL
  rent <- NULL
  
 ## Extract house yield values
  
  if(length(index.results$house.yields) == len){
    hu <- c(hu, 'H') 
    type <- c(type, rep('house', len))
    yield <- c(yield, index.results$house.yields)
    price <- c(price, index.results$house.price)
    rent <- c(rent, index.results$house.rent)
    
  }
  
 ## Extract unit yields  
  
  if(length(index.results$unit.yields) == len){
    hu <- c(hu, 'U')  
    type <- c(type, rep('unit', len))
    yield <- c(yield, index.results$unit.yields)
    price <- c(price, index.results$unit.price)
    rent <- c(rent, index.results$unit.rent)
  }
  
 ## If nothing then give nothign  
  
  if(length(hu) == 0) {
    return('NA')
  } else {
    
    
    
    index.tidy <- data.frame(method=rep('Index', len * length(hu)),
                             geo=rep(geo.name, len * length(hu)),
                             time=rep(1:len, length(hu)),
                             type=type,
                             yield=yield,
                             price=price,
                             rent=rent)
    return(index.tidy)
  }  
}


### index geo wrapper that applies over all geographices in a field ----------------------

indexTidyerGeoWrap <- function(x.data, 
                               geo.level,
                               verbose=FALSE
                               ){
  
  # ARGUMENTS
  #
  # x.data = index data to be tidyied transaction data
  # geo.level = various levels at which to perform the analysis

 ## Set list of geos  
  geo.data <- x.data[[which(names(x.data) == geo.level)]]
  geo.list <- as.list(names(geo.data))
  
 ## Calculate all tidy dfs  
  
  ind.list <- lapply(X=geo.list, FUN=indexTidyer, x.data=geo.data)
  
 ## Find out which ones didn't have comlete data  
  
  ind.class <- which(unlist(lapply(ind.list, class)) == 'data.frame')
  
 ## Conver to a data.frame  
  
  ind.df <- rbind.fill(ind.list[ind.class])
  ind.df$geo.level <- geo.level
  
 ## Return Values  
  
  return(ind.df)
}

##########################################################################################
# Wrappers for apply index functions across dimensions                                   #
##########################################################################################

### Area Level Wrapper for various functions across all different geographic levels ------

indexLevelWrap <- function(x.data, 
                           geo.levels=apmOptions$geo.levels,
                           wrap.function='indexGeoWrap',
                           verbose=FALSE
)
{
  
  # ARGUMENTS
  #
  # trans.data = transaction data
  # geo.levels = various levels at which to perform the analysis
  # wrap.function = function to lapply over all geo levels
  
  ## Prep data and objects
  
  results.list <- lapply(X=as.list(geo.levels), FUN=get(wrap.function), x.data=x.data, 
                         verbose=verbose)
  
  ## Fix names and return values  
  
  names(results.list) <- geo.levels
  
  # convert to data.frame if necessary
  if(wrap.function == 'indexTidyerGeoWrap') results.list <- rbind.fill(results.list)
  
  return(results.list)
  
}  

##########################################################################################
# Function for calculating sale-resale indices                                           #
##########################################################################################

# Make the set of matches SRS sales/rentals ----------------------------------------------

indexMakeRepSales <- function(trans.obj,
                              time.field,
                              price.field,
                              id.prop,
                              id.sale
                              )
{
  
  # ARGUMENTS
  #
  # trans.obj = cleaned trans obj from zpiCTClean()
  # price.field = field where the price variable is located
  # time.field = field where the time variable is located
  # id.prop = field where the unique property id is located
  # id.sale = field where the unique sale id is located
  
  ## Set up data  
  
  temp.data <- trans.obj[, c(id.sale, id.prop, time.field, price.field)]
  names(temp.data) <- c('saleID', 'propID', 'time', 'price')
  
  # Re order
  temp.data <- temp.data[order(temp.data$propID, temp.data$time, 
                               temp.data$price),]
  
  # set up two data sets to compare
  temp.1 <- temp.data[-nrow(temp.data), ]
  temp.2 <- temp.data[-1, ]
  
  ## Find matches
  
  srs.id <- temp.1$propID == temp.2$propID & temp.1$time < temp.2$time
  
  # Time to srs
  temp.1 <- temp.1[srs.id, ]
  temp.2 <- temp.2[srs.id, ]
  
  ## Build a data frim of repeat sales
  
  srs.data <- data.frame(propID=temp.1$propID,
                         sale1=temp.1$saleID,
                         sale2=temp.2$saleID,
                         time1=temp.1$time,
                         time2=temp.2$time,
                         price1=temp.1$price,
                         price2=temp.2$price
  )
  
  #   # Add location data
  #   names(srs.data)[1] <- id.prop
  #   srs.data <- merge(srs.data, loc.obj, by=id.prop)
  #   
  ## Return data
  
  return(srs.data)
}

### Calculate the index with SRS model ---------------------------------------------------

indexSrsSeries <- function(srs.obj,
                           base.merge=1,
                           smooth.par=.3,
                           semilog=TRUE,
                           weighted=TRUE,
                           het.corr=TRUE,
                           save.model=FALSE
)
{
  # ARGUMENTS
  #
  # srs.obj = merged hed data object
  # base.merge = hedonic specification
  # smooth = smooth the series with LOWESS?
  # smooth.par = lowess smooth parameter
  # semilog = is this is a semilog regression (log(price) as dependent)
  # weighted = weight the observations by time between them (less = higher weight)
  # het.corr = apply a heteroskedasticity correction
  # save.model = should complete regression model be saved?
  
  ## Set a few parameters
  
  time.start <- min(srs.obj$time1)
  time.nbr <- max(srs.obj$time2)
  
  ## Calculate price difference
  
  if(!semilog){
    srs.obj$price.dif <- srs.obj$price2 - srs.obj$price1
  } else {
    srs.obj$price.dif <- log(srs.obj$price2) - log(srs.obj$price1)
  }
  
  ## Set up time dummy matrix
  
  time.matrix <- array(0, dim = c(nrow(srs.obj), 
                                  time.nbr - time.start - base.merge + 1))
  
  # Fill in time matrix
  for (tm in seq(time.start + base.merge, time.nbr)) {
    time.matrix[srs.obj$time1==tm, tm - base.merge] <- -1
    time.matrix[srs.obj$time2==tm, tm - base.merge] <- 1
  }
  
  # Name Time matrix
  colnames(time.matrix) <- paste0("time.", seq(time.start + base.merge, time.nbr))
  
  ## Run the regression
  
  reg.model <- lm(srs.obj$price.dif ~ time.matrix + 0)
  
  # If het corr and weighted
  
  if(!weighted){
    if(het.corr){
      err <- residuals(reg.model)
      wgts <- sqrt(1 / abs(err))
      reg.model <- lm(srs.obj$price.dif ~ time.matrix + 0, weights=wgts)
    } 
  } else {
    srs.obj$time.dif <- srs.obj$time2-srs.obj$time1
    if(het.corr){
      err <- residuals(reg.model)
      err.fit <- lm((err^2) ~ srs.obj$time.dif)
      wgts <- sqrt(fitted(err.fit))
      reg.model <- lm(srs.obj$price.dif ~ time.matrix + 0, weights=wgts)
    } else {
      wgts <- sqrt(sqrt(1 / srs.obj$time.dif))
      reg.model <- lm(srs.obj$price.dif ~ time.matrix + 0, weights=wgts)
    }
  }
  
  ## Make the index
  
  # Extract coefficients
  coefs <- as.data.frame(summary(reg.model)$coefficients)
  
  # Conver to a series 
  if(semilog){
    temp.series <- 1 + c(0, exp(coefs$Estimate) - 1)
  } else {
    base.value <- mean(srs.obj$price1[srs.obj$time1 %in% seq(1, base.merge)])
    temp.series <- c(base.value, coefs$Estimate + base.value)
    temp.series <- temp.series / temp.series[1]
  }
  
  ## inflate to 100 andy/or smooth  
  
  raw.series <- 100 * temp.series
  smooth.series <- lowess(100 * temp.series, f=smooth.par)$y
  
  ## Confints  
  
  lo.ci.raw <- c(array(0, dim = base.merge), confint(reg.model, level = .95)[, 1])
  hi.ci.raw <- c(array(0, dim = base.merge), confint(reg.model, level = .95)[, 2])
  lo.ci.smooth <- lowess(lo.ci.raw, f=smooth.par)$y
  hi.ci.smooth <- lowess(hi.ci.raw, f=smooth.par)$y
  
  ## Return 
  
  return(list(raw = raw.series,
              smooth = smooth.series,
              model = ifelse(save.model, reg.model, 'not saved'),
              confint = list(low = list(raw=lo.ci.raw,
                                        smooth=lo.ci.smooth),
                             high = list(raw=hi.ci.raw,
                                         smooth=hi.ci.smooth))
  ))
}

