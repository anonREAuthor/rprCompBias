##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with sale rent method           #
#                                                                                        #
##########################################################################################

### Function to compare price and rent on only matched properties ------------------------

srmMatcher <- function(trans.data,           # Transaction data
                       index.obj,            # Index obj from apmAreaIndLevelWrap()
                       index.geo = 'Global',# Level at which to use adj Indexes
                       match.field = 'ID',   # Field containing matching ID
                       sale.field = 'Price', # Field containing sale price
                       rent.field = 'Rent',  # Field containing rent 
                       time.field = 'Year'   # Field containing time breakdown
){
  
  # ARGUMENTS
  #
  # Transaction data
  # Index obj from apmAreaIndLevelWrap()
  # Level at which to use adj Indexes
  # Field containing matching ID
  # Field containing sale price
  # Field containing rent 
  # Field containing time breakdown
  
 ## Deal with a global index.geo
 
  if(index.geo == 'Global') trans.data$Global <- 0
  
 ## Split into sales and rentals
  
  sales <- trans.data[trans.data$transType == 'sale', ]
  rentals <- trans.data[trans.data$transType == 'rent', ]
  
 ## Matching sales to rentals
  
  # Remove NAs in match.field
  xSales <- subset(sales, !is.na(sales[match.field]))
  xRentals <- subset(rentals, !is.na(rentals[match.field]))
  
  # Sort to order
  xSales <- xSales[order(xSales[, match.field]),]
  xRentals <- xRentals[order(xRentals[, match.field]),]
  
  # Extract matching field
  sMatch <- xSales[, match.field]
  rMatch <- xRentals[, match.field]
  
  # Perform cross match identification
  mSales <- xSales[!is.na(match(sMatch, rMatch)), ]
  mRentals <- xRentals[!is.na(match(rMatch, sMatch)), ]
  
  # Make the match
  match.trans <- merge(mSales[, c(match.field, 'PropertyType', 'lga', 'sla1','suburb',
                                  'postCode', 'UID', sale.field, time.field)],
                  mRentals[, c(match.field, 'UID', rent.field, time.field)],
                  by=match.field)
  
  # Rename Match Fields
  names(match.trans) <- c(match.field, 'PropertyType', 'lga', 'sla1', 'suburb', 
                          'postCode', 'saleID', 'saleValue', 'saleTime', 'rentID',
                          'rentValue', 'rentTime')
  
  ## Make time adjustments to matched transactions
  
  match.trans <- srmTimeAdjGeoWrap(trans.data=match.trans,
                                   index.obj=index.obj,
                                   index.geo=index.geo)
  
  # Calc Yields
  match.trans$srm.saleyield <- (match.trans$adjRent * 52) / match.trans$saleValue
  match.trans$srm.rentyield <- (match.trans$rentValue * 52) / match.trans$adjSale
  match.trans$srm.yield <- (match.trans$srm.saleyield + match.trans$srm.rentyield) / 2
  
  # Add information on time between transactions
  match.trans$timeGap <- match.trans$rentTime - match.trans$saleTime
  
  ## Return Values    
  
  return(match.trans)  
  
} 

### Wrap function to apply time adjustment overall the entire choses geo -----------------

srmTimeAdjGeoWrap <- function(trans.data,
                              index.obj,
                              index.geo
){
  
  # ARGUMENTS
  #
  # trans.data = full transaction data
  # index.obj = list containing price / rent indexes
  # index.geo = geography of index values to use
  
  ## List of Geographies  
  
  if(index.geo != 'Global'){
    geo.list <- levels(as.factor(trans.data[, index.geo]))
  } else {
    geo.list <- 'Global'
  }
  
  ## Extract the relevant index objects
  
  idObj <- which(names(index.obj) == index.geo)
  index.obj <- index.obj[[idObj]]
  
  ## Make the matches
  
  match.list <- lapply(geo.list, FUN=srmTimeAdjuster, trans.data=trans.data,
                       index.obj=index.obj, index.geo=index.geo)
  
  
  ## Convert to data frame
  
  match.data <- rbind.fill(match.list)
  
  ## Return values  
  
  return(match.data)  
  
}

### Time adjusts the direct matches based on a given set of indexes ----------------------

srmTimeAdjuster <- function(trans.data,          # Transaction data
                            index.obj,           # Index obj from apmCreateIndexes()
                            index.geo,          # Geography to time adjust at
                            geo.value=NULL      # Specific Geo Value
)
{

  # ARGUMENTS
  #
  # trans.data = full transaction data
  # index.obj = list containing price / rent indexes
  # index.geo = geography of index values to use
  # geo.value = specific geographic area to analyze
  
 ## Isolate data from specific area  
  
  if(!is.null(geo.value)) trans.data <- trans.data[trans.data[ ,index.geo] == geo.value, ]

 ## Extract proper set of indices
  
  idO <- which(names(index.obj) == geo.value)
  index.obj <- index.obj[[idO]]
  
 ## Split by use
  
  houseTrans <- trans.data[trans.data$PropertyType == "House", ]
  unitTrans <- trans.data[trans.data$PropertyType == "Unit", ]
  
  # Make adjustment to houses
  if(nrow(houseTrans) > 0 & index.obj$house.sale[1] != "NA" &
     index.obj$house.rent[1] != 'NA'){
    houseSaleAdj <- (index.obj$house.sale[as.numeric(as.factor(houseTrans$rentTime))] /
                       index.obj$house.sale[as.numeric(as.factor(houseTrans$saleTime))])
    houseTrans$adjSale <- houseTrans$saleValue * houseSaleAdj
    
    houseRentAdj <- (index.obj$house.rent[as.numeric(as.factor(houseTrans$saleTime))] /
                       index.obj$house.rent[as.numeric(as.factor(houseTrans$rentTime))])
    houseTrans$adjRent <- houseTrans$rentValue * houseRentAdj
  } else {
    houseTrans <- NULL
  }
  
  # Make adjustment to units 
  if(nrow(unitTrans) & index.obj$unit.sale[1] != "NA" &
     index.obj$unit.rent[1] != 'NA'){
    unitSaleAdj <- (index.obj$unit.sale[as.numeric(as.factor(unitTrans$rentTime))] /
                      index.obj$unit.sale[as.numeric(as.factor(unitTrans$saleTime))])
    unitTrans$adjSale <- unitTrans$saleValue * unitSaleAdj
    
    unitRentAdj <- (index.obj$unit.rent[as.numeric(as.factor(unitTrans$saleTime))] /
                      index.obj$unit.rent[as.numeric(as.factor(unitTrans$rentTime))])
    unitTrans$adjRent <- unitTrans$rentValue * unitRentAdj
  } else {
    unitTrans <- NULL
  }
  
  ## Merge back together
  
  if(!is.null(houseTrans) && !is.null(unitTrans)){
    match.trans <- rbind(houseTrans, unitTrans)
  }
  if(!is.null(houseTrans) & is.null(unitTrans)){
    match.trans <- houseTrans
  }  
  if(is.null(houseTrans) & !is.null(unitTrans)){
    match.trans <- unitTrans
  }  
  if(is.null(houseTrans) & is.null(unitTrans)){
    match.trans <- NULL
  }  
  
  ## Return Data  
  
  return(match.trans)
  
}

### Function for applying match method to all properties ---------------------------------

srmYieldWrap <- function(match.data,             # Matched data object
                         verbose=FALSE)
{
  
  if(verbose) cat('Calculating yields with match method\n')
  
  ## All Metro  
  
  if(verbose) cat('...Analyze at Global Level\n')
  
  dmMetroH <- spaceTimeShard(match.data[match.data$PropertyType == 'House',],
                             metric=c('srm.yield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  dmMetroU <- spaceTimeShard(match.data[match.data$PropertyType == 'Unit',],
                             metric=c('srm.yield'),
                             spaceField='all', timeField='saleTime',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  ## LGA  
  
  if(verbose) cat('...Analyze at LGA Level\n')
  
  dmLgaH <- spaceTimeShard(match.data[match.data$PropertyType == 'House',],
                           metric=c('srm.yield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  dmLgaU <- spaceTimeShard(match.data[match.data$PropertyType == 'Unit',],
                           metric=c('srm.yield'),
                           spaceField='lga', timeField='saleTime',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))

  ## Suburb
  
  if(verbose) cat('...Analyze at Suburb Level\n')

  dmSuburbH <- spaceTimeShard(match.data[match.data$PropertyType == 'House',],
                              metric=c('srm.yield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  dmSuburbU <- spaceTimeShard(match.data[match.data$PropertyType == 'Unit',],
                              metric=c('srm.yield'),
                              spaceField='suburb', timeField='saleTime',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median')) 
  
  ## Combine Results  
  
  dmResults <- list(Global=list(house=dmMetroH, unit=dmMetroU),
                    lga=list(house=dmLgaH, unit=dmLgaU),
                    suburb=list(house=dmSuburbH, unit=dmSuburbU))
  
  ## Return Results
  
  return(dmResults) 
}  
