##########################################################################################
#                                                                                        #
#  Data Preparation functions for APM data                                               #
#                                                                                        #
##########################################################################################

### Master function to build all data ----------------------------------------------------

apmFullDataBuild <- function(dataPath,               # Location of raw data
                             saleFile,               # File name/loc of sales
                             rentFile,               # File name/loc of rents
                             geoFiles=list(),        # List of shapefiles
                             offline=FALSE,          # Are you offline??
                             verbose=FALSE,          # Show progress?
                             optionChanges=NULL      # Future capability to change options
                             )
  {
 
### Example Function Call --------------------------------------------------------------
  
  if(F){
    
  apmFullDataBuild(dataPath="C:/data/research/priceRentMethComp/",
                   saleFile='transData/sales10_15.csv',
                   rentFile='transData/rents10_15.csv',
                   geoFiles=list(suburb='shapefiles/Vic_Suburbs.shp',
                                 lga='shapefiles/Vic_LGAs.shp',
                                 sla1='shapefiles/Vic_SLA1.shp',
                                 postcode='shapefiles/Vic_PostCodes.shp',
                                 ssFile='spatialData/allSS.csv'),
                   offline=TRUE,
                   verbose=TRUE,
                   optionChanges=NULL)
  }   
  
  # Load Raw data
  apmLoadRawData(dataPath=dataPath, saleFile=salePath, rentFile=rentPath,
                 geoFiles=geoPath, offline=offline, verbose=verbose)
  
  # Build Data
  
  allTrans <- apmBuildData(rawRents, rawSales, ssData, geoShapes,
                           verbose=verbose)
  
  # Clean Data
  
  allTrans <- apmCleanData(apmDataObj=allTrans, verbose=verbose)
  
  # Apply geographic/time filters
  
  cleanData <- apmSetGeoThres(allTrans, geoShapes, verbose=verbose) 
  
  # Isolate clean transaction data
  cleanTrans <- cleanData$apmDataObj
  
  # Isolate study area shapes
  studyShapes <- cleanData$studyShapes

 ## Write out data objects

  if(verbose) cat('Writing out data objects')
  save(cleanTrans, file=paste0(exportPath, 'cleanTrans.RData'))
  save(studyShapes, file=paste0(exportPath, 'studyShps.RData')) 
  
 ## Clean up environment
  
  rm(rawRents, envir=.GlobalEnv)
  rm(rawSales, envir=.GlobalEnv)
  rm(ssData, envir=.GlobalEnv)
  gc()
  
}

### Function to load necessary APM data ----------------------------------------
  
apmLoadRawData <- function(dataPath,               # Location of raw data
                           saleFile,               # File name/loc of sales
                           rentFile,               # File name/loc of rents
                           geoFiles=list(),        # List of shapefiles
                           offline=FALSE,          # Are you offline??
                           verbose=FALSE           # Show progress?
                           )
{
  
  ### Example Function Call --------------------------------------------------------------
  
  if(F){
    
    apmLoadRawData(dataPath="C:/data/research/priceRentMethComp/",
                   saleFile='transData/sales10_15.csv',
                   rentFile='transData/rents10_15.csv',
                   geoFiles=list(suburb='shapefiles/Vic_Suburbs.shp',
                                 lga='shapefiles/Vic_LGAs.shp',
                                 sla1='shapefiles/Vic_SLA1.shp',
                                 postcode='shapefiles/Vic_PostCodes.shp',
                                 ssFile='spatialData/allSS.csv'),
                   offline=TRUE,
                   verbose=TRUE)
     }

  ### Read in raw data -------------------------------------------------------------------
  
  if(verbose) cat('Loading Data\n')
  
  ## Read in Data  
  
  # Sales data
  if(verbose) cat('...Sales Data\n')
  assign('rawSales', read.csv(paste0(dataPath, saleFile), 
                              stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  if(verbose) cat('...Rental Data\n')
  assign('rawRents', read.csv(paste0(dataPath, rentFile), 
                              stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  
  if(verbose) cat('...SS Data\n')
  assign('ssData', read.csv(paste0(dataPath, geoFiles$ssFile), 
                     stringsAsFactors = FALSE),
         envir=.GlobalEnv)
  
  if(verbose) cat('...Geographic Shapefiles\n')
  subShp <- readShapePoly(paste0(dataPath, geoFiles$suburb))
  lgaShp <- readShapePoly(paste0(dataPath, geoFiles$lga))
  sla1Shp <- readShapePoly(paste0(dataPath, geoFiles$sla1))
  postCodeShp <-  readShapePoly(paste0(dataPath, geoFiles$postcode))
  assign('geoShapes', list(suburb=subShp,
                           lga=lgaShp,
                           sla1=sla1Shp,
                           postcode=postCodeShp), envir=.GlobalEnv)
  
}

### Function to convert various APM date structures into R date structure ----------------

apmFixDates <- function(xDates      # Vector of dates to be fixed
                        )
{
  
  ## Set required libraries
  
  require(stringr)
  
  ## Fix missingness
  
  xDates[xDates == ""] <- '01/01/2001'
  
  ## Break down dates
  
  # Remove Time
  xDates <- gsub(" 0:00", "", xDates)
  xDates <- gsub(" 12:00:00 AM", "", xDates)
  
  
  # Find location of slashes
  sLoc <- matrix(unlist(str_locate_all(xDates, '/')), ncol=4, byrow=TRUE)[,1:2]
  
  # Correct Days
  days <- as.numeric(substr(xDates, 1, sLoc[ ,1] - 1))
  days <- ifelse(days < 10, paste0('0', days), as.character(days))
  
  # Correct Months
  months <- as.numeric(substr(xDates, sLoc[ ,1] + 1, sLoc[ ,2] - 1))
  months <- ifelse(months < 10, paste0('0', months), as.character(months))
  
  # Correct years
  years <- as.numeric(substr(xDates, sLoc[ ,2] + 1, 50))
  years <- ifelse(years < 2000, paste0('20', years), as.character(years))
  
  ## Recombine into R date format  
  
  newDates <- as.Date(paste0(days, '/' , months, '/', years), "%d/%m/%Y")
  
  ## Return Values  
  
  return(newDates)
}


### Function to integrate data  ----------------------------------------------------------  

apmBuildData <- function(rawRents,              # Raw rental data
                         rawSales,              # Raw sale data
                         ssData,                # space syntax data
                         geoShapes,             # geographic shape files
                         verbose=FALSE          # Show Progress?
                         )
{
 
 ## Example function call
  
  if(F){
    allTrans <- buildAPMData(rawRents, rawSales, ssData, geoShapes,
                             verbose=TRUE)
  }
  
  if(verbose) cat('Building Data\n')
  
    
 ## Create conforming fields regarding transaction times and values

  rawSales <- rawSales[!is.na(rawSales$AddressID),]
  rawRents <- rawRents[!is.na(rawRents$AddressID),]
  
  # Fix date formats
  if(verbose) cat('...Fix Date Values\n')
  rawSales$transDate <- apmFixDates(rawSales$FinalResultEventDate)
  rawRents$transDate <- apmFixDates(rawRents$EventDate)

  # Build new column for transaction Value
  if(verbose) cat('...Fix Transaction Values\n')
  rawSales$transValue <- as.numeric(rawSales$FinalResultEventPrice)
  rawRents$transValue <- as.numeric(rawRents$EventPrice)

  # Set transaction Type   
  rawSales$transType <- 'sale'  
  rawRents$transType <- 'rent'

 ## Remove duplicates
  
  rawSales <- rawSales[order(rawSales$EventDate, decreasing=TRUE),]
  rawRents <- rawRents[order(rawRents$EventDate, decreasing=TRUE),]
  
  rawSales$dupid <- paste0(rawSales$AddressID, "..", as.numeric(as.factor(rawSales$LastAdvertisedEventDate)))
  rawRents$dupid <- paste0(rawRents$AddressID, "..", as.numeric(as.factor(rawRents$LastAdvertisedEventDate)))
  
  rawSales <- rawSales[!duplicated(rawSales$dupid),]
  rawRents <- rawRents[!duplicated(rawRents$dupid),]
  
  
  
  ## Create UniqueID
  
  if(verbose) cat('...Assign Unique Ids\n')
  rawSales <- rawSales[order(rawSales$FinalResultEventDate), ]
  rawRents <- rawRents[order(rawRents$FinalResultEventDate), ]
  
  rawSales$UID <- paste0('sale', 1:nrow(rawSales))
  rawRents$UID <- paste0('rental', 1:nrow(rawRents))
  
  
  
 ## Fix missing lat long

  if(verbose) cat('...Fix Missing Lat/Long Values\n')

  # ID missing
  sXY <- which(is.na(rawSales$Property_Latitude) | 
               is.na(rawSales$Property_Longitude))
  rXY <- which(is.na(rawRents$Property_Latitude) | 
               is.na(rawRents$Property_Longitude))

  # Fix, if possible 
  rawSales$Property_Latitude[sXY] <- rawSales$Street_Centroid_Latitude[sXY]
  rawSales$Property_Longitude[sXY] <- rawSales$Street_Centroid_Longitude[sXY]
  rawRents$Property_Latitude[rXY] <- rawRents$Street_Centroid_Latitude[rXY]
  rawRents$Property_Longitude[rXY] <- rawRents$Street_Centroid_Longitude[rXY]

 ## Limit both datasets to a standard field list  

  if(verbose) cat('...Trimming Fields\n')

  # Combine and clip fields to make cleaning easier
  allTrans <- rbind(rawSales[ ,apmOptions$rawColumnList], 
                    rawRents[ ,apmOptions$rawColumnList])

 ## Add additional time information

  if(verbose) cat('...Adding Additional Time Data\n')

  # Add a yearly variable
  allTrans$transYear <- as.numeric(substr(allTrans$transDate, 1, 4))

  # Add a month
  allTrans$transMonth <- (((12 * (allTrans$transYear - apmOptions$startYear)) + 
                             as.numeric(substr(allTrans$transDate, 6, 7))) - 
                              (apmOptions$startMonth - 1))

  # Add a days count
  allTrans$transDays <- 1+ (as.numeric(allTrans$transDate - 
                                       as.Date(paste0(apmOptions$startYear, '-', 
                                                      apmOptions$startMonth, '-01'))))

  # Add a quarter count
  allTrans$transQtr <- ((allTrans$transMonth - 1) %/% 3) + 1

 ## Fix NA Fields

  if(verbose) cat('...Fixing NA Fields\n')
  naFields <- apmOptions$naFields
  for(naF in 1:length(naFields)){
    naX <- which(is.na(allTrans[ ,naFields[[naF]]]))
    allTrans[naX, naFields[[naF]]] <- 0
    nFalse <- which(allTrans[ ,naFields[[naF]]] == "")
    allTrans[nFalse, naFields[[naF]]] <- 0
    nFalse <- which(allTrans[ ,naFields[[naF]]] == 'False')
    allTrans[nFalse, naFields[[naF]]] <- 0
    nTrue <- which(allTrans[ ,naFields[[naF]]] == 'True')
    allTrans[nTrue, naFields[[naF]]] <- 1
    allTrans[, naFields[[naF]]] <- as.numeric(  allTrans[, naFields[[naF]]])
    naX <- which(is.na(allTrans[ ,naFields[[naF]]]))
    allTrans[naX, naFields[[naF]]] <- 0
  }

  ##  Check for and remove duplicates

  if(verbose) cat('...Check and Remove Duplicates\n')

  # Create a unique ID
  allTrans$dUID <- paste0(allTrans$AddressID,"..", allTrans$transDate, "..", 
                        allTrans$transType)

  # Keep only those not duplicated
  allTrans <- subset(allTrans, !duplicated(dUID))

  # Remove the UID field
  allTrans$dUID <- NULL

  ## Add Spatial Information

  if(verbose) cat('Adding Spatial Information\n')

  # Remove Missing lat/long
  allTrans <- subset(allTrans, !is.na(Property_Latitude) & 
                       !is.na(Property_Longitude))

  # Create a spatial points data frame
  allSP <- SpatialPointsDataFrame(coords=cbind(allTrans$Property_Longitude,
                                               allTrans$Property_Latitude),
                                  data=allTrans)

  # Add PostCodes
  if(verbose) cat('...Adding Postcodes\n')
  spJoin <- over(allSP, geoShapes$postcode)
  allSP@data$postCode <- as.character(spJoin$POA_2006)

  # Add Suburbs
  if(verbose) cat('...Adding Suburbs\n')
  spJoin <- over(allSP, geoShapes$suburb)
  allSP@data$suburb <- as.character(spJoin$NAME_2006)

  # correct error in names
  allSP@data$suburb <- gsub(' - Bal', '', allSP@data$suburb)

  # Add SLA1
  if(verbose) cat('...Adding Sla1s\n')
  spJoin <- over(allSP, geoShapes$sla1)
  allSP@data$sla1 <- as.character(spJoin$SLA_NAME11)

  # Add LGA
  if(verbose) cat('...Adding LGAs\n')
  spJoin <- over(allSP, geoShapes$lga)
  allSP@data$lga <- as.character(spJoin$LGA_NAME11)

  ## Convert back to regular data.frame

  allTrans <- allSP@data

  ## Add SS Measures

  if(verbose) cat('...Adding SS data\n')

  # Build Single SS File
  ssTrim <- ssData[, c('AddressID', apmOptions$ssFields)]
  names(ssTrim)[2:3] <- c('ssChoice', 'ssInteg')

  # Add Measure to trans
  allTrans$ssChoice <- ssTrim$ssChoice[match(allTrans$AddressID,
                                             ssTrim$AddressID)]
  allTrans$ssInteg <- ssTrim$ssInteg[match(allTrans$AddressID, 
                                           ssTrim$AddressID)]

 ## Return values  
  
  return(allTrans)
}

### Function to clean data  --------------------------------------------------------------  

apmCleanData <- function(apmDataObj,            # transaction data from buildAPMData()
                         verbose=FALSE){


 ## Example function call
  
  if(F){
    allTrans <- apmCleanData(apmDataObj=allTrans, verbose=TRUE)
  }
  
  if(verbose) cat('Cleaning Data\n')

 ## Remove by year
  
  apmDataObj <- apmDataObj[apmDataObj$transYear >= apmOptions$startYear,]
  apmDataObj <- apmDataObj[apmDataObj$transYear <= apmOptions$endYear,]
  
 ## Remove all non house and units

  # Create separate labels
  if(verbose) cat('...Creating Property Type Labels\n')
  apmDataObj$Terrace <- ifelse(apmDataObj$PropertyType == 'Terrace', 1, 0)
  apmDataObj$Townhouse <- ifelse(apmDataObj$PropertyType == 'Townhouse', 1, 0)
  apmDataObj$Studio <- ifelse(apmDataObj$PropertyType == 'Studio', 1, 0)
  apmDataObj$Duplex <- ifelse(apmDataObj$PropertyType == 'Duplex', 1, 0)
  apmDataObj$Villa <- ifelse(apmDataObj$PropertyType == 'Villa', 1, 0)

  # Convert to House or Unit
  apmDataObj$PropertyType[apmDataObj$PropertyTypes %in% 
                          apmOptions$houseTypes] <- 'House'
  apmDataObj$PropertyType[apmDataObj$PropertyTypes %in% 
                          apmOptions$unitTypes] <- 'Unit'
  
  # Limit to houses or units
  if(verbose) cat('...Limiting to Houses and Units\n')
  apmDataObj <- subset(apmDataObj, PropertyType == 'House' | PropertyType == 'Unit')

  ## Removing missing values  

  if(verbose) cat('...Removing Observations with missing data\n')

  for(ij in 1:length(apmOptions$reqFields)){
    idX <- which(names(apmDataObj) == apmOptions$reqFields[[ij]])
    apmDataObj <- subset(apmDataObj, !is.na(apmDataObj[ ,idX]))
  }

 ## Remove suspect values

  if(verbose) cat('...Removing Observation with suspect data values\n')

  # Set limits
  areaLimits <- c(40, 25000)
  bathLimits <- c(1, 8)
  bedLimits <- c(0, 1, 8)  # apt limit, then home limit, then all limit
  rentLimits <- c(125, 2500)
  saleLimits <- c(150000, 4000000)

  # Remove by characteristic
  apmDataObj <- subset(apmDataObj, AreaSize >= apmOptions$areaLimits$min & 
                       AreaSize <= apmOptions$areaLimits$max)
  apmDataObj <- subset(apmDataObj, Baths >= apmOptions$bathLimits$min & 
                       Baths <= apmOptions$bathLimits$max)

  apmDataObjU <- subset(apmDataObj, PropertyType == 'Unit' & 
                         Bedrooms >= apmOptions$bedLimits$unitMin &
                          Bedrooms <= apmOptions$bedLimits$unitMax)
  apmDataObjH <- subset(apmDataObj, PropertyType == 'House' & 
                         Bedrooms >= apmOptions$bedLimits$houseMin &
                          Bedrooms <= apmOptions$bedLimits$houseMax)
  apmDataObj <- rbind(apmDataObjH, apmDataObjU)

  # Split back out
  xSales <- subset(apmDataObj, transType == 'sale')
  xRentals <- subset(apmDataObj, transType == 'rent')

  # Remove by suspect trans value
  xSales <- subset(xSales, transValue >= apmOptions$saleLimits$min & 
                     transValue <= apmOptions$saleLimits$max)
  xRentals <- subset(xRentals, transValue >= apmOptions$rentLimits$min & 
                       transValue <= apmOptions$rentLimits$max)

 ## Return combined object
  
  return(rbind(xSales, xRentals))
 
}

### Function that sets the limits for # of obs by geo by time ---------------------------- 

apmSetGeoThres <- function(apmDataObj,           # transaction data from cleanAPMData()
                           geoShapes,            # geographic shape files
                           verbose=FALSE         # Show progress
                           )
{
  
 ## Example function call
  
  if(F){
    allTrans <- apmSetGeoThres(allTrans, geoShapes, verbose=TRUE) 
  }
  


 ## Determine which geographies meet which time thresholds
  
  if(verbose) cat('...Adding Sample Size Threshold Designators\n')
  
  # Yearly threshold
  yearThres <- mapply(apmGeoLimit, 
                      locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                      MoreArgs=list(timeField='transYear',
                                    transData=apmDataObj,
                                    geoTempLimit=apmOptions$geoTempLimit))
  
  names(yearThres) <- paste0(rep("YT_"),
                             rep(c('both', 'house', 'unit', 'either'), 4),
                             rep("_", 16),
                             c(rep('postCode',4), rep('sla1',4),
                               rep('suburb',4), rep('lga', 4)))
  
  # Quarterly threshold
  qtrThres <- mapply(apmGeoLimit, 
                     locField=c('postCode', 'sla1', 'suburb', 'lga'), 
                     MoreArgs=list(timeField='transQtr',
                                   transData=apmDataObj,
                                   geoTempLimit=apmOptions$geoTempLimit))
  
  names(qtrThres) <- paste0(rep("QT_"),
                            rep(c('both', 'house', 'unit', 'either'), 4),
                            rep("_", 16),
                            c(rep('postCode',4), rep('sla1',4),
                              rep('suburb',4), rep('lga', 4)))
  
 ## Add designators to transactions
  
  # Yearly thresholds
  apmDataObj <- apmApplyThres(yearThres[1:4], apmDataObj, 'YT', 'postCode')
  apmDataObj <- apmApplyThres(yearThres[5:8], apmDataObj, 'YT', 'sla1')
  apmDataObj <- apmApplyThres(yearThres[9:12], apmDataObj, 'YT', 'suburb')
  apmDataObj <- apmApplyThres(yearThres[13:16], apmDataObj, 'YT', 'lga')
  
  # Quarterly thresholds
  apmDataObj <- apmApplyThres(qtrThres[1:4], apmDataObj, 'QT', 'postCode')
  apmDataObj <- apmApplyThres(qtrThres[5:8], apmDataObj, 'QT', 'sla1')
  apmDataObj <- apmApplyThres(qtrThres[9:12], apmDataObj, 'QT', 'suburb')
  apmDataObj <- apmApplyThres(qtrThres[13:16], apmDataObj, 'QT', 'lga')

  
  # Remove observations form areas without enough sales
  
  if(verbose) cat('...Limiting Observations to areas with enough transactions\n')
  
  houseTrans <- subset(apmDataObj, PropertyType == 'House')
  houseTrans <- houseTrans[houseTrans[ ,apmOptions$geoTempFieldHouse] == 1,]
  unitTrans <- subset(apmDataObj, PropertyType == 'Unit')
  unitTrans <- unitTrans[unitTrans[ ,apmOptions$geoTempFieldUnit] == 1,]
  
  # Merge back together
  apmDataObj <- rbind(houseTrans, unitTrans)
  
  # Remove the unneeded fields
  xFields <- c(grep('YT_', names(apmDataObj)), grep('QT_', names(apmDataObj))) 
  apmDataObj <- apmDataObj[ ,-xFields]
  
  ## Limit shapefiles
  
  if(verbose) cat('Limiting Geography Files to Extent of Data\n')
  
  # Suburbs
  studySuburbs <- geoShapes$suburb[(which(geoShapes$suburb@data$NAME_2006 %in% 
                                            names(table(apmDataObj$suburb)))), ]
  
  # Post Codes
  studyPostCodes <- geoShapes$postcode[(which(geoShapes$postcode@data$POA_2006 %in% 
                                                names(table(apmDataObj$postCode)))), ]
  
  # SLA1
  studySLA1s <- geoShapes$sla1[(which(geoShapes$sla1@data$SLA_NAME11 %in% 
                                        names(table(apmDataObj$sla1)))), ]
  
  # LGAs
  studyLGAs <- geoShapes$lga[(which(geoShapes$lga@data$LGA_NAME11 %in% 
                                      names(table(apmDataObj$lga)))), ]
  
  # Combine into a single list
  studyShapes <- list(suburb=studySuburbs,
                      postcode=studyPostCodes,
                      lga=studyLGAs,
                      sla1=studySLA1s)  
    
 ## Return values
  
  return(list(apmDataObj=apmDataObj,
              studyShapes=studyShapes))
}

### Function to determine which geo areas meet use and time criteria ---------------------

apmGeoLimit <- function(transData,               # Dataframe of trans data
                        locField = 'locName',    # Field containing location
                        timeField = 'transYear', # Field containing time
                        geoTempLimit = 3         # Min trans per use/time/loc
){  
  
  # Split transactions by use
  houseSales <- subset(transData, PropertyType == 'House' &
                         transType == 'sale')
  unitSales <- subset(transData, PropertyType == 'Unit' & 
                        transType == 'sale')
  houseRentals <- subset(transData, PropertyType == 'House' & 
                           transType == 'rent')
  unitRentals <- subset(transData, PropertyType == 'Unit' & 
                          transType == 'rent')
  
  # Determine which suburbs meet criteria for each
  saleHTable <- table(houseSales[,locField], houseSales[,timeField])
  shKeep <- which(apply(saleHTable, 1, min) >= geoTempLimit)
  shGeo <- rownames(saleHTable[shKeep, ])
  saleUTable <- table(unitSales[,locField], unitSales[,timeField])
  suKeep <- which(apply(saleUTable, 1, min) >= geoTempLimit)
  suGeo <- rownames(saleUTable[suKeep, ])
  rentHTable <- table(houseRentals[,locField], houseRentals[,timeField])
  rhKeep <- which(apply(rentHTable, 1, min) >= geoTempLimit)
  rhGeo <- rownames(rentHTable[rhKeep, ])
  rentUTable <- table(unitRentals[,locField], unitRentals[,timeField])
  ruKeep <- which(apply(rentUTable, 1, min) >= geoTempLimit)
  ruGeo <- rownames(rentUTable[ruKeep, ])
  bothGeo <- intersect(intersect(intersect(shGeo, suGeo), rhGeo), ruGeo)
  houseGeo <- intersect(shGeo,rhGeo)
  unitGeo <- intersect(suGeo, ruGeo)
  eitherGeo <- union(houseGeo, unitGeo)
  
  # Create tables
  return(list(bothGeo = bothGeo,
              houseGeo = houseGeo,
              unitGeo = unitGeo,
              eitherGeo = eitherGeo))  
}

### Apply the threshold designations across all transactions -----------------------------

apmApplyThres <- function(thresData,       # Threshold data object from prrGeoLimit
                          transData,       # Set of transaction data
                          timePrefix='YT', # Which time was used YT or QT
                          geo="postCode"   # Which geo to use (one at a time)
){
  
  # Pull out single designations
  both <- ifelse(transData[,geo] %in% thresData[[1]],1,0)
  house <- ifelse(transData[,geo] %in% thresData[[2]],1,0)
  unit <- ifelse(transData[,geo] %in% thresData[[3]],1,0)
  either <- ifelse(transData[,geo] %in% thresData[[4]],1,0)
  
  # Combine them
  all <- as.data.frame(cbind(both, house, unit, either))
  
  # Rename
  names(all) <- paste0(timePrefix, "_", names(all), "_",geo)
  
  # Add to existing transactions
  return(cbind(transData, all))
}

