##########################################################################################
#                                                                                        #
#  Suite of functions for analyzing price to rent ratios with spatial aggregation method #
#                                                                                        #
##########################################################################################

### Function to compute spatial aggregation value at each level --------------------------

spagLevelWrap <- function(cleanData,            
                          verbose=FALSE
)
{
  
  if(verbose) cat('Calculating yields with median method\n')
  
  ## Metro Level
  
  if(verbose) cat('...At Global level\n')
  spagMetro <- prrStsGeoWrap(stsData = cleanData,
                           metric=c('transValue', 'transValue'),
                           spaceField='all', timeField='transQtr',
                           defDim='time', stsLimit=apmOptions$geoTempLimit, 
                           calcs=list(median='median'))
  
  # Metro by Use
  spagMetroH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  spagMetroU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit',],
                            metric=c('transValue', 'transValue'),
                            spaceField='all', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  ## At LGA Level
  
  if(verbose) cat('...At LGA level\n')
  # All Uses
  spagLga <- prrStsGeoWrap(stsData = cleanData,
                         metric=c('transValue', 'transValue'),
                         spaceField='lga', timeField='transQtr',
                         defDim='time', stsLimit=apmOptions$geoTempLimit, 
                         calcs=list(median='median'))
  
  # By Use
  spagLgaH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  spagLgaU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='lga', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  ## At SLA1 Level
  
  if(verbose) cat('...At SLA1 level\n')
  
  # All Uses  
  spagSla <- prrStsGeoWrap(stsData = cleanData,
                         metric=c('transValue', 'transValue'),
                         spaceField='sla1', timeField='transQtr',
                         defDim='time', stsLimit=apmOptions$geoTempLimit, 
                         calcs=list(median='median'))
  
  # By Use
  spagSlaH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  spagSlaU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                          metric=c('transValue', 'transValue'),
                          spaceField='sla1', timeField='transQtr',
                          defDim='time', stsLimit=apmOptions$geoTempLimit, 
                          calcs=list(median='median'))
  
  ## At Suburb Level
  
  if(verbose) cat('...At Suburb level\n')
  
  # All Uses  
  spagSuburb <- prrStsGeoWrap(stsData = cleanData,
                            metric=c('transValue', 'transValue'),
                            spaceField='suburb', timeField='transQtr',
                            defDim='time', stsLimit=apmOptions$geoTempLimit, 
                            calcs=list(median='median'))
  
  # By Use
  spagSuburbH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  spagSuburbU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                             metric=c('transValue', 'transValue'),
                             spaceField='suburb', timeField='transQtr',
                             defDim='time', stsLimit=apmOptions$geoTempLimit, 
                             calcs=list(median='median'))
  
  ## At PostCode Level
  
  if(verbose) cat('...At Post Code Level\n')
  
  # All Uses   
  spagPostcode <- prrStsGeoWrap(stsData = cleanData,
                              metric=c('transValue', 'transValue'),
                              spaceField='postCode', timeField='transQtr',
                              defDim='time', stsLimit=apmOptions$geoTempLimit, 
                              calcs=list(median='median'))
  
  # by Use   
  spagPostcodeH <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'House', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  # All Uses   
  spagPostcodeU <- prrStsGeoWrap(cleanData[cleanData$PropertyType == 'Unit', ],
                               metric=c('transValue', 'transValue'),
                               spaceField='postCode', timeField='transQtr',
                               defDim='time', stsLimit=apmOptions$geoTempLimit, 
                               calcs=list(median='median'))
  
  ## Combine Results  
  
  spagResults <- list(Global=list(all=spagMetro, house=spagMetroH, unit=spagMetroU),
                    lga=list(all=spagLga, house=spagLgaH, unit=spagLgaU),
                    sla1=list(all=spagSla, house=spagSlaH, unit=spagSlaU),
                    suburb=list(all=spagSuburb, house=spagSuburbH, unit=spagSuburbU),
                    postCode=list(all=spagPostcode, house=spagPostcodeH, unit=spagPostcodeU))
  
  ## Return Results
  
  return(spagResults)
}
