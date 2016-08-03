
### Function to set the intial, default option levels --------------------------

apmSetOptions <- function(show=FALSE    # Print options to the screen
                          )
  {
  
 ## Set the geographic levels to use throughout the analysis
  
  geo.levels <- c('Global', 'lga', 'suburb')
  
 ## Set field from which to require obsevations
  
  reqFields <- c('transValue', 'AreaSize', 'Bedrooms', 'Baths', 'ssInteg', 'ssChoice')
  
 ## Set the initial limits
  
  areaLimits <- list(min=40, max=25000)
  bathLimits <- list(min=1, max=8)
  bedLimits <- list(unitMin=0, houseMin=1, unitMax=6, houseMax=8)  
  rentLimits <- list(min=125, max=2500)
  saleLimits <- list(min=150000, max=4000000)
  
 ## Set raw data column list
  
  rawColumnList <- c('UID', 'GeographicalID', 'EventID', 'AddressID', 
                     'FlatNumber', 'transDate', 'transValue', 'transType',
                     'PropertyType', 'Property_Latitude', 'Property_Longitude',
                     'AreaSize', 'Bedrooms', 'Baths', 'Parking','HasFireplace',
                     'HasPool', 'HasGarage', 'HasAirConditioning')
  
  naFields <- list('HasPool', 'HasGarage', 'HasAirConditioning', 'HasFireplace')
  ssFields <- c('L_choice_2500', 
                'T64_Integration_Segment_Length_Wgt_R25000_metric')
  unitTypes <- c('Unit', 'Studio')
  houseTypes <- c('House', 'Terrace', 'Townhouse', 'Villa', 'Duplex')
  
 ## Set time information
  
  startYear <- 2011
  endYear <- 2015
  startMonth <- 1
  time.field <- 'transQtr'
  time.periods <- 20
  geoTempLimit <- 3
  geoTempFieldUnit <- "QT_unit_postCode"
  geoTempFieldHouse <- "QT_house_postCode"
  
 ## Set up index parameters
  
  srs.model <- TRUE
  srs.limit <- 100
  
 ## Set equations for impute model  
  
  unitEquation <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr)  + Bedrooms + Baths + HasPool + HasGarage #
  
  houseEquation <- log(transValue) ~ as.factor(postCode) + as.factor(transQtr) + 
    Bedrooms + Baths + HasPool + HasGarage + log(AreaSize) 
  
 ## Add to a temporary list of options  
  
  tempOptions <- list(geo.levels = geo.levels,
                      areaLimits = areaLimits,
                      bathLimits = bathLimits,
                      bedLimits = bedLimits,
                      rentLimits = rentLimits,
                      saleLimits = saleLimits,
                      rawColumnList = rawColumnList,
                      reqFields = reqFields,
                      naFields = naFields,
                      ssFields = ssFields,
                      startYear = startYear,
                      endYear = endYear,
                      startMonth = startMonth,
                      time.field = time.field,
                      time.periods = time.periods,
                      unitTypes = unitTypes,
                      houseTypes = houseTypes,
                      geoTempLimit = geoTempLimit,
                      geoTempFieldHouse = geoTempFieldHouse,
                      geoTempFieldUnit = geoTempFieldUnit,
                      houseEquation = houseEquation,
                      unitEquation = unitEquation,
                      srs.model = srs.model,
                      srs.limit = srs.limit)
 
 ## Assign the options to the global environment  
   
  assign('apmOptions', tempOptions, envir=.GlobalEnv)
  
 ## If show, then print to screen  
  
  if(show) return(tempOptions)
}
  
### Function for changing individual options -----------------------------------

apmChangeOptions <- function(...,         # List of options to change
                             show=FALSE   # Print all options to screen?
                             )
  {
 
 ## Example function calls
  
  if(F){
    apmChangeOptions(list(bathLimits=list(min=1)))
    apmChangeOptions(list(bathLimits=list(min=1),
                          bathLimits=list(max=9),
                          rentLimits=list(max=2300)))
    }
  
 ## Extract options to change
  
  chgOpts <- list(...)[[1]]
  
 ## Make changes
  
  for(ij in 1:length(chgOpts)){
    
    # ID the name of opt to change
    optNames <- names(chgOpts)[ij]
    
    # ID the location of the option
    idOpts <- which(names(apmOptions) == optNames)
    
    # ID the location of the sub option (the particular value)
    idsubOpts <- which(names(apmOptions[[idOpts]]) == names(chgOpts[[ij]]))
    
    # Make the change to the global object
    apmOptions[[idOpts]][[idsubOpts]] <- chgOpts[[ij]][[1]]
    
    # Print the option change to the screen
    cat('Option Updated:\n', names(apmOptions)[[idOpts]], 
        names(apmOptions[[idOpts]])[[idsubOpts]], 'now set to', 
        apmOptions[[idOpts]][[idsubOpts]], '\n')
  }

 ## Assign new values to global environment  
    
 assign('apmOptions', apmOptions, envir=.GlobalEnv)
  
 ## If show, then print all to screen  
 if(show) return(apmOptions) 
}
  
### Create global plot options -----------------------------------------------------------

apmPlotOptions <- function(){
  
  # Set colors for plots
  assign('unitCols', c('forestgreen', 'green', 'lightgreen'), envir=.GlobalEnv)
  assign('houseCols', c('navy', 'royalblue2', 'skyblue'), envir=.GlobalEnv)
  assign('methSizes', c(1, 1.5, 2), envir=.GlobalEnv)
  assign('methLines', c(1, 1, 1), envir=.GlobalEnv)  
  
  # Set graphical theme
  theme_prr <- theme_grey() +
    theme(text = element_text(size=11),
          panel.background = element_rect(colour='gray95', fill='gray95'),
          panel.grid.major=element_line(colour='white', size=.5),
          panel.grid.minor=element_line(colour='white', size=.1),
          plot.background=element_rect(fill='white'),
          axis.title.y=element_text(colour='black'),
          axis.text.y=element_text(hjust=1),
          legend.position='bottom',
          legend.background=element_rect(fill='white'),
          legend.key=element_rect(fill='white', color='white'),
          legend.text=element_text(color='black'),
          legend.title=element_blank(),
          legend.key.width=unit(2, "cm"),
          strip.background = element_rect(fill = "gray50", 
                                          color = "gray50", size = .1),
          strip.text.x = element_text(face = "bold"),
          strip.text.y = element_text(face = "bold"))
  assign('theme_prr', theme_prr, envir=.GlobalEnv)
}

