##########################################################################################
#                                                                                        #
#   General Functions for dealing with APM Data                                          #  
#                                                                                        #
##########################################################################################

### Function to source all other necessary custom functions ------------------------------

sourceAPMFunctions <- function(offline=FALSE,              # Are you offline?
                               verbose=FALSE               # Show progress
)
{
  
  if(verbose) cat('Sourcing Custom APM Functions\n')
  
  if(offline){
    source('c:/Code/research/ausPropMrkt/apmOptions.R')
    source('c:/Code/research/ausPropMrkt/apmDataPrep.R')
    source('c:/Code/research/ausPropMrkt/apmDataAnalysis.R')
    source('c:/Code/research/ausPropMrkt/apmSpagMethod.R')
    source('c:/Code/research/ausPropMrkt/apmIndexMethod.R')
    source('c:/Code/research/ausPropMrkt/apmSrmMethod.R')
    source('c:/Code/research/ausPropMrkt/apmHedimpMethod.R')
    source('c:/Code/research/ausPropMrkt/apmPredAccr.R')
    source('c:/Code/research/ausPropMrkt/apmCompBias.R')
    source('c:/code/dataVizTools/ggplottools.R')
    source('c:/Code/dataMgmtTools/dataMungeTools.R')
    
    
    source('c:/Code/dataAnalysisTools/stShardFunctions.R')
  } else {
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmOptions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataPrep.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmDataAnalysis.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmSpagMethod.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmIndexMethod.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmHedimpMethod.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmSrmMethod.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmCompBias.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/ausPropMrkt/',
                  'master/apmPredAccr.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataAnalysisTools/master/stShardFunctions.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataMgmtTools/master/dataMungeTools.R'))
    source(paste0('https://raw.githubusercontent.com/andykrause/',
                  'dataVizTools/master/ggPlotTools.R'))
    
  }
  
 ## Load Libraries
  
  if(verbose) cat('Loading Libraries\n')
 
  require(plyr)
  require(dplyr)
  require(reshape2)
  require(stringr)
  require(maptools)
  require(sp)
  require(rgeos)
  require(spdep)
  require(ggplot2)
  require(akima)
  require(rgdal)
  require(grid)
  require(gstat)
}


