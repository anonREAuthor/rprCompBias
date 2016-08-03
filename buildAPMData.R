##########################################################################################
#                                                                                        #
#  Process for converting raw APM .zip files (form AURIN) into .csvs and a .db database  #
#                                                                                        #
##########################################################################################

##########################################################################################
###  Example Code Use                                                                    #
##########################################################################################

if(F){
  
  # Set base path
  basePath <- 'c:/temp/apm'
  
  # Run full conversion
  buildAPMData(basePath,
               newFileName = 'test.db',
               transList = list('rentals'='rent','sales'=c('sold', 'auct')),
               verbose = TRUE)
  
  # Instruction for simply converting .zips to .csvs 
  #(warning this doesn't correct bad file names)
  convertAPMData(basePath, transType='sold', verbose=TRUE)

}

##########################################################################################
###  Functions                                                                           #   
##########################################################################################

### Master function that converts all zips into a single .db -------------------------

buildAPMData <- function(basePath,                                    # Path to data
                         newFileName='test.db',                       # Name of exp. file
                         transList=list('rentals'='rent',
                                          'sales'=c('sold', 'auct')), # List of file types
                         verbose=TRUE                                 # Show progress?
){
  
  # Set up libraries
  require(RSQLite)
  require(plyr)
  require(RODBC)
  require(RCurl)
  
  # Source necessary files
  source(paste0("https://raw.githubusercontent.com/andykrause/dataMgmtTools/",
                "master/basicConversionTools.R"))
  
  # Fix any bad file names
  fixFileNames(basePath, badStr='.csv.zip', newStr='.zip', verbose=verbose)
  
  # Build a temporary folder
  dir.create(paste0(basePath, '/temp'), showWarnings = FALSE)
  
  # Convert .zips to .csvs
  if(verbose) cat('Converting .zips to .csvs', '\n')
  for(tL in 1:length(transList)){
    convertAPMData(basePath, transType=transList[[tL]], 
                   folderName=names(transList[tL]), verbose=verbose)
  }
  
  # Combine all .csvs into a SQLite database
  if(verbose) cat('Converting .csvs to .db', '\n')
  convertCSVtoSQLite(dataPathCurrent=basePath,
                     newFileName=newFileName,
                     verbose=verbose,
                     overWrite=TRUE,
                     tableNames=c('rentals','sales'))
  
  # Successful output message
  if(verbose) cat('\n***CONVERSION SUCCESSFUL.  Data saved in:', 
                  paste0(basePath, '/', newFileName), '***\n')
  
  # Remove temporary directory
  unlink(paste0(basePath, '/temp'), recursive=TRUE, force=TRUE)
  
}

### Helper function that unzips, combines and turns .zips in to a .csv -------------------

convertAPMData <- function(basePath,            # The main directory where the datalives
                           transType='sold',    # Type of data (subdirectory) to work on
                           folderName=NULL,     # Name export folder if not transTYpe
                           verbose=TRUE         # Do you want to see the progress?
){
  
  # Require libraries
  require(plyr)
  
  # Fix to lower case
  transType <- tolower(transType)
  
  # Get a list of zip files to extract & remove non zip
  if(verbose) cat("Reading in .ZIP files to be extracted\n")
  zipFiles <- tolower(list.files(basePath))
  zipFiles <- as.list(zipFiles[grep('.zip', zipFiles)])
  zipFiles <- selectFiles(zipFiles, transType)
  
  # Convert to .csv
  if(verbose) cat("Converting to .csv\n")
  lapply(zipFiles, extractAPMData, dataPath=basePath, verbose=TRUE)
  
  # Conversion Message
  if(verbose) cat(length(zipFiles), 'Files extracted\n')
  
  # Read in all data
  if(verbose) cat('Reading in all .csv files \n')
  csvFiles <- list.files(paste0(basePath, '/temp'))
  csvFiles <- as.list(paste0(basePath, '/temp/', csvFiles[grep('.csv', csvFiles)]))
  csvFiles <- selectFiles(csvFiles, transType)

  # Read .csv into memory
  csvData <- lapply(csvFiles, tryReadCSV, stringsAsFactors=FALSE)
  
  # Remove temp files
  lapply(csvFiles, file.remove)
  
  # Merge into a single file
  if(verbose) cat('Merging to a single file \n')
  csvData <- rbind.fill(csvData)
  
  # Write out the file
  if(is.null(folderName)) folderName <- transType[1]
  if(verbose) cat('Writing out merged ', folderName, 'file to ',
                  paste0(basePath, '/', folderName, '.csv'),'\n')
  write.csv(csvData, paste0(basePath, '/', folderName, '.csv'))
}

### Helper functions that handles the unzipping and renaming process ---------------------

extractAPMData <- function(fileName,             # File name to be extracted
                           dataPath,             # Path to the data (.zip file)
                           verbose=TRUE          # Do you want to see the progress?
){
  
  # Create temporary directory  
  tempPath = gsub('.zip', '', paste0(dataPath, '/temp/', fileName))
  dir.create(tempPath, showWarnings = FALSE)
  
  # Extract data
  if(verbose){
    cat('  Extracting, renaming and moving', gsub('.zip', '.csv', fileName), '\n')
  }
  unzip(paste0(dataPath, '/', fileName), exdir=tempPath)
  
  # Remove all non .csv files
  fNames <- list.files(tempPath)
  xCut <- c(grep('.csv', fNames))
  if(length(xCut) != 0) xNames <- as.list(paste0(tempPath, '/', fNames[-xCut]))
  lapply(xNames, file.remove)

  # Rename File
  newName <- paste0(gsub('.zip', '', fileName), '.csv')
  file.rename(from=paste0(tempPath, '/', list.files(tempPath)[1]),
              to=paste0(tempPath, '/', newName))
  
  # Move File
  file.copy(from=paste0(tempPath, '/', newName),
            to=paste0(dataPath, '/temp/', newName),
            overwrite=TRUE)
  
  # Delete old file and directory
  file.remove(paste0(tempPath, '/', list.files(tempPath)))
  unlink(tempPath, recursive = TRUE, force=TRUE) 
  
}

### Helper function that allow for sources files directly from github --------------------

sourceHttps <- function(u,                       # URL of file
                        unlink.tmp.certs = FALSE # Security cert handling
) {
  # load package
  require(RCurl)
  
  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")){
    download.file(url="http://curl.haxx.se/ca/cacert.pem",
                  destfile = "cacert.pem")
  }
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}

### Helper function that pulls out certain files from a list -----------------------------

selectFiles <- function(fileList,         # list/vector of existing files
                        selector          # vector of text string to look for in list
                        ){
  # Single selector
  if(length(selector) == 1){
    fileList <- as.list(fileList[grep(selector, fileList)])
  } else {
    
  # Multiple selectors  
    xFiles <- list(0)
    for(xF in 1:length(selector)){
      xFiles[[xF]] <- as.list(fileList[grep(selector[xF], fileList)])
    }
    fileList <- as.list(unlist(xFiles))
  }
    
  # Return Value
  return(fileList)
}

### Function that fixes bad file names ---------------------------------------------------

fixFileNames <- function(dirPath,                 # File directory to work in
                         badStr,                  # Bad string part of file name
                         newStr="",               # New string to replace it with
                         verbose=FALSE            # Show progress?
){
  
  #  Read in list of file names
  allFiles <- tolower(list.files(dirPath))
  
  # Locate those containing bad names
  toRename <- grep(badStr, allFiles)
  
  # Fix the names
  if(length(toRename) > 0){
    if(verbose) cat('Replacing', badStr, 'with', newStr, '\n') 
    for(tR in 1:length(toRename)){
      currName <- paste0(dirPath, '/', allFiles[toRename[tR]])
      newName <- gsub(badStr, newStr, currName)
      file.rename(from=currName, to=newName)
    }
   if(verbose) cat(length(toRename), 'file name(s) fixed.\n\n') 
  }
}

### Function that averts errors caused by reading in .csvs with no data ------------------

tryReadCSV <- function(x,                        # File Name
                       stringsAsFactors=FALSE    # Option
                       )
{
  xx <- try(read.csv(x, stringsAsFactors=stringsAsFactors), silent=TRUE)
  if(class(xx) == 'data.frame') return(xx)
}


