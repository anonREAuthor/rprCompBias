##########################################################################################
#                                                                                        #
#  Functions to analyze composition bias                                                 #
#                                                                                        #
##########################################################################################

### Structural Bias ----------------------------------------------------------------------

apmStrBias <- function(trans.data,
                       time.field,
                       house.cats=c('Bedrooms', 'Baths', 'AreaSize'),
                       unit.cats=c('Bedrooms', 'Baths', 'Parking')
                       )
  {  
  
  # ARGUMENTS
  #
  # trans.data: full transaction data
  # time.field: field containing time breakdown
  # house.cats: categories to consider for houses
  # unit.cats: categories to consider for units
  
 ## Order the categories
  
  house.cats <- sort(house.cats)
  unit.cats <- sort(unit.cats)
  
 ## Split Data
  
  # Sale and Rents
  sales <- trans.data[trans.data$transType == 'sale',]
  rents <- trans.data[trans.data$transType == 'rent',]
  
  # Units and House by S/R
  sales.h <- sales[sales$PropertyType == 'House',]
  sales.u <- sales[sales$PropertyType == 'Unit',]
  rents.h <- rents[rents$PropertyType == 'House',]
  rents.u <- rents[rents$PropertyType == 'Unit',]
  
  # Measure presence of all categories in all time periods
  t.len <- length(1:max(trans.data[,time.field]))
  sh.len <- length(table(sales.h[,time.field]))
  rh.len <- length(table(rents.h[,time.field]))
  su.len <- length(table(sales.u[,time.field]))
  ru.len <- length(table(rents.u[,time.field]))
  
 ## Houses
  
  # If all categories filled
  if(sh.len == t.len & rh.len == t.len){
  
    # Make capture list
    house.list <- list()
    
    # Run through categories
    for(i in 1:length(house.cats)){
      cat.hs <- tapply(sales.h[,house.cats[i]], sales.h[,time.field], mean, na.rm=TRUE)
      cat.hr <- tapply(rents.h[,house.cats[i]], rents.h[,time.field], mean, na.rm=TRUE)
      house.list[[i]] <- cat.hs / cat.hr
    }
  
    # Make combined average
    house.mean <- Reduce('+', house.list) / length(house.list)
    
    # Build data.frame componenent
    h.time <- rep(1:t.len, length(house.cats) + 1)
    h.fields <- c(sort(rep(house.cats, t.len)), rep('Combined', t.len))
    h.ratio <- c(unlist(house.list), house.mean)
    
    # Put into a data.frame
    house.val <- data.frame(time=h.time,
                            field=h.fields,
                            ratio=h.ratio,
                            type='House')
  } else {
    house.val <- NULL
  }
  
 ## Units  
  
  # If all categories filled
  if(su.len == t.len & ru.len == t.len){
    
    # Make capture list
    unit.list <- list()
    
    # Run through categories
    for(i in 1:length(unit.cats)){
      cat.us <- tapply(sales.u[,unit.cats[i]], sales.u[,time.field], mean, na.rm=TRUE)
      cat.ur <- tapply(rents.u[,unit.cats[i]], rents.u[,time.field], mean, na.rm=TRUE)
      unit.list[[i]] <- cat.us / cat.ur
    }
    
    # Make combined average
    unit.mean <- Reduce('+', unit.list) / length(unit.list)
    
    # Build data.frame componenent
    u.time <- rep(1:t.len, length(unit.cats) + 1)
    u.fields <- c(sort(rep(unit.cats, t.len)), rep('Combined', t.len))
    u.ratio <- c(unlist(unit.list), unit.mean)
    
    # Put into a data.frame
    unit.val <- data.frame(time=u.time,
                           field=u.fields,
                           ratio=u.ratio,
                           type='Unit')
  } else {
    unit.val <- NULL
  }
  
 ## Return Values  
  
  return(rbind(house.val, unit.val))

}

### Locational Bias ----------------------------------------------------------------------

apmLocBias <- function(sp.data, 
                       geo.field
                       )
  {  
  
  # ARGUMENTS
  #
  # sp.data: full transaction data
  # geo.field: field containing geographic level of aggregation

 ## Fix trans.data
  
  if(class(sp.data) == 'SpatialPointsDataFrame'){
    trans.data <- sp.data@data
  } else {
    trans.data <- sp.data
  }
  
 ## Split sales into houses and units  
  
  sales.h <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'House',]
  sales.u <- trans.data[trans.data$transType == 'sale' & 
                          trans.data$PropertyType == 'Unit',]
  
 ## Build a-spatial models  
  
  house.mod <- lm(log(transValue) ~ log(AreaSize)+Bedrooms+Baths+HasPool+HasGarage,
                  data=sales.h)
  unit.mod <- lm(log(transValue) ~ Bedrooms+Baths+HasPool+HasGarage,
                 data=sales.u)

 ## Add residuals to the data
  
  sales.h$resid <- house.mod$resid
  sales.u$resid <- unit.mod$resid
  
  # Combine data
  sales <- rbind(sales.h, sales.u)
  
  # Create df of mean residuals per geo.field
  s.me <- tapply2DF(sales$resid, sales[,geo.field], median)
  
 ## Split into houses and units
  
  houses <- trans.data[trans.data$PropertyType == 'House',]
  units <- trans.data[trans.data$PropertyType == 'Unit',]
  
 ## Add mean spatial difference to dataset  
  
  houses$loc.s <- s.me$Var[match(houses[,geo.field], s.me$ID)]
  units$loc.s <- s.me$Var[match(units[,geo.field], s.me$ID)]
  
 ## Calculate mean spatial dif for house rent  
  
  house.ratio <- tapply(houses$loc.s, houses$transType, mean, na.rm=TRUE)
  unit.ratio <- tapply(units$loc.s, units$transType, mean, na.rm=TRUE)
  
 ## Calculate difference between the two  
  
  house.dif <- house.ratio[2] - house.ratio[1]
  unit.dif <- unit.ratio[2] - unit.ratio[1]
  
 ## Return Values  
  
  return(list(dif=c(house.dif, unit.dif),
              data=rbind(houses, units)))
} 

### Omitted Variable Bias ----------------------------------------------------------------

apmOmitBias <- function(dif.data, 
                        match.data
                        )
  {  
  
  # ARGUMENTS
  #
  # dif.data: Diffenced locational data from apmLocBias()
  # match.data: Matched dataset
  
 ## Remove data missing locational values
  
  dif.data <- dif.data[!is.na(dif.data$loc.s),]
  
 ## Split sales into houses and units  
  
  sales.h <- dif.data[dif.data$transType == 'sale' & 
                        dif.data$PropertyType == 'House',]
  sales.u <- dif.data[dif.data$transType == 'sale' & 
                        dif.data$PropertyType == 'Unit',]
  
 ## Estimate models  
  
  house.mod <- lm(log(transValue) ~ log(AreaSize) + Bedrooms + Baths + HasPool +
                    HasGarage + loc.s, data=sales.h)
  unit.mod <- lm(log(transValue) ~ Bedrooms + Baths + HasPool + HasGarage + loc.s,
                   data=sales.u)
  
 ## Add residuals to the data
  
  sales.h$resid <- house.mod$resid
  sales.u$resid <- unit.mod$resid
  
 ## Isolate owner vs. renter homes   
  
  # Houses
  owner.houses <- sales.h[!sales.h$UID %in% match.data$saleID, ]
  rent.houses <- sales.h[sales.h$UID %in% match.data$saleID, ]
  
  # Units
  owner.units <- sales.u[!sales.u$UID %in% match.data$saleID, ]
  rent.units <- sales.u[sales.u$UID %in% match.data$saleID, ]
  
 ## Create the difference between the two  
  
  house.ratio <- (mean(owner.houses$resid, na.rm=TRUE) - 
                    mean(rent.houses$resid, na.rm=TRUE))
  unit.ratio <- (mean(owner.units$resid, na.rm=TRUE) - 
                   mean(rent.houses$resid, na.rm=TRUE))
  
 ## Return Values  
  
  return(list(dif=c(house.ratio, unit.ratio)))

} 

