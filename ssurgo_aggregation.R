# need this for ddply()
library(plyr)

# load horizon and component data
chorizon <- read.csv('chorizon_table.csv')

# only keep some of the columns from the component table
component <- read.csv('component_table.csv')[,c('mukey','cokey','comppct_r')]

# custom function for calculating a weighted mean
# values passed in should be vectors of equal length
wt_mean <- function(property, weights)
        {
        # compute thickness weighted mean, but only when we have enough data
        # in that case return NA
        
        # save indices of data that is there
        property.that.is.na <- which( is.na(property) )
		property.that.is.not.na <- which( !is.na(property) )
        
        if( length(property) - length(property.that.is.na) >= 1)
                prop.aggregated <- sum(weights[property.that.is.not.na] * property[property.that.is.not.na], na.rm=TRUE) / sum(weights[property.that.is.not.na], na.rm=TRUE)
        else
                prop.aggregated <- NA

        return(prop.aggregated)
        }

profile_total <- function(property, thickness)
        {
        # compute profile total
        # in that case return NA
        
        # save indices of data that is there
        property.that.is.na <- which( is.na(property) )
		property.that.is.not.na <- which( !is.na(property) )
        
        if( length(property) - length(property.that.is.na) >= 1)
                prop.aggregated <- sum(thickness[property.that.is.not.na] * property[property.that.is.not.na], na.rm=TRUE)
        else
                prop.aggregated <- NA

        return(prop.aggregated)
        }

# define a function to perfom hz-thickness weighted aggregtion
component_level_aggregation <- function(i)
        {

        # horizon thickness is our weighting vector
        hz_thick <- i$hzdepb_r - i$hzdept_r

        # compute wt.mean aggregate values
        clay <- wt_mean(i$claytotal_r, hz_thick)
        silt <- wt_mean(i$silttotal_r, hz_thick)
        sand <- wt_mean(i$sandtotal_r, hz_thick)
        # compute profile sum values
        water_storage <- profile_total(i$awc_r, hz_thick)

        # make a new dataframe out of the aggregate values
        d <- data.frame(cokey=unique(i$cokey), clay=clay, silt=silt, sand=sand, water_storage=water_storage)

        return(d)
        }

mapunit_level_aggregation <- function(i)
        {
        # component percentage is our weighting vector
        comppct <- i$comppct_r

        # wt. mean by component percent
        clay <- wt_mean(i$clay, comppct)
        silt <- wt_mean(i$silt, comppct)
        sand <- wt_mean(i$sand, comppct)
        water_storage <- wt_mean(i$water_storage, comppct)

        # make a new dataframe out of the aggregate values
        d <- data.frame(mukey=unique(i$mukey), clay=clay, silt=silt, sand=sand, water_storage=water_storage)

        return(d)
        }
        
# aggregate horizon data to the component level
chorizon.agg <- ddply(chorizon, .(cokey), .fun=component_level_aggregation, .progress='text')

# join up the aggregate chorizon data to the component table
comp.merged <- merge(component, chorizon.agg, by='cokey')

# aggregate component data to the map unit level
component.agg <- ddply(comp.merged, .(mukey), .fun=mapunit_level_aggregation, .progress='text')

# save data back to CSV
write.csv(component.agg, file='something.csv', row.names=FALSE)
