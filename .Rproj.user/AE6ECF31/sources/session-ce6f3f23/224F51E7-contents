library(ggplot2)

# Import designations
station_designation <- read.csv('station_designation.csv')
station_designation <- station_designation[, c('STATION', 'DESIGNATION')]

## Remove duplicate stations
duplicate_stations <- duplicated(station_designation[, c('STATION')])
station_designation <- station_designation[!duplicate_stations, ]

# Import station data, only include from latest year 2018
station_data <- read.csv('station_data.csv')
station_data <- station_data[, c('STATION', 'PARM', 'PARM_DESCRIPTION', 'YEAR', 'RESULT', 'UNITS', 'VALUQUALIFI')]
station_data <- station_data[station_data$YEAR==2018,]

# Return every known entry for given contaminant
pull_contaminant <- function(contaminant){
    contaminant_data <- station_data[station_data$PARM == contaminant,]
    contaminant_data$DESIGNATION <- NA
    return(contaminant_data)
}

# References station_designation and appends designation status 
# for every station with given contaminant
annotate_contaminant <- function(new_contaminant) {
  new_contaminant_data <- pull_contaminant(new_contaminant)

  i <- 1
  for (i in 1:nrow(new_contaminant_data)){
    match_id <- new_contaminant_data[i,1]
   if (match_id %in% station_designation$STATION) {
     new_contaminant_data[i,]$DESIGNATION <- station_designation[station_designation$STATION == match_id, ]$DESIGNATION
   } 
  }
  
  return(new_contaminant_data[!is.na(new_contaminant_data$DESIGNATION),])
}