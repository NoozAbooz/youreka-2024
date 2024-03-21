# Import designations
station_designation <- read.csv('station_designation.csv')
station_designation <- station_designation[, c('STATION', 'DESIGNATION')]

duplicate_stations <- duplicated(station_designation[, c('STATION')])
station_designation <- station_designation[!duplicate_stations, ]

# Import station data
station_data <- read.csv('station_data.csv')
station_data <- station_data[, c('STATION', 'PARM', 'PARM_DESCRIPTION', 'YEAR', 'RESULT', 'UNITS')]
station_data <- station_data[station_data$YEAR==2018,]

pull_contaminant <- function(contaminant){
    contaminant_data <- station_data[station_data$PARM == contaminant,]
    contaminant_data$DESIGNATION <- NA
    return(contaminant_data)
}

filter_contaminant <- function(new_contaminant) {
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

test <- filter_contaminant('HGUT')
