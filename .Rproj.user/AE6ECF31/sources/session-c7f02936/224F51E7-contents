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

hgut_data <- pull_contaminant('HGUT')

i <- 1
for (i in 1:nrow(hgut_data)){
  match_id <- hgut_data[i,1]
  hgut_data[i,]$DESIGNATION <- station_designation[station_designation$STATION == match_id, ]$DESIGNATION
}

