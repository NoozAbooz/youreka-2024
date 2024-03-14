# Import designations
station_designation <- read.csv('station_designation.csv')
station_designation <- station_designation[, c('STATION', 'DESIGNATION')]

duplicate_stations <- duplicated(station_designation[, c('STATION')])
station_designation <- station_designation[!duplicate_stations, ]

# Import station data
station_data <- read.csv('station_data.csv')
station_data <- station_data[, c('STATION', 'PARM', 'PARM_DESCRIPTION', 'YEAR', 'RESULT', 'UNITS')]
station_data <- station_data[station_data$YEAR==2018,]

hg_data <- station_data[station_data$PARM == 'HGUT',]
