# Import designations
station_designation <- read.csv('station_designation.csv')
station_designation <- station_designation[, c('STATION', 'DESIGNATION')]

## Remove duplicate stations
duplicate_stations <- duplicated(station_designation[, c('STATION')])
station_designation <- station_designation[!duplicate_stations, ]

# Import station data
station_data <- read.csv('station_data_2021-2022.csv')
# Keep purge any rows that contain "<" in their result column since they are invalid for a numeric field
# It seems like these rows indicate the concentrations were too small for the instruments to detect, so we should ignore regardless
station_data <- station_data[!grepl("[^0-9.]", station_data$Result), ]
station_data$Result <- as.numeric(station_data$Result)
# Remove rows with empty results
station_data<- station_data[complete.cases(station_data), ]
station_data <- station_data[, c('Year', 'Collection.Site', 'Analyte', 'Result', 'Units')]

# Return every known entry for given contaminant
pull_contaminant <- function(contaminant){
    contaminant_data <- station_data[station_data$'Analyte' == contaminant,]
    contaminant_data$DESIGNATION <- NA
    return(contaminant_data)
}

# References station_designation and appends designation status 
# for every station with given contaminant
annotate_contaminant <- function(new_contaminant) {
    new_contaminant_data <- pull_contaminant(new_contaminant)

    i <- 1
    for (i in 1:nrow(new_contaminant_data)){
      # Cross reference 2nd column in station_data, "Collection.Site" for station ID
      match_id <- new_contaminant_data[i,2]
      if (match_id %in% station_designation$STATION) {
        new_contaminant_data[i,]$DESIGNATION <- station_designation[station_designation$STATION == match_id, ]$DESIGNATION
      } 
    }
  
    return(new_contaminant_data[!is.na(new_contaminant_data$DESIGNATION), ])
}
