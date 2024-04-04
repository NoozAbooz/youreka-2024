source("data-import.R")

# Replace parms with readable names
station_data[station_data=="URANIUM,   UNFILTERED TOTAL"]<-"uranium"

# Individual two-sided stats testing
individual_statistical_test <- function(contaminant){
  selected_contaminant <- annotate_contaminant(contaminant)
  
  # Divide data into 'rural' and 'urban' dataframes
  rural <- selected_contaminant[selected_contaminant$DESIGNATION == 'rural',]
  urban <- selected_contaminant[selected_contaminant$DESIGNATION == 'urban',]
  
  # Run two-sided t-test
  t_test <- t.test(rural$RESULT, urban$RESULT, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
  
  # Generate boxplot
  long_name <- unique(station_data$PARM_DESCRIPTION[station_data$PARM == contaminant])
  unit <- "mcg / litre"
  boxplot(rural$RESULT, urban$RESULT, names = c('Rural', 'Urban'), main = paste('Boxplot of', long_name, 'concentration in rural and urban areas'), ylab = unit, col = c('red', 'blue'))
  
  return(t_test)
}

individual_statistical_test('UUUT')
