source("data-import.R")

# Generate list of significantly different contaminants (p-value < 0.05) and which side (rural vs urban) is larger
parm_names <- unique(station_data$'Analyte')

significantly_different <- c()
higher_concentration <- c()

for (parm in parm_names){
  selected_contaminant <- annotate_contaminant(parm)
  
  # Divide data into 'rural' and 'urban' dataframes
  rural <- selected_contaminant[selected_contaminant$DESIGNATION == 'rural',]
  urban <- selected_contaminant[selected_contaminant$DESIGNATION == 'urban',]
  
  # Perform t-test only if value exists for both
  if (nrow(rural) <= 1 | nrow(urban) <= 1){
    next
  }
  
  t_test <- t.test(rural$Result, urban$Result, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
  
  # Check if p-value is less than 0.05
  if (!is.na(t_test$p.value) && t_test$p.value < 0.05){
    # Replace parm with PARM_DESCRIPTION from station_data
    #parm <- unique(station_data$PARM_DESCRIPTION[station_data$PARM == parm])
    significantly_different <- c(significantly_different, parm)
    
    if (t_test$estimate[2] > t_test$estimate[1]){
      higher_concentration <- c(higher_concentration, 'urban')
    } else {
      higher_concentration <- c(higher_concentration, 'rural')
    }
  }
}
# Save significantly_different and higher concentration to csv
write.csv(data.frame(significantly_different, higher_concentration), 'significantly_different.csv', row.names = FALSE)