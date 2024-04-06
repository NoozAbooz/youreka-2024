source("data-import.R")
library(ggplot2)

# Individual two-sided stats testing
individual_statistical_test <- function(contaminant){
  selected_contaminant <- annotate_contaminant(contaminant)
  
  # Divide data into 'rural' and 'urban' dataframes
  rural <- selected_contaminant[selected_contaminant$DESIGNATION == 'rural',]
  urban <- selected_contaminant[selected_contaminant$DESIGNATION == 'urban',]
  
  # Run two-sided t-test, with p = 0.05 
  t_test <- t.test(rural$Result, urban$Result, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
  
  # Generate boxplot
  #long_name <- unique(station_data$PARM_DESCRIPTION[station_data$PARM == contaminant])
  unit <- unique(station_data$'Units'[station_data$'Analyte' == contaminant])
  #boxplot(rural$Result, urban$Result, names = c('Rural', 'Urban'), main = paste(contaminant, 'concentration in rural and urban areas'), ylab = unit, col = c('red', 'blue'), outline=FALSE)
  
  plot.data <- rbind(rural, urban)
  order <- c('rural', 'urban')
  
  ggplot(plot.data, aes(x = DESIGNATION, y = Result, color = DESIGNATION)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = quantile(urban$Result, c(0.1, 0.92))) +
    coord_cartesian(ylim = quantile(urban$Result, c(0.1, 0.92))) +
      
    ggtitle(paste(contaminant, 'concentration in rural and urban water streams')) +
    ylab(unit) +
    scale_x_discrete(limits=order,labels=c("Rural", "Urban")) +
    labs(
      subtitle = "Based on Ontario provincial water quality testing data collected between 2021 and 2022",
      caption = "Source: Ontario Open Data Catalogue, 2024",
      x = "Designation"
    ) +
    theme(
      plot.title = element_text(size = 25, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "italic"),
      plot.caption = element_text(size = 8, face = "italic"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10)
    )
  
  #return(t_test)
}

individual_statistical_test('Manganese')
