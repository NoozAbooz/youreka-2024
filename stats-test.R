source("data-import.R")
options(warn=-1)
library(ggplot2)
library(dplyr)

# Individual two-sided stats testing
individual_statistical_test <- function(contaminant){
  selected_contaminant <- annotate_contaminant(contaminant)
  
  # Divide data into 'rural' and 'urban' dataframes
  rural <- selected_contaminant[selected_contaminant$DESIGNATION == 'rural',]
  urban <- selected_contaminant[selected_contaminant$DESIGNATION == 'urban',]
  
  # Run two-sided t-test, with p = 0.05. Data is not normally distributed so don't use this
  #t_test <- t.test(rural$Result, urban$Result, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
  # Run Mann Whitney/wilcox U test instead
  wilcox_test <- wilcox.test(rural$Result, urban$Result, alternative = "two.sided", conf.level = 0.95, exact = FALSE)
  
  # Generate boxplot
  #long_name <- unique(station_data$PARM_DESCRIPTION[station_data$PARM == contaminant])
  unit <- unique(station_data$'Units'[station_data$'Analyte' == contaminant])
  
  # Combine into one dataframe
  plot.data <- rbind(rural, urban)
  order <- c('rural', 'urban')
  
  # Generate boxplot with ggplot default colours
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
  # Save plot as image
  ggsave(paste('./plots/', contaminant, '_boxplot.png'), width = 20, height = 15, units = 'in', dpi = 300)
  
  # Generate violin plot
  ggplot(plot.data, aes(x = DESIGNATION, y = Result, fill = DESIGNATION)) +
    geom_violin(trim = FALSE) +
    scale_y_continuous(limits = quantile(urban$Result, c(0.1, 0.92))) +
    coord_cartesian(ylim = quantile(urban$Result, c(0.1, 0.92))) +
    ggtitle(paste(contaminant, 'distribution in rural and urban water streams')) +
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
  
  ggsave(paste('./plots/', contaminant, '_violinplot.png'), width = 20, height = 15, units = 'in', dpi = 300)
  
  # Return test results from before
  sink("wilcox_test.txt", append=TRUE)
  cat(contaminant, "__**CONTAMINANT**__\n")
  lapply(wilcox_test, write, "wilcox_test.txt", append=TRUE, ncolumns=1000)
}

contaminants_to_test <- c('Reactive silicate', 'Copper', 'Uranium', 'Zinc', 'Barium', 'Manganese', 'Strontium', 'Lithium', 'Ammonia & ammonium', 'Molybdenum')
i <- 1
for (i in contaminants_to_test){
    individual_statistical_test(i)
}
