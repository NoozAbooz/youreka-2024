source("data-import.R")
options(warn=-1)
library(ggplot2)
library(dplyr)
file.remove("wilcox_test.txt")

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
        scale_color_grey() +
        scale_fill_grey() +
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
            plot.title = element_text(size = 40, face = "bold"),
            plot.subtitle = element_text(size = 30, face = "italic"),
            plot.caption = element_text(size = 30, face = "italic"),
            axis.title.x = element_text(size = 30, face = "bold"),
            axis.title.y = element_text(size = 30, face = "bold"),
            axis.text.x = element_text(size = 30),
            axis.text.y = element_text(size = 30),
            legend.title = element_text(size = 22),
            legend.text = element_text(size = 30)
        )
    # Save plot as image
    ggsave(paste('./plots/', contaminant, '_boxplot.png'), width = 28, height = 15, units = 'in', dpi = 300)
    
    # Generate violin plot
    ggplot(plot.data, aes(x = DESIGNATION, y = Result, fill = DESIGNATION)) +
        scale_color_grey() +
        scale_fill_grey() +
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
            plot.title = element_text(size = 40, face = "bold"),
            plot.subtitle = element_text(size = 30, face = "italic"),
            plot.caption = element_text(size = 30, face = "italic"),
            axis.title.x = element_text(size = 30, face = "bold"),
            axis.title.y = element_text(size = 30, face = "bold"),
            axis.text.x = element_text(size = 30),
            axis.text.y = element_text(size = 30),
            legend.title = element_text(size = 22),
            legend.text = element_text(size = 30)
        )
    ggsave(paste('./plots/', contaminant, '_violinplot.png'), width = 28, height = 15, units = 'in', dpi = 300)
    
    # Return test results from before
    cat(contaminant, "__**CONTAMINANT**__\n")
    lapply(c(contaminant, "__**CONTAMINANT**__\n", wilcox_test, "\n"), write, "wilcox_test.txt", append=TRUE, ncolumns=1000)
}

significantly_different <- read.csv('significantly_different.csv')
parm_names <- unique(significantly_different$'significantly_different')

# Iterate through chosen list of contaminants and generate plots/p-values
for (parm in parm_names){
    individual_statistical_test(parm)
}