# Information -------------------------------------------------------------

# Written by: Drew DiPrinzio
# Last Updated: 3/31/2020

# Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

# Import data -------------------------------------------------------------
most_correlated <- readRDS('data/datasets/most_correlated_pairs.rds')

# Run Density Plots and Line Charts for specified country crop pairs ------

  plot_pairs <- function(crop,pair,min_date,max_date,rank) {
    
    print(glue('Creating plots for Pair: {pair} & Crop: {crop}'))
    
    #Prepare country names for filenames and chart elements
    country_list <- str_split(pair,";")[[1]]
    country1 <- country_list[1]
    country2 <- country_list[2]
    crop_name <- crop %>% tolower() %>% str_replace_all('[ -]',"_") %>% str_remove_all('[()]')
    
    export_folder <- paste0('export/',rank,"_",tolower(country1),"_",tolower(country2),'_',tolower(crop_name)) %>%
      str_replace_all("___","_") %>%
      str_replace_all(" ","_")
    
    if(!dir.exists(export_folder)) {dir.create(export_folder)}
    
    #Read in rds saved in "run_summary_stats" function
    rds_name <- paste0('export/country_pairs/',tolower(country1),"_",tolower(country2),'_',tolower(crop_name),'_returns.rds') %>%
      str_replace_all(.,"___","_") %>%
      str_replace_all(.," ","_")
    
    inter <- readRDS(rds_name)
    
    #Run density plot
    density_plot <- inter %>%
      ggplot(aes(x=return,color=country_name)) +
      geom_density() + 
      theme_minimal() + 
      ggtitle(sprintf('Distribution of Returns\n%s and %s\n%s',country1,country2,crop)) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab('Return (%)') + ylab('Density') + labs(color='Country Name')
    
    density_plot
    
    #Save density plot
    density_name <- paste0(export_folder,"/",tolower(country1),"_",tolower(country2),'_',tolower(crop_name),'_density.jpg') %>%
      str_replace_all(.,"___","_") %>%
      str_replace_all(.," ","_")
    
    ggsave(density_name,device = 'jpeg', width = 3.72, height = 3.72)
    
    #Run index returns chart
    line_plot <- inter %>%
      ggplot(aes(x = date, y = index, color = country_name)) +
      geom_line() +
      geom_smooth(method = 'lm') +
      xlim(min_date,max_date) +
      theme_minimal() +
      ggtitle(sprintf('Indexed Returns\n%s and %s\n%s',country1,country2,crop)) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab('Date') + ylab('Indexed Price') + labs(color='Country Name')
    
    line_plot
    
    #Save index returns charts 
    line_name <- paste0(export_folder,"/",tolower(country1),"_",tolower(country2),'_',tolower(crop_name),'_line.jpg') %>%
      str_replace_all(.,"___","_") %>%
      str_replace_all(.," ","_")
    
    ggsave(line_name,device = 'jpeg', width = 3.72, height = 3.72)
    
    #Run correlation chart
    inter2 <- inter %>% 
      select(date,country_name,return) %>%
      pivot_wider(.,names_from = country_name, values_from = return)

    corr_plot <- inter2 %>%
      ggplot(aes(x=get(colnames(inter2)[2]),y=get(colnames(inter2)[3]))) + 
      geom_point() +
      geom_smooth(method = 'lm') +
      theme_minimal() +
      ggtitle(sprintf('Correlation of Returns\n%s and %s\n%s',country1,country2,crop)) +
      theme(plot.title = element_text(hjust = 0.5)) + 
      xlab(colnames(inter2)[2]) + ylab(colnames(inter2)[3]) 
    
    corr_plot
    
    #Save correlation chart
    corr_name <- paste0(export_folder,"/",tolower(country1),"_",tolower(country2),'_',tolower(crop_name),'_correlation.jpg') %>%
      str_replace_all(.,"___","_") %>%
      str_replace_all(.," ","_")
    
    ggsave(corr_name,device = 'jpeg', width = 3.72, height = 3.72)
    
    output <- list(density     = density_plot,
                   line        = line_plot,
                   correlation = corr_plot)
    
    return(output)
    
  }

# Export Charts for a Specified Range of Rows ----------------------------
  options(warn=-1)
  
  # Filter from most correlated rows to run and save plots. These will be in the export folder.
  rows_to_run <- 1:8
  
  pairs_to_plot <- most_correlated %>% 
    select(crop,pair,min_date,max_date,rank) %>%
    slice(rows_to_run)
  
  # This will save the charts as pdf in the 'export' folder
  charts <- pmap(pairs_to_plot,plot_pairs)
  
  # View plots
  charts[[1]]['density']
  charts[[1]]['line']
  charts[[1]]['correlation']
