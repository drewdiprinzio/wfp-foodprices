# Information -------------------------------------------------------------

# Written by: Drew DiPrinzio
# Last Updated: 3/31/2020

# Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)

# Create export folder --------------------------------------------------
if(!dir.exists('export')){dir.create('export')}
if(!dir.exists('export/country_pairs')){dir.create('export/country_pairs')}

# Import fp_data ----------------------------------------------------------
fp_data <- readRDS('data/datasets/fp_data.rds')
pairwise_inputs <- readRDS('data/datasets/pairwise_inputs.rds')

# Create Generalized Function for Correlation Calculation -----------------

  #' param crop String. Crop with at least two overlapping countries.
  #' param pair String. Pair of countries for a specific crop, delimited by ";". 
  #' param min_corr A double. This is the minimum correlation for which an country-crop pair will be saved down for further analysis.
  run_summary_stats <- function(crop,pair,min_corr = 0.4){
      
    print(paste0('running ',crop,' for: ',pair))
    
    # 1. Initialize Variables
      country_list <- str_split(pair,";")[[1]]
      country1 <- country_list[1]
      country2 <- country_list[2]
      crop_name <- crop %>% tolower() %>% str_replace_all('[ -]',"_") %>% str_remove_all('[()]')
      log <- ''
    
    # 2. Filter full dataset to the specific crop and country-pair
      filtered_data <- fp_data %>%
        filter(country_name %in% country_list, cm_name == crop) %>%
        select(date,country_name,cm_name,mkt_name,new_price,new_unit)
      
    # 3. Check crop units for each country
      units <- filtered_data %>% count(country_name,new_unit)
      country1_units <- units %>% filter(country_name == country1) %>% pull(new_unit) %>% paste0(collapse = '; ')
      country2_units <- units %>% filter(country_name == country2) %>% pull(new_unit) %>% paste0(collapse = '; ')
      
      #creates a warning if the units are inconsistent, but continues analysis
      if(nrow(units %>% distinct(new_unit))>1) {log = paste0(log,'Inconsistent Units')}
    
    # 4. Keep "national average" price if given in data, or take average across markets
      analysis_table <- filtered_data %>%
        group_by(country_name,date) %>%
        mutate(is_national_average = ifelse(mkt_name == 'National Average',1,0),
               has_national_average = max(is_national_average)) %>%
        ungroup() %>%
        filter(!(has_national_average == 1 & is_national_average == 0)) %>%
        group_by(country_name,date) %>%
        summarise(avg_price = mean(new_price)) %>%
        ungroup() %>%
        arrange(country_name,date)
  
    # 5. Add price return and index price variables for correlation calculation and plots
      returns <- analysis_table %>%
        group_by(country_name) %>% 
        mutate(return = (avg_price - lag(avg_price))/lag(avg_price),
               return_for_calc = ifelse(is.na(return),1,1+return),
               min_date = min(date)) %>%
        ungroup() %>%
        filter(date >= max(min_date)) %>%
        group_by(country_name) %>% 
        mutate(index  = case_when(date == min(date) ~ 1,
                                  TRUE ~  1 * cumprod(return_for_calc))) %>%
        ungroup() %>%
        select(date,country_name,avg_price,return,index)
    
    # 6. Filter to overlapping observations to run returns correlation
      correlation <- returns %>%
        select(date,country_name,return) %>%
        spread(country_name,return) %>%
        filter(!is.na(get(country1)) & !is.na(get(country2))) 
    
    # 7. Create summary statistics
      min_date <- min(correlation$date)
      max_date <- max(correlation$date)
      overlapping_obs <- nrow(correlation)
      correlation <- cor(correlation[,country1],correlation[,country2],use = 'pairwise.complete.obs',method='pearson')
      
      # Save rds to 'country_pairs' folder for pairs which have a correlation >= min_corr. These are used for plotting. 
      # If you want to save all RDS, you can set min_corr argument to -1.
        if (correlation >= min_corr) {
          rds_name <- paste0('export/country_pairs/',tolower(country1),"_",tolower(country2),'_',tolower(crop_name),'_returns.rds') %>%
            str_replace_all(.,"___","_") %>%
            str_replace_all(.," ","_")
          print(glue('saving {rds_name}'))
          saveRDS(returns,rds_name)
        }
    
    # 8. Create result row
      result <- tibble(crop            = crop,
                       pair            = pair,
                       min_date        = min_date,
                       max_date        = max_date,
                       overlapping_obs = overlapping_obs,
                       correlation     = correlation,
                       country1_units  = country1_units,
                       country2_units  = country2_units,
                       log             = log)
   
    return(result)
  }
  
  # Run Summary Statistics Function to Create Correlation Table -------------
# This takes about 1 minute to run ~300 inputs, and sort correlations in descending order
  
pairwise_results <- pmap_dfr(pairwise_inputs,run_summary_stats, min_corr = 0.4) %>%
  arrange(desc(correlation))

View(pairwise_results)


# Filter to Most Correlated Crops and save csv ----------------------------
  most_correlated <- pairwise_results %>% 
    filter(correlation >= 0.4) %>%
    mutate(rank=row_number())
  
  top_crops <- most_correlated %>%
    count(crop) %>%
    arrange(desc(n))
  
  # Save as csv and rds
  write.csv(most_correlated,'export/most_correlated_pairs.csv',row.names = F)
  saveRDS(most_correlated,'data/datasets/most_correlated_pairs.rds')