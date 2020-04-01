# Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(glue)

# Import data -------------------------------------------------------------

  # Download the file and save to a 'data/import/' folder in your working directory
  # https://data.humdata.org/dataset/wfp-food-prices
  import_file <- read.csv('data/import/wfpvam_foodprices.csv',stringsAsFactors = F)
  
# Add variables -----------------------------------------------------------

  # Add date variable and rename common variables
  fp_data <- import_file %>%
    mutate(date = as.Date(glue('{mp_year}-{mp_month}-01'),'%Y-%m-%d')) %>%
    rename(country_id    = adm0_id,
           country_name  = adm0_name,
           district_id   = adm1_id,
           district_name = adm1_name)
  
  rm(import_file)
  
  # Normalize prices across KGs
  kg_units <- fp_data %>%
    pull(um_name) %>%
    unique() %>%
    as.character() %>% 
    .[grepl('( G)|(KG)',.)]

  kg_conversion_table <- tibble(um_name = kg_units) %>%
                         mutate(number = as.numeric(str_remove_all(um_name,'[ a-zA-Z]')),
                                number = ifelse(is.na(number),1,number),
                                conversion_factor = case_when(
                                 grepl('KG',um_name) ~ 1 / number,
                                 TRUE ~ 1000 / number),
                                new_unit = 'KG') %>%
                         select(um_name,conversion_factor,new_unit)
  
  fp_data <- left_join(fp_data,kg_conversion_table,by='um_name') %>%
             mutate(new_unit  = ifelse(is.na(new_unit),um_name,new_unit),
                    conversion_factor = ifelse(is.na(conversion_factor),1,conversion_factor),
                    new_price = mp_price*conversion_factor)
  
  if(!dir.exists('data/datasets')){dir.create('data/datasets')}
  saveRDS(fp_data,'data/datasets/fp_data.rds')
  
  
# Summary functions -------------------------------------------------------

  # Summary of observations for each country by year
  summary <- fp_data %>% 
    count(country_name,mp_year) %>%
    spread(mp_year,n)
  
# Build Input Table of Country-Crop Pairs --------------------------------

  months_of_data_min <- 175
  
  # A. Filter to country-crop combinations with at least 175 months of price data for a crop
  country_crop_year <- fp_data %>%
    distinct(country_name,cm_name,date) %>%
    count(country_name,cm_name,name = 'count_dates') %>%
    arrange(desc(count_dates)) %>%
    filter(count_dates >= months_of_data_min)

  # B. Keep commodities for which at least two countries have >= 175 months of data
  country_crop_year <- country_crop_year %>%
    group_by(cm_name) %>%
    mutate(country_count = n()) %>%
    ungroup() %>%
    arrange(desc(country_count),cm_name) %>%
    filter(country_count > 1) 

  # C. Create table with crop and list of countries separated by ';'
  country_crop_groups <- country_crop_year %>%
    distinct(cm_name,country_count) %>%
    mutate(country_list = rep('init'))
  
  for (i in 1:nrow(country_crop_groups)){
    country_crop_groups$country_list[i] <- country_crop_year %>%
      filter(cm_name == country_crop_groups$cm_name[i]) %>%
      pull(country_name) %>% 
      paste(collapse = "; ") %>%
      as.character()
    }
  
  # D. Define 'generate_pairs' function
  # The 'generate_pairs' function creates rows of all unique country pairs for a crop.
  # The number of pairs for a list of n countries will be ((n-1)+(n-2)+...+0) 
  generate_pairs <- function(cm_name, country_list) {
    list_of_countries <- str_split(country_list,";")[[1]] %>% trimws(.)
    vec_length <- length(list_of_countries)
    output_vec <- tibble(crop = 'init', pair = 'init')
     for (j in 1:(vec_length-1)){
      for (k in (j+1):vec_length){
        inter_pair <- paste0(trimws(list_of_countries[j]),";",trimws(list_of_countries[k])) 
        output_vec <- output_vec %>% add_row(crop = cm_name, pair = inter_pair)
      } 
     }
    output_vec <- output_vec %>% filter(crop != 'init')
    return(output_vec)
  }
  
  # E. Loop generate_pairs function over country_crop_groups to create inputs for analysis
  pairwise_inputs <- country_crop_groups %>%
    select(cm_name,country_list) %>%
    pmap_dfr(.,generate_pairs)

  saveRDS(pairwise_inputs,'data/datasets/pairwise_inputs.rds')
  