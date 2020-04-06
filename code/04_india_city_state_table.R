# Information -------------------------------------------------------------
# Description: Analyzes Indian state average rice prices compared to national average
# Shows use cases for tile plot and R Leaflet mapping
# Written by: Drew DiPrinzio
# Last Updated: 4/6/2020

# Libraries ---------------------------------------------------------------
library(rvest)
library(tidyverse)
library(ggplot2)
library(geojsonio)
library(leaflet)
# The following packages are also each used once in plotting.
# You should ensure they are installed: 
## RColorBrewer
## htmltools
## htmlwidgets

# Import India City to State Table from Wikipedia ---------------------------
  india_state_url <- "https://en.wikipedia.org/wiki/List_of_cities_in_India_by_population"
  
  temp <- india_state_url %>% 
    read_html %>%
    html_nodes("table")
  
  city_state_table <- html_table(temp[1])[[1]] %>%
    select_all(.,tolower) %>%
    select_all(.,~str_replace_all(.x," ","_")) %>%
    select(city,state_or_union_territory) %>%
    mutate(city = case_when(city == 'Bangalore' ~ 'Bengaluru',
                            city == 'Aizawl' ~ 'Aizwal',
                            city == 'Bhubaneswar' ~ 'Bhubaneshwar',
                            city == 'Thiruvananthapuram' ~ 'Tiruvanantapuram',
                            city == 'Pondicherry' ~ 'Puducherry',
                            grepl('Hubli',city) ~ 'Dharwad',
                            city == 'Vijayawada' ~ 'Vijaywada',
                            city == 'Kozhidoke' ~ 'Kozhikode',
                            TRUE ~ city)) %>%
    rename(state=state_or_union_territory) %>%
    mutate(city = str_replace_all(city,'\\[|\\]|[0-9]',""))
  
  # Add states which are not represented in table
  cities <- c('Shillong','Panaji','Hisar','Ernakulam','Dimapur','Itanagar','Mandi','Kohima','Port Blair')
  states <- c('Meghalaya','Goa','Haryana','Kerala','Nagaland','Arunachal Pradesh','Himachal Pradesh','Nagaland','Andaman and Nicobar Islands')
  
  cities_to_add <- tibble(city = cities, state = states)
  
  city_state_table <- rbind(city_state_table,cities_to_add)

# Read in WFP Data saved as RDS in step 1 ---------------------------------
india_fp_data <- readRDS('data/datasets/fp_data.rds') %>% 
  filter(country_name == 'Bassas da India') %>%
  mutate(mkt_name = case_when(mkt_name == 'T.Puram' ~ 'Tiruvanantapuram',
                              mkt_name == 'Kozhidoke' ~ 'Kozhikode',            
                              mkt_name == 'Thiruchirapalli' ~ 'Tiruchirappalli',
                              TRUE ~ mkt_name)) %>%
  left_join(city_state_table,by=c('mkt_name'='city')) %>%
  mutate(state = ifelse(mkt_name == 'National Average','National Average',state))
  
  # Should return zero if states are properly mapped
  test <- india_fp_data %>% filter(is.na(state)) %>% count(mkt_name)

# Summary -----------------------------------------------------------------
india_crops <-india_fp_data %>% 
  count(cm_name,sort = T)

india_markets <- india_fp_data %>%
  count(mkt_name, sort = T)

india_states <- india_fp_data %>%
  count(state, sort = T)

# Generate national average, state average, and z-scores of prices -------

  # Subset india data table -------------------------------------------
  india_rice <- india_fp_data %>% 
    filter(cm_name == 'Rice - Retail') %>%
    select(date,state,mkt_name,new_price)

  # Create national average if it doesn't exist for date --------------
  india_rice <- india_rice %>%
    group_by(date) %>%
      mutate(ntl_avg = case_when(
        max(--(mkt_name == 'National Average')) == 1 ~ (max(--(mkt_name == 'National Average'))) * new_price,
        TRUE ~ mean(new_price))) %>%
    ungroup() %>%
    filter(mkt_name != 'National Average')
  
  # Create state average ----------------------------------------------------
  state_avg <- india_rice %>% 
    group_by(state,date) %>%
    mutate(state_avg = mean(new_price),
           num_markets=n()) %>%
    ungroup()

  # Find Z-Score (Used for tile plot) ---------------------------------------
  scores <- state_avg %>%
    distinct(date,state,.keep_all = T) %>%
    select(date,state,ntl_avg,state_avg) %>%
    group_by(date) %>%
    mutate(stddev=sd(state_avg)) %>%
    ungroup() %>%
    mutate(score = (state_avg - ntl_avg)/stddev)

  # Rank Each State by Number of Above Average and Below Average (used for Leaflet map) ------
  rank <- scores %>%
    mutate(rank = ifelse(score<0,-1,1)) %>%
    group_by(state) %>%
    summarise(rank=sum(rank)) %>%
    arrange(desc(rank))

# Plotting ----------------------------------------------------------------

  # Create tile chart -------------------------------------------------------
  tile <- ggplot(data = scores, 
                 mapping = aes(x = date,y = state,fill = score)) +
    geom_tile(na.rm = T,width=31) +
    scale_fill_gradient(name = "Standard Deviations from\nNational Average Price\n(Z-Score)",
                        low = "#F7AC29",
                        high = "#481C71") +
    scale_x_date(date_breaks = '2 years',date_labels = '%Y') +
    scale_y_discrete(limits = rev(levels(as.factor(scores$state)))) +
    labs(title = "Average Monthly Rice Prices in Indian States Compared to National Average",
         subtitle = "1994 - 2020",
         x = "Month",
         y = "State") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          panel.background = element_rect(linetype = 'blank',fill='white'),
          panel.border = element_rect(color = 'black',fill = NA))


  #View chart  
  tile
  
  #Save chart
  ggsave('data/monthly_prices_compared_to_national_average.jpg',device = 'jpeg', width = 14, height = 8.5)
  
  # Create leaflet ----------------------------------------------------------
  
  # Json data downloaded from this repository as a txt file, and saved with the .json extension:
  # https://github.com/Subhash9325/GeoJson-Data-of-Indian-States/blob/master/Indian_States
  # Unfortunately, this is missing Telangana (which became a state recently), but will update this in future.
  data <- geojsonio::geojson_read('data/Indian_States.json',what='sp')
  
  names_for_join <- tibble(state=data[["NAME_1"]]) %>%
    mutate(state= case_when(state=='Andaman and Nicobar' ~ 'Andaman and Nicobar Islands',
                            state=='Orissa' ~ 'Odisha',
                            TRUE ~ state))

  joined_data <- left_join(names_for_join,rank,by='state')
  
  data[["rank"]] <- joined_data$rank
  
  labels <- sprintf(
    "<strong>%s Rank:</strong><br/>%g",
    data$NAME_1, data$rank
  ) %>% lapply(htmltools::HTML)
  
  # This creates the map & legend
  rank_map <- leaflet(data) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorQuantile("Purples", rank,n = 5,na.color = "FFFFFF")(rank),
                highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                label = labels) %>%
    addLegend("bottomright", 
              # pal = colorQuantile("Purples",data$rank,n = 5), 
              values = ~rank,
              bins = 5,
              na.label = 'NA',
              colors = RColorBrewer::brewer.pal(5,'Purples'),
              labels = c('Mostly Below','Sometimes Below','Average','Sometimes Above','Mostly Above'),
              title = "Indian State Rice Prices from 1994-2020<br/>States Ranked by Months Below or Above National Average Price",
              opacity = 1)
  
  rank_map  
  
  # Save html widget as self-contained document
  htmlwidgets::saveWidget(rank_map, file = "state_ranks.html",selfcontained = T)
  
  