#### PREP ####

library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)
library(leaflet)
library(maptools)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(tidymodels)
library(rstanarm)
library(broom.mixed)

colonialism <- read_xls("raw_data/colonialism.xls") %>%
  clean_names() 

climaterisk <- read_xls("raw_data/vulnerability.xls", skip = 2) %>%
  clean_names() %>%
  rename(vulnerable = climate_vulnerability_cv_cdi_adj_for_income_regulation) %>%
  rename(governance = kkm_regulatory_quality_score_2008) %>%
  rename(income = income_per_capita_us_ppp_2008) %>%
  rename(area = area_sq_km) %>%
  mutate(country = str_to_title(country)) %>%
  select(country, vulnerable, income, governance, area, world_sub_region) %>%
  mutate(country = recode(country, 'Korea, Rep.' = 'Korea, Republic of',
                          "Korea, Dem. Rep." = "Korea, Democratic People's Republic of",
                          'Taiwan (China)' = 'Taiwan',
                          'Antigua And Barbuda' = 'Antigua and Barbuda',
                          'Bahamas, The' = 'Bahamas',
                          'Slovak Republic' = 'Slovakia',
                          
                          # Starting here are the ones I am renaming so that it aligns with the wrldsimplmod countries' names. 
                          
                          'Iran, Islamic Rep.' = 'Iran (Islamic Republic of)',
                          'Bosnia And Herzegovina' = 'Bosnia and Herzegovina',
                          'Myanmar' = 'Burma',
                          'Congo, Rep.' = 'Congo',
                          'Congo, Dem. Rep.' = 'Democratic Republic of the Congo',
                          'Egypt, Arab Rep.' = 'Egypt',
                          'Yemen, Rep.' = 'Yemen',
                          'French Guians' = 'French Guiana',
                          'Micronesia, Fed. Sts.' = 'Micronesia, Federated States of',
                          'Gambia, The' = 'Gambia',
                          "Cote D'ivoire" = "Cote d'Ivoire",
                          'Kyrgyz Republic' = 'Kyrgyzstan',
                          'Falkland Islands' = 'Falkland Islands (Malvinas)',
                          "Lao Pdr" = "Lao People's Democratic Republic",
                          'Libya' = 'Libyan Arab Jamahiriya',
                          'Macedonia, Fyr' = 'The former Yugoslav Republic of Macedonia',
                          'Hong Kong Sar, China' = 'Hong Kong',
                          'Isle Of Man' = 'Isle of Man',
                          'Macao Sar, China' = 'Macau',
                          'West Bank And Gaza' = 'Palestine',
                          
                          # I feel somewhat like I am simplifying decades of history into one line of code!
                          
                          'Serbia And Montenegro' = 'Serbia',
                          'Moldova' = 'Republic of Moldova',
                          'Russian Federation' = 'Russia',
                          'St. Kitts And Nevis' = 'Saint Kitts and Nevis',
                          'St. Lucia' = 'Saint Lucia',
                          'Trinidad And Tobago' = 'Trinidad and Tobago',
                          'Sao Tome And Principe' = 'Sao Tome and Principe',
                          'Tanzania' = 'United Republic of Tanzania',
                          'St. Vincent And The Grenadines' = 'Saint Vincent and the Grenadines',
                          'Venezuela, Rb' = 'Venezuela',
                          'Vietnam' = 'Viet Nam',
                          'Virgin Islands (U.s.)' = 'United States Virgin Islands',
                          'Wallis And Futuna' = 'Wallis and Futuna Islands',
                          'Pitcairn' = 'Pitcairn Islands',
                          'St. Pierre And Miquelon' = 'Saint Pierre and Miquelon',
                          'St. Helena' = 'Saint Helena',
                          'Turks And Caicos Islands' = 'Turks and Caicos Islands',
                          'Svalbard And Jan Mayen' = 'Svalbard')) %>%
  mutate(vulnerable = replace(vulnerable, 188, 42.000))

# BIG IMPORTANT NOTE: I replaced Somalia's 100.000 climate risk rate with 42.000
# (near Burma's), because it is a single dramatic outlier that changes the later
# graph. I will be sure to note this via text inside my Shiny app!

# Mapping these data sets (and doing the corresponding recoding work) requires
# navigating a lot of complex history that I'm having difficulty summarizing
# into one number. For instance, in the countries data, we have Yemen listed as
# being decolonized in 1990, but Yemen People’s Republic decolonized in 1967. In
# the climate data, there is one one 'Yemen Rep.' Which name maps on to which? I
# ended up choosing to rename Yemen Rep. as Yemen, both to match the later
# colonization date and also to match wrldsimplmod.

countries <- read_xls("raw_data/countries_files/icowcol.xls") %>%
  mutate(date = str_sub(Indep, 1, 4)) %>%
  mutate(twenty = ifelse(date >= 1900, "Colonized", "Independent")) %>%
  mutate(Name = recode(Name, 'United States of America' = 'United States',
                       'Tunisia (postcolonial)' = 'Tunisia',
                       'St. Kitts and Nevis' = 'Saint Kitts and Nevis',
                       'St. Vincent and the Grenadines' ='Saint Vincent and the Grenadines',
                       'Prussia / Germany' = 'Germany',
                       'Fed. States of Micronesia' = 'Micronesia, Federated States of',
                       'Iran' = 'Iran (Islamic Republic of)',
                       'Brunei' = 'Brunei Darussalam',
                       'Austria-Hungary' = 'Austria',
                       "Laos" = "Lao People's Democratic Republic",
                       'Egypt (poat-colonial)' = 'Egypt',
                       'South Korea' = 'Korea, Republic of',
                       "Korea, Dem. Rep." = "Korea, Democratic People's Republic of",
                       'Moldova' = 'Republic of Moldova',
                       'Serbia / Yugoslavia' = 'Serbia',
                       'Tanzania' = 'United Republic of Tanzania',
                       'Vietnam' = 'Viet Nam'))

emissions <- read_csv("raw_data/emissions.csv", 
                      col_types = cols(
                        Year = col_double(),
                        Country = col_character(),
                        Total = col_double(),
                        `Solid Fuel` = col_double(),
                        `Liquid Fuel` = col_double(),
                        `Gas Fuel` = col_double(),
                        Cement = col_double(),
                        `Gas Flaring` = col_double(),
                        `Per Capita` = col_double(),
                        `Bunker fuels (Not in Total)` = col_double()
                      )) %>%
  group_by(Country) %>%
  mutate(sum = sum(Total)) %>%
  slice(1) %>%
  select(Country, sum) %>%
  mutate(Country = str_to_title(Country)) %>%
  mutate(Country = recode(Country, "United States Of America" = "United States",
                          "China (Mainland)" = "China",
                          "Russian Federation" = "Russia",
                          "Islamic Republic Of Iran" = "Iran (Islamic Republic of)",
                          "Plurinational State Of Bolivia" = "Bolivia",
                          "Myanmar (Formerly Burma)" = "Burma",
                          "United Republic Of Tanzania" = "United Republic of Tanzania",
                          "Libyan Arab Jamahiriyah" = "Libyan Arab Jamahiriya",
                          "French West Africa" = "Western Sahara",
                          "Cote D Ivoire" = "Cote d'Ivoire",
                          "Republic Of Cameroon" = "Cameroon",
                          "Democratic Republic Of The Congo (Formerly Zaire)" = "Democratic Republic of the Congo",
                          "United Republic Of Tanzania" = "United Republic of Tanzania",
                          "Lao People S Democratic Republic" = "Lao People's Democratic Republic",
                          "Republic Of Korea" = "Korea, Republic of", 
                          "Democratic People S Republic Of Korea" = "Korea, Democratic People's Republic of",
                          "Antarctic Fisheries" = "Antarctica",
                          "France (Including Monaco)" = "France",
                          "Italy (Including San Marino)" = "Italy")) %>%
  mutate(sum = ifelse(sum == 102510260, 60000000, sum))

# BIG IMPORTANT NOTE AGAIN: I replaced the U.S. total emissions of 102510260, a dramatic outlier, to be 60,000,000, closer to the second largest # of 47649834 (China). 

# The below joins the colonialism data with climate risk data. 

fit_mod <- inner_join(countries, climaterisk, by = c("Name" = "country")) %>%
  group_by(twenty) %>%
  mutate(avg_risk = mean(vulnerable)) 

wrldsimplmod <- readRDS("joined.rds")

secondmod <- readRDS("joined2.rds")

model <- inner_join(emissions, climaterisk, by = c("Country" = "country"))

reg <- stan_glm(formula = vulnerable ~ sum + income + area + governance,
                data = model, 
                refresh = 0, 
                seed = 8) %>%
  tidy()


#### SETTING UP SHINY SERVER ####

shinyServer(function(input, output) {
  
#### FIRST PAGE ####

  output$countriesPlot <- renderPlot({
    
    bar_graph <- fit_mod %>%
      select(twenty, avg_risk) %>%
      slice(1) %>%
      ggplot(mapping = aes(x = twenty, y = avg_risk)) + 
      geom_col(fill = "darkgreen") +
      labs(title = "Climate Risk Today for Countries Colonized vs. Independent", 
           subtitle = "At The Turn of the 20th Century", 
           x = "Countries' Colonial Status in 1900",
           y = "Average Climate Vulnerability Today",
           caption = "The average climate risk among 131 colonized countries is 4.58, 
      and the average for 45 independent countries is 1.52.") +
      theme_classic()
    
    bar_graph
    
  })
  
#### SECOND PAGE ####  
  
    output$colonialPlot <- renderPlot({
    
      if (input$selected_characteristic == "Years Colonized") {
        
        #display first line plot
        
      q1 <- colonialism %>%
        select(country_name, colyears) 
      
      q2 <- climaterisk %>%
        subset(country != "Somalia") %>%
        subset(country != "Burundi")
      
      q3 <- inner_join(q1, q2, by = c("country_name" = "country")) 
      
      colonize <- q3 %>%
        ggplot(aes(x = colyears, y = vulnerable, color = world_sub_region)) +
        geom_point(alpha = 0.7) +
        geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
        labs(title = "Countries by Years Colonized and Vulnerability to Climate Risk",
             x = "Years Colonized",
             y = "Vulnerability to Climate Risk",
             caption = "This graph indicates there is not a significant correlation
       overall between the years a country has been colonized, 
       and its current vulnerability to climate risk.") +
        theme_bw() + 
        scale_color_discrete("World Region")
      
      colonize
      }
      
    
    else {
      if(input$selected_characteristic == "Colonization Type") {
        
       
        types <- fit_mod %>%
          ggplot(aes(x = Type, y = vulnerable)) +
          geom_col(fill = "orange") +
          labs(title = "The Effect of Different Types of Colonialism on Climate Risk",
               y = "Climate Risk",
               subtitle = "Key: 1 = Formation, 2 = Decolonization, 3 = Secession, 4 = Partition",
               caption = "This graph shows that decolonialization and occuption by a foreign power 
           has a significantly greater effect on risk than histories of secession or partition.") 
        
        types
        
      }
      
      else {
        
        types <- fit_mod %>%
          ggplot(aes(x = Type, y = vulnerable)) +
          geom_col(fill = "orange") +
          labs(title = "The Effect of Different Types of Colonialism on Climate Risk",
               y = "Climate Risk",
               subtitle = "Key: 1 = Formation, 2 = Decolonization, 3 = Secession, 4 = Partition",
               caption = "This graph shows that decolonialization and occuption by a foreign power 
           has a significantly greater effect on risk than histories of secession or partition.") 
        
        types
        
      }
      
      
    }
      
      
    
  })
    
    
    
    

#### THIRD PAGE ####
    
# PART ONE: CLIMATE RISK MAP
    
    # Table with names of countries, sorted by climate risk.
    
    output$risk <- renderDataTable({
      datatable(
        
        selection <- climaterisk %>%
          select(country, income, vulnerable) %>%
          rename("Income Per Capita" = "income",
                 "Climate Risk" = "vulnerable",
                 "Country" = "country"),
        options = list(pageLength = 10)
      )
    })
    
    # Leaflet map, with help referencing Wyatt Hurt's project on Transboundary Water
    # Conflict.
    
    output$map <- renderLeaflet({
      
      # Set color palette, using 9 bins.
      
      bins_climaterisk <-
        colorBin(
          "Blues",
          wrldsimplmod$vulnerable,
          9,
          
          # I would like to reconfigure if Somalia (an extreme outlier) were temporarily taken out. It dramatically changes the colors on the map!
          
          pretty = FALSE,
          na.color = "#DFDFDF"
        )
      
      # Build map using CARTO DB Positron provider tiles. View middle of earth.
      
      leaflet(wrldsimplmod, width = "100%") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0,
                lat = 30,
                zoom = 1.5) %>%
        
        # Adding polygons.
        
        addPolygons(
          stroke = FALSE,
          smoothFactor = 0.2,
          fillOpacity = 1,
          popup = paste(
            wrldsimplmod$NAME,
            "<br>",
            "Climate Risk:",
            wrldsimplmod$vulnerable
          ),
          color = ~ bins_climaterisk(wrldsimplmod$vulnerable)
        ) %>%
        
        # The map legend.
        
        addLegend(
          "bottomright",
          pal = bins_climaterisk,
          values = wrldsimplmod$vulnerable,
          title = "Climate Risk",
          opacity = 1,
          labFormat = labelFormat(digits = 0)
        )
    }) 
    
# PART TWO: EMISSION MAP
    
    # Table with names of countries, sorted by climate risk.
    
    output$emissions <- renderDataTable({
      datatable(
        emissions %>%
          rename("Total Emissions" = "sum"),
        options = list(pageLength = 10)
      )
    })
    
    # Leaflet map
    
    output$map_2 <- renderLeaflet({
      
      # Set color palette, using 9 bins.
      
      bins_emissions <-
        colorBin(
          "Reds",
          secondmod$sum,
          9,
          pretty = FALSE,
          na.color = "#DFDFDF"
        )
      
      # Build map using CARTO DB Positron provider tiles. View middle of earth.
      
      leaflet(secondmod, width = "100%") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0,
                lat = 30,
                zoom = 1.5) %>%
        
        # Adding polygons.
        
        addPolygons(
          stroke = FALSE,
          smoothFactor = 0.2,
          fillOpacity = 1,
          popup = paste(
            secondmod$NAME,
            "<br>",
            "Emissions:",
            secondmod$sum
          ),
          color = ~ bins_emissions(secondmod$sum)
        ) %>%
        
        # The map legend. 
        
        addLegend(
          "bottomright",
          pal = bins_emissions,
          values = secondmod$sum,
          title = "Total Emissions",
          opacity = 1,
          labFormat = labelFormat(digits = 0)
        )
    })
    
#### FOURTH PAGE ####

# Running my model.
    
    # Is there a predictive relationship between CO2 emissions and climate risk?
    
    # Trying to produce total emissions for each country between 1751 and 2014. 
    
    output$emissionsPlot <- renderPlot({
          
          emissions_risk <- model %>%
            ggplot(aes(x = sum, y = vulnerable)) +
            geom_line()
          
          emissions_risk
      
    })
    
    
    # I am also controlling for three variables from each country here: income per capita, area, and governance quality. This data was included inside the climate risk dataset.
    
    output$modelPlot <- renderTable({
      
      reg
      
    })
    
#### FIFTH PAGE ####
    
# This is my About page, and I only have text-based elements!
    
#### CLOSING OUT SHINY ####
    
})

