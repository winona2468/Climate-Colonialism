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

colonialism <- read_xls("raw_data/colonialism.xls") %>%
  clean_names()

climaterisk <- read_xls("raw_data/vulnerability.xls", skip = 2) %>%
  clean_names() %>%
  rename(vulnerable = climate_vulnerability_cv_cdi_adj_for_income_regulation) %>%
  select(country, vulnerable, world_sub_region)

countries <- read_xls("raw_data/countries_files/icowcol.xls") %>%
  mutate(date = str_sub(Indep, 1, 4)) %>%
  mutate(twenty = ifelse(date >= 1900, "Colonized", "Independent"))

wrldsimplmod <- readRDS("joined.RDS")

fit_mod <- inner_join(countries, climaterisk, by = c("Name" = "country")) %>%
  group_by(twenty) %>%
  mutate(avg_risk = mean(vulnerable)) 

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
  summarize(sum = sum(Total), .groups = "drop") %>%
  select(Country, sum) %>%
  mutate(Country = str_to_title(Country)) 


#### SETTING UP SHINY SERVER ####

shinyServer(function(input, output) {
  
#### FIRST PAGE ####

  output$countriesPlot <- renderPlot({
    
    bargraph <- fit_mod %>%
      ggplot(mapping = aes(x = twenty, y = avg_risk)) + 
      geom_col(fill = "darkseagreen4") +
      theme_bw() +
      labs(title = "Climate Risk Today for Countries Independent vs. Colonized At The Turn of the 20th Century",
           x = "Countries (Until Decolonization Post-1900)",
           y = "Average Climate Vulnerability Today") +
      theme_classic()
    
    bargraph
    
  })
  
#### SECOND PAGE ####  
  
    output$colonialPlot <- renderPlot({
    
      if (input$selected_characteristic == "Years Colonized") ({
        
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
      })
      
    
    else(input$selected_characteristic == "Type of Colonization") ({
      
    types <- fit_mod %>%
      ggplot(aes(x = Type, y = vulnerable)) +
      geom_col(fill = "orange") +
      labs(title = "The Effect of Different Types of Colonialism on Climate Risk",
           y = "Climate Risk",
           subtitle = "Key: 1 = Formation, 2 = Decolonization, 3 = Secession, 4 = Partition",
           caption = "This graph shows that decolonialization and occuption by a foreign power 
           has a significantly greater effect on risk than histories of secession or partition.") 
    
    types
    
    })
    
  })
    
    output$message <- renderText({
      paste0("More information: ", 
             input$selected_characteristic, "!")
    })
    
#### THIRD PAGE ####
    
# PART ONE: CLIMATE RISK MAP
    
    # Table with names of countries, sorted by climate risk.
    
    output$risk <- renderDataTable({
      datatable(
        
        selection <- fit_mod %>%
          select(Name, date, twenty, vulnerable) %>%
          rename("Start of 20th Century" = "twenty",
                 "Year Decolonized" = "date",
                 "Climate Risk" = "vulnerable",
                 "Country" = "Name"),
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
          fit_mod$vulnerable,
          9,
          pretty = FALSE,
          na.color = "#DFDFDF"
        )
      
      # Build map using CARTO DB Positron provider tiles. View middle of earth.
      
      leaflet(width = "100%") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0,
                lat = 30,
                zoom = 1.5) %>%
        
        # Adding polygons.
        
        addPolygons(
          data = wrldsimplmod,
          stroke = FALSE,
          smoothFactor = 0.2,
          fillOpacity = 1,
          popup = paste(
            fit_mod$Name,
            "Country <br>",
            "Climate Risk:",
            fit_mod$vulnerable
          ),
          color = ~ bins_climaterisk(fit_mod$vulnerable)
        ) %>%
        
        # The map legend. 
        
        addLegend(
          "bottomright",
          pal = bins_climaterisk,
          values = fit_mod$vulnerable,
          title = "Climate Risk",
          opacity = 1,
          labFormat = labelFormat(digits = 0)
        )
    })
    
# PART TWO: EMISSION MAP
    
    # Table with names of countries, sorted by climate risk.
    
    output$emissions <- renderDataTable({
      datatable(
        emissions,
        options = list(pageLength = 10)
      )
    })
    
    # Leaflet map
    
    output$map_2 <- renderLeaflet({
      
      # Set color palette, using 9 bins.
      
      bins_emissions <-
        colorBin(
          "Reds",
          emissions$sum,
          9,
          pretty = FALSE,
          na.color = "#DFDFDF"
        )
      
      # Build map using CARTO DB Positron provider tiles. View middle of earth.
      
      leaflet(width = "100%") %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0,
                lat = 30,
                zoom = 1.5) %>%
        
        # Adding polygons.
        
        addPolygons(
          data = wrldsimplmod,
          stroke = FALSE,
          smoothFactor = 0.2,
          fillOpacity = 1,
          popup = paste(
            emissions$Country,
            "Country <br>",
            "Emissions:",
            emissions$sum
          ),
          color = ~ bins_emissions(emissions$sum)
        ) %>%
        
        # The map legend. 
        
        addLegend(
          "bottomright",
          pal = bins_emissions,
          values = emissions$sum,
          title = "Total Emissions",
          opacity = 1,
          labFormat = labelFormat(digits = 0)
        )
    })
    
#### FOURTH PAGE ####

# Running my model.
    
#### FIFTH PAGE ####
    
# This is my About page, and I only have text-based elements!
    
#### CLOSING OUT SHINY ####
    
})

