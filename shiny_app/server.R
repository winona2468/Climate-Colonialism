library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)

colonialism <- read_xls("raw_data/colonialism.xls") %>%
  clean_names() 

vulnerability <- read_xls("raw_data/vulnerability.xls", skip = 2) %>%
  clean_names() %>%
  rename(vulnerable = climate_vulnerability_cv_cdi_adj_for_income_regulation)

#getwd(), setwd()

shinyServer(function(input, output) {

    output$colonialPlot <- renderPlot({
    
      q1 <- colonialism %>%
        select(country_name, colyears) 
      
      q2 <- vulnerability %>%
        select(country, vulnerable, world_sub_region)
      
      q3 <- inner_join(q1, q2, by = c("country_name" = "country")) 
      
      graph1 <- q3 %>%
        ggplot(aes(x = colyears, y = vulnerable, color = world_sub_region)) +
        geom_point(alpha = 0.7) +
        labs(title = "Countries by Years Colonized and Vulnerability to Climate Risk",
             x = "Years Colonized",
             y = "Vulnerability to Climate Risk",
             caption = "This graph indicates there is not a significant correlation
       overall between the years a country has been colonized, 
       and its current vulnerability to climate risk.") +
        theme_bw() + 
        scale_color_discrete("World Region")
      
      graph1
      
    })
    
    output$country_message <- renderText({
      paste0("This is the country you chose: ", 
             input$selected_country, "!")
})
    
})

