library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)

colonialism <- read_xls("raw_data/colonialism.xls") %>%
  clean_names() 

# Adding my first graph.

shinyServer(function(input, output) {

    output$colonialPlot <- renderPlot({
    
    colonialism %>%
        filter(country_name %in% c("Afghanistan", "China", "Angola")) %>%
        ggplot(mapping = aes(x = country_name, y = colyears)) + 
            geom_col()
    })

})


