library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)

#Setting up my page structure.

# Only three things go here. 1. Static text like p() and h2(). 2. Inputs like text box and sliders which the user interacts with. 3. Display

country.names <- c("Somalia", "China", "Afghanistan")

shinyUI(
    navbarPage(theme = shinytheme("cyborg"),
               "Climate Colonialism",
               tabPanel(
                   "About",
                   p(),
                   h4("Welcome to my Gov 50 final project! My name is Winona, and I am a junior at Harvard College. 
                      This semester, I was interested in understanding the relationship between empire and climate risk. 
                      Over the past few centuries, colonialism has violently affected countries across the world. 
                      Today, alongside ongoing relationships of dependence and domination, the effects of colonial encounters persist. 
                      I would like to know if climate risk is among them.
                      To do so, I will take three separate sets of data: 
                      on countries in Asia and Africa impacted by colonialism, the CO2 emissions of countries, and the projected climate risk of countries."),
                   h4(a("This is where I found data about climate risk.", href = "https://www.cgdev.org/publication/dataset-vulnerability-climate-change")),
                   h4(a("This is where I found data about colonialism in Asia and Africa.", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UQZFYA"))
               ),
              
               tabPanel(
                   "Mapping Global Empires", 
                   mainPanel(
                   plotOutput("colonialPlot")
                   )
                  ),
               
               tabPanel(
                 "Interactive Activity", 
                 sidebarLayout(
                   sidebarPanel("Choose a country.",
                                p("As I continue working on my project, different options you select will go to more information about each country."),
                                selectInput(inputId = "selected_country",                  # a name for the value you choose here
                                            label = "Choose a country from this list!",    # the name to display on the slider
                                            choices = country.names)),
                   mainPanel(
                     textOutput("country_message")
                   )
                  )
                )
          )
)



