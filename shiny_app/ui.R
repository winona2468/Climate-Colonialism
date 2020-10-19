library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)

#Setting up my page structure.

# Only three things go here. 1. Static text like p() and h2(). 2. Inputs like text box and sliders which the user interacts with. 3. Display

shinyUI(
    navbarPage(theme = shinytheme("cyborg"),
               "Climate Colonialism",
               tabPanel(
                   "About",
                   p("This project will analyze the relationship between empire and climate risk. I will take three separate sets of data: about countries impacted by colonialism, the CO2 emissions of countries, and the projected climate risk of countries. I hope to visualize if there is a correlation between empire and future effects of the climate.")),
                   h6(a("Here is my first dataset about climate risk.", href = "https://www.cgdev.org/publication/dataset-vulnerability-climate-change")),
               tabPanel(
                   "Mapping Global Empires", 
                   plotOutput("colonialPlot")
               )
    ))


