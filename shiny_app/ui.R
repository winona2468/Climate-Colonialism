#### PREP ####

library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readr)
library(readxl)
library(leaflet)
library(maptools)
library(DT)

#### SETTING UP THE PAGE ####

shinyUI(
    navbarPage(theme = shinytheme("journal"),
               "Climate Colonialism",

#### FIRST PAGE ####

               tabPanel(
                   "The Challenge",
                   p(),
                   h4("The empire of the climate is the first, the most powerful of all empires —Montesquieu"),
                   h2(" "),
                   
               mainPanel(
                     plotOutput("countriesPlot"),
                     h4("Why is this the case?")
                   )    
               ),

#### SECOND PAGE ####

               tabPanel(
                 "History of Empire", 
                 
                 fluidPage(
                   
                   titlePanel("A Short History"),
                   
                   p(paste("Here I will include a short historical background on the history of empire, 
                   and its relationship with climate colonialism. I will also replace the third graph in the list.")),
                   
                   sidebarLayout(
                     
                     # Left panel. 
                     
                     sidebarPanel(
                       
                       helpText("Choose a characteristic of colonialism:"),
                       
                       selectInput("selected_characteristic", h3("Characteristics"),
                                   choices = list("Years Colonized",
                                                  "Colonization Type",
                                                  "Different Environmental Risks")),
                     ),
                     
                    # Right side plot. 
                    
                      mainPanel(
                         plotOutput("colonialPlot")
                      )
                     )
                   
                   )
                 ),

#### THIRD PAGE ####

               tabPanel(
                 "Emissions vs. Effects", 

# PART ONE    
                fluidPage(
                  
                  # Two columns. 
                  
                  fluidRow(column(
                    4,
                    h3("Countries around the world will be impacted differently by the climate crisis."),
                    p("Click on the countries to the right for their projected climate risk.
                      Note: In the data, the true value for Somalia's climate risk is 100.000. 
                      As an extreme outlier, this number was changed here for the purposes of the visual."
                    ),
                    
                    # Plot table.
                    
                    dataTableOutput("risk")
                  ),
                  column(8,
                         
                         # Plot Leaflet map.
                         
                         leafletOutput("map", height = 700))
                     ),
# PART TWO                  
                  fluidRow(column(
                    4,
                    h3("But these countries are contributing to the climate crisis at very different rates."),
                    p("Click to the right for countries' CO2 emissions between 1751 and 2014.
                      Note: In the data, the true value for the United States's emissions is 102,510,260. 
                      As an extreme outlier, this number was changed here for the purposes of the visual."
                    ),
                    
                    # Plot table.
                    
                    dataTableOutput("emissions")
                  ),
                  column(8,
                         
                         # Plot Leaflet map.
                         
                         leafletOutput("map_2", height = 700))
                  )
                  
                 )

                ),
              
#### FOURTH PAGE ####

                tabPanel(
                  "Model",
                  h3("Is there a predictive relationship between a country's CO2 emissions and their climate risk?"),
                  h4("First, I have found the sum of CO2 emissions between 1751 and 2014 for various countries. 
                     Then, I ran climate risk on the sum of these emissions.
                     The graph between these two variables does not produce a clear regression line, 
                     though the general trend seems to be that countries with few emissions also have high climate risk."),
                  
                  mainPanel(
                    plotOutput("emissionsPlot"),
                    h4("To put together my model, I also controlled for three variables: income per capita, area, and governance quality. 
                     This data was included inside the climate risk dataset."),
                    tableOutput("modelPlot"),
                    h4("Governance is the only variable which seems to have a substantive effect on climate risk.")
                  ) 
                ),


#### FIFTH PAGE ####

               tabPanel(
                 "About", 
                 mainPanel(
                   h4("Welcome to my Gov 50 final project! My name is Winona, and I am a junior at Harvard College. 
                      This semester, I was interested in understanding the relationship between empire and climate risk. 
                      Over the past few centuries, colonialism has violently affected countries across the world. 
                      Today, alongside ongoing relationships of dependence and domination, the effects of colonial encounters persist. 
                      I would like to know if climate risk is among them.
                      To do so, I have taken four separate datasets: 
                        on colonial history, countries in Asia and Africa impacted by colonialism, the CO2 emissions of countries, and the projected climate risk of countries."),
                   h5("First is a 2007 dataset on colonial history from the Harvard Dataverse. 
                      This was put together by the Issue Correlates of War (ICOW) Project, 
                      which tried 'to identify colonial or other dependency relationships for each state over the past two centuries.' 
                      The dataset includes the years in which countries were decolonized, as well as characteristics like the type of independence, whether it was violent, etc."),
                   h5(a("ICOW Colonial History Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5EMETG")),
                   h5("Next is a 2017 dataset from the Harvard Dataverse which measures characteristics of colonialism specifically in countries in Africa and Asia.
                      I found this useful because it contains a column specifying the number of years countries were colonized (a variable not found in the previously mentioned dataset).
                      I was interested in whether there is a correlation between number of years colonized and climate risk, so I combined this data with climate vulnerability data for a graph on the second tab.
                      It is important to note that this dataset does not capture important regions in the world like Latin America and Europe. 
                      Nevertheless, it is still interesting to see the results in Asia and Africa, since those regions have been heavily affected by colonialism over time."),
                   h5(a("Colonialism in Asia and Africa Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UQZFYA")),
                   h5("Third is a 2011 dataset on climate risk, created by David Wheeler for the Center for Global Development. 
                      This was perhaps the most important dataset for my project, because it 'measures the vulnerability of 233 countries to three major effects of climate change: weather-related disasters, sea-level rise, and reduced agricultural productivity'.
                      One important note is that this dataset was collected in 2011. In the 9 years which have passed since then, our world and individual countries have changed in their increasing susceptibility to environmental harm. 
                      I wonder whether a dataset collected today would reflect significantly different comparative risks across countries.
                      Nevertheless, since this project discusses the effects of the colonial era (especially in the sense of political control, legal status, and physical occupation, all still going strong in the 20th century), perhaps data closer to then is still useful to see."),
                   h5(a("Climate Risk Data", href = "https://www.cgdev.org/publication/dataset-vulnerability-climate-change")),
                   h5("Last is a 2013 dataset from the Carbon Dioxide Information Analysis Center (Boden, Marland, Andres). 
                      It measures CO2 Emissions from fossil-fuels per country annually from 1751 to 2014.
                      I found this dataset on Github, prepared by the datahub.io project."),
                   h5(a("CO2 Emissions Data", href = "https://github.com/datasets/co2-fossil-by-nation")),
                   h4("Finally, I just wanted to sound three notes of caution: First, do not interpret any of the linkages I have created on this site as causal. I am interested in examining whether there is a relationship between different variables as a predictive question.
                   Second, the data I have used to map out colonial history largely defines the colonial era as ending within the 20th century and moving into a 'postcolonial' era. 
                   Nevertheless, relationships of exploitation and dependency between nations hugely uneqal in power continue to this day, whether it be through systems of global capitalism or claims on 'terrorities' like Puerto Rico (in the case of the United States).
                   To define climate colonialism only by an earlier period is insufficient, and so this project only represents a historical starting point for what is a highly urgent contemporary crisis.
                   Third, if you find any errors or would like to share any constructive advice, please reach out! You can find my email below at my Github repository for this project."),
                   h5(a("Project Github", href = "https://github.com/winona2468/Gov-50-Final-Project")),
                   h4("Thank you for reading!")
                 )
               )
           )
)



