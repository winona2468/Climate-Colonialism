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
library(gt)

#### SETTING UP THE PAGE ####

shinyUI(
    navbarPage(theme = shinytheme("journal"),
               "Climate Colonialism",

#### FIRST PAGE ####

               tabPanel(
                   "A Riddle",
                    
                   fluidPage(
                     
                     titlePanel("What predicts risk to the climate crisis?"),
                     
                     mainPanel(
                       h4("For countries, you might guess that it's something like CO2 emissions: their own actions linked to the consequences. 
                           However, that is not the case."),
                       plotOutput("model_emissions_plot"),
                       h4("Indeed, running the model yielded zero correlation."),
                       h4("If not emissions, then what?")
                     )
                   
                   )
               ),




#### SECOND PAGE ####

tabPanel(
  "Emissions vs. Effects", 
  
  # PART ONE    
  fluidPage(
    
    # Two columns. 
    
    fluidRow(column(
      4,
      h3("Mapping it out: countries around the world are contributing to climate crisis at very different rates."),
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
    ),
    
    # PART TWO   
    
    fluidRow(column(
      4,
      h3("But those emitting the least, are often impacted the most."),
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
    )
    
  )
  
),




#### THIRD PAGE ####

            tabPanel(
              "Climate Empire", 
              
              fluidPage(
                
                titlePanel("Climate crisis travels, exploitatively, across borders."),
                
                mainPanel(
                  h4("Scholars like Doreen Martinez and Olúfẹ́mi O. Táíwò have been calling this global system of domination by the term of climate colonialism. 
                  'To me, [climate colonialism] is the deepening or expansion of foreign domination through climate initiatives that 
                     exploit poorer nations' resources or otherwise compromises their sovereignty,' wrote Táíwò.
                     'Others focus more on how formerly colonized countries are paying the price for a crisis 
                     caused disproportionately by the emissions from more industrialized nations—their current and past colonizers.'"),
                  h4("Let's look again at our data: we'll categorize countries by their history of colonization, and then map climate risk."),
                  plotOutput("introPlot"),
                  h4("Could this be a coincidence? There are a lot of outliers on this graph, and when we remove them, the boxplots still show some overlap.
                     Nevertheless, if we look at the median values, it's astonishing that at least half of non-colonized countries 
                     experience so little climate risk compared to the rest of the world."),
                  plotOutput("introboxPlot"),
                  h4("Histories of global empire (consider The Triangular Trade or Orientalism) are also heavily connected to regions of the world. 
                     Today, consider multiple instances of European nations exploiting those on the African continent: for example, Táíwò writes about Norway seeking East African land for carbon offsets 
                     that would create evictions and food scarcity across Uganda, Mozambique, and Tanzania, as well as the fact that Africa houses the world's largest solar power plant (in Morocco), 
                     yet also has the least access to solar power. Below, let's compare average climate risk across regions."),
                  plotOutput("worldregionPlot"),
                  h4("Imagine climate colonialism today, and think of how mining devastates local land (Mexico), mercury contaminates waters (Bolivia), forests disappear (India, Burma), and all forms of life suffer. 
                      As Montesquieu said, 'The empire of the climate is the first, the most powerful of all empires.'")
                  
                  )
               )
            ),



#### FOURTH PAGE ####

              tabPanel(
                "Models",
                h2("Can a country's colonial history predict their climate risk?"),
                
                fluidPage(
                  fluidRow(
                    column(
                    4,
                    h3("Let's start by defining which colonial histories we're including."),
                    h4("The ICOW Colonial History data set defines formal independence as when countries have control over their own foreign policy, 
                         and when they resemble the form of the modern state. 
                         It categorizes countries as achieving independence in four primary ways:
                         Formation (i.e. it was never colonized), Decolonization (it was once ruled by a foreign power), Secession (it was once part of another), and Partition (it was partitioned out of another state, which did not survive).
                         I have combined this data with climate vulnerability data.
                         Learn more to the right."),
                    h4("Note: 'Date of No Direct Colonial Control' refers not only to when a state has been recognized within an international system, 
                            but also when the colonial power no longer has direct control over the territory."),
                    h4("Precaution: The organization by the modern nation-state excludes by design (arguably a colonial project in and of itself); examples of who's left out include the varied Indigenous peoples in North America.
                       This data also does not account for modern neocolonial levers: political and economic domination, without direct rule or land occupation.")
                    
                          ),
                  
                  column(8,
                         dataTableOutput('fitmodTable'),
                         h1("")
                         )
                     ),
                  
                  mainPanel(
                      h3("So which model works best?"),
                      h4("Model 1: Our first option for defining countries as once-colonized is to take all countries marked under 'Decolonization' inside the ICOW data set. This gives a more clean-cut division of states, though 
                      one concern is that, under this definition, even powerful countries like the United States (once colonized by the United Kingdom) are included."),
                      h4("Model 2: Our second option is to manually revise the countries listed above with historical particularities related to imperialism. First, I include all countries marked in the data by secession or partition, like Guatemala and Bangladesh.
                      Many of these countries indeed have an imperial history too, though in different ways. Further, I remove the U.S. because of its later emergence as an imperial power, and Canada for profitting from imperialism too."),
                      h4("Model 3: Our third option adds the condition of decolonization after 1850, a period when industrial-era warming accelerated. 
                      I hypothesize this may better capture the predictive effect of colonization on climate risk, since colonialism is ongoing in a period of increasing climate change.")
                      ),
                  
                  
                  sidebarLayout(
                    
                        # In a gray box.
                    
                            sidebarPanel(
                               
                               selectInput("selected_model", h3("Models"),
                                           choices = list("Model 1",
                                                          "Model 2",
                                                          "Model 3")),
                               tableOutput("modelsPlot")
                                         ),
                             
                          
                              mainPanel(
                                       h4("* Alas, Model 1 has the lowest standard deviation! Let's use that one. Below is the posterior distribution. *"),
                                       plotOutput("posteriorPlot")
                              ),
                            
                            
                  
                  ),
                    
                  mainPanel(
                      h3("What important variables can we control for?"),
                  ),
                  
                  mainPanel(
                    h4("After running a bunch, I found two with particularly interesting effects: world region and governance."),
                    tableOutput("model_world"),
                    h4("Here, we run the same model but control for world region.
                        This output suggests that region, perhaps much more so than the binary status of being colonized or not, is predictive of climate risk to varying degrees. 
                        Of course, as discussed earlier, there may be significant overlap between these variables. Further, the high standard error is notable; with a t-test, we could perhaps better determine if these
                        are the best regional divisions for the patterns we're seeing."),
                    tableOutput("model_gov"),
                    h4("The variable governance refers to countries' 2008 KKM regulatory quality score, which measures countries by their
                       political stability, government effectiveness, regulatory quality, rule of law, and control of corruption, among other characteristics. The higher the score, the better.
                       Interestingly, this result could be interpreted in a number of contradictory, politicized ways. For example, one might say that since better governed countries have lower climate risk, high risk countries are in part to blame for their own misfortune. 
                       Another might argue that low quality governance is highly correlated with or even caused by imperialism and the conflict and violence it creates. 
                       This underscores the important roles both subjective interpretation and accurate historical, sociological context play, when we are looking at data.")
                  ),
                  
                  
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
                      To do so, I have taken three separate datasets: 
                        on colonial history, the CO2 emissions of countries, and the projected climate risk of countries."),
                   h5("First is a 2007 dataset on colonial history from the Harvard Dataverse. 
                      This was put together by the Issue Correlates of War (ICOW) Project, 
                      which tried 'to identify colonial or other dependency relationships for each state over the past two centuries.' 
                      The dataset includes the years in which countries were decolonized, as well as characteristics like the type of independence, whether it was violent, etc."),
                   h5(a("ICOW Colonial History Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/5EMETG")),
                   h5("Second is a 2011 dataset on climate risk, created by David Wheeler for the Center for Global Development. 
                      This was perhaps the most important dataset for my project, because it 'measures the vulnerability of 233 countries to three major effects of climate change: extreme weather-related disasters, sea-level rise, and reduced agricultural productivity'.
                      For example, with the extreme weather vulnerability variable, Wheeler writes, 'My core model specifies climate impact risk as a function of radiative forcing from atmospheric accumulation of CO2. I define climate impact risk in year t as the 
                      probability that a representative individual will be affected by an extreme weather event in that year.' The 'vulnerable' variable I use is the overall metric across the three effects of climate change.
                      One important note is that this dataset was collected in 2011. In the 9 years which have passed since then, our world and individual countries have changed in their increasing susceptibility to environmental harm. 
                      I wonder whether a dataset collected today would reflect significantly different comparative risks across countries.
                      Nevertheless, since this project discusses the effects of the colonial era (especially in the sense of political control, legal status, and physical occupation, all still going strong in the 20th century), perhaps data closer to then is still useful to see."),
                   h5(a("Climate Risk Data", href = "https://www.cgdev.org/publication/dataset-vulnerability-climate-change")),
                   h5("Third is a 2013 dataset from the Carbon Dioxide Information Analysis Center (Boden, Marland, Andres). 
                      It measures CO2 Emissions from fossil-fuels per country annually from 1751 to 2014.
                      I found this dataset on Github, prepared by the datahub.io project."),
                   h5(a("CO2 Emissions Data", href = "https://github.com/datasets/co2-fossil-by-nation")),
                   h5("If I were to expand my project, I would consider this 2017 dataset from the Harvard Dataverse which measures characteristics of colonialism in countries in Africa and Asia."),
                   h5(a("Colonialism in Asia and Africa Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UQZFYA")),
                   h4("Finally, I just wanted to sound three notes of caution: First, do not interpret any of the linkages I have created on this site as causal. I am interested in examining whether there is a relationship between different variables as a predictive question.
                   Second, the data I have used to map out colonial history largely defines the colonial era as ending within the 20th century and moving into a 'postcolonial' era. 
                   Nevertheless, relationships of exploitation and dependency between nations hugely unequal in power continue to this day, whether it be through systems of global capitalism or claims on 'terrorities' like Puerto Rico (in the case of the United States).
                   To define climate colonialism only by an earlier period is insufficient, and so this project only represents a historical starting point for what is a highly urgent contemporary crisis.
                   Third, if you find any errors or would like to share any constructive advice, please reach out! You can find my email below at my Github repository for this project."),
                   h5(a("Project Github", href = "https://github.com/winona2468/Gov-50-Final-Project")),
                   h4("Thank you for reading! And thank you my wonderful Gov 50 teachers, Wyatt Hurt and Tyler Simko.")
                 )
               )
           )
)



