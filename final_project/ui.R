#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#Tabs
shinyUI(
    navbarPage(theme = shinytheme("cyborg"),
               "Winona's Final Project",
               tabPanel(
                   "About",
                   p("This is the text for page about project", a("this is a link", href = "[aste url here"))),
                   h2("next line of text"),
               # You would add your content within the parentheses above.
               tabPanel(
                   "Page2 Title"
               )
    ))
