#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(ggvis)
library(highcharter)
library(rlang)
library(shiny)

# Define UI for application that draws a histogram
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(ggvis)
library(highcharter)
library(rlang)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("West Brook Antenna DataBase Viewer Version 2"),
    fluidRow(
      column(3,
             wellPanel(
               selectInput('age' , 
                           'Year Class' , 
                           c(sym('0') , sym('1')) ,
                           sym('1')) ,
               splitLayout(
                 checkboxGroupInput("river" ,
                                    "River" , 
                                    c("west brook" , "wb jimmy" , "wb obear" , "wb mitchell") ,
                                    "west brook" ) ,
                 checkboxGroupInput("seasoN" ,
                                    "Season" ,
                                    c('1' , '2' , '3' , '4') ,
                                    '1' )),
               sliderInput("slider2", label = h3("Slider Range"), min = 10, 
                           max = 1000, value =  60)
             )),
      
      # Show a plot of the generated distribution
      column(9,
           
                    splitLayout(cellWidths = c("50%", "50%"),
                                ggvisOutput("plot1"),
                                ggvisOutput("plot2")),
                    ggvisOutput("plot3")
      )
    )
  ))