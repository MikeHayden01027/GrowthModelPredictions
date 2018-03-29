#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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



setwd("/Users/michaeljames/Documents/MastersProject/West Brook Visulizations/Data")
load("pForMike.RData")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  plotPred <- function(d, varsToPlot, isYOYGG, speciesGG , EEvent) {
    #  all = c('len','temp','flow','count')
    all = c('temp','flow','count')
    
    # if(length(varsToPlot) == 1) {
    notPlot <- NA
    notPlot[1] <- all[!(all %in% varsToPlot)][1]
    notPlot[2] <- all[!(all %in% varsToPlot)][2]
    #   notPlot[3] <- all[!(all %in% varsToPlot)][3]
    
    #   pGG <- p %>% filter(isYOY == isYOYGG, species == speciesGG, eval(as.name(notPlot[1])) == 0, eval(as.name(notPlot[2])) == 0, eval(as.name(notPlot[3])) == 0 ) %>%
    #                 distinct(eval(as.name(varsToPlot[1])), iter, isYOY, river, species, season, .keep_all = TRUE)
    d %>% filter(isYOY == isYOYGG, eval(as.name(notPlot[1])) == 0, eval(as.name(notPlot[2])) == 0 ) %>%
      distinct(eval(as.name(varsToPlot[1])), iter, isYOY, river, season, .keep_all = TRUE) %>%
      group_by_(EEvent , 'river' , 'season' ) %>% 
      summarise(meanPredGr = mean(predGr) , 
                maxPredGr = max(predGr) ,
                minPredGr = min(predGr)) %>%
      filter(season %in% input$seasoN) }
  
  age <- reactive({  sym(input$age)})
  
  age <- reactive({  sym(input$age)})
  
  pGGflow <- reactive({ 
    plotPred(p , 'flow' , age() , 'bkt' , 'flow')  %>% filter(river %in% input$river) })
  
  pGGtemp <- reactive({ 
    plotPred(p , 'temp' , age() , 'bkt' , 'temp') %>% filter(river %in% input$river) })
  
  pGGcount <- reactive({ 
    plotPred(p , 'count' , age() , 'bkt' , 'count') %>% filter(river %in% input$river) })
  
  #pGGtemp <- reactive({ 
  #  plotPred(p , 'len' , age() , 'bkt' , 'temp') %>% filter(river %in% input$river) })
  
  
  #output$plot1 <- renderHighchart({
  # e <-  pGGtemp() %>%  filter(river %in% input$river) 
  #  groupVar2 <- sym('river')     
  #  highchart() %>%
  #    hc_add_series(e , "line" ,  hcaes(x = temp , y = meanPredGr  , group =  river ))})
  
  #output$plot2 <- renderHighchart({
  #  e <-  pGGflow() %>% filter(river %in% input$river) 
  #  groupVar2 <- 'river'
  #  highchart() %>%
  #    hc_add_series(e , "line" ,  hcaes(x = flow , y = meanPredGr ,  group =  "input$group" ))})
  
  input_group <- reactive({ sym(input$group) })
  
  vis1 <- reactive({ x <- pGGtemp %>%  group_by(river , season) %>%
                                       ggvis(y =~ meanPredGr , x =~temp) %>%
                                        layer_points(fill = ~river , shape = ~season) %>%
                                        layer_lines(stroke = ~ river , strokeWidth := 2) %>%
                                        add_legend("shape", properties = legend_props(legend = list(y = 100))) %>%
                                        set_options(duration = 0)
    
                                  if(input$error == TRUE){
                                         x <- x %>%  layer_lines(x=~temp , y=~maxPredGr , strokeDash := 5 , stroke = ~river) %>%
                                                     layer_lines(x=~temp , y=~minPredGr , strokeDash := 5 , stroke = ~river) }
      x
  })
  
  vis1 %>% bind_shiny("plot1")
  
  vis2 <- reactive({ x <- pGGflow %>%  group_by(river , season) %>%
                                       ggvis(y =~ meanPredGr , x =~flow) %>%
                                       layer_points(fill = ~river, shape = ~season) %>%
                                       layer_lines(stroke = ~ river , strokeWidth := 2) %>%
                                       set_options(duration = 0)
    
                                  if(input$error == TRUE){
                                         x <- x %>%  layer_lines(x=~flow , y=~maxPredGr , strokeDash := 5 , stroke = ~river) %>%
                                                     layer_lines(x=~flow , y=~minPredGr , strokeDash := 5 , stroke = ~river) }
    x
      
      
  })
  
  vis2 %>% bind_shiny("plot2")
  
  
  vis3 <- reactive({ x <- pGGcount %>%  group_by(river , season) %>%
    ggvis(y =~ meanPredGr , x =~count) %>%
    layer_points(fill = ~river, shape = ~season) %>%
    layer_lines(stroke = ~ river , strokeWidth := 2) %>%
    set_options(duration = 0)
  
  if(input$error == TRUE){
    x <- x %>%  layer_lines(x=~count , y=~maxPredGr , strokeDash := 5 , stroke = ~river) %>%
      layer_lines(x=~count , y=~minPredGr , strokeDash := 5 , stroke = ~river) }
  x
  
  
  })
  
  vis3 %>% bind_shiny("plot3")
  
})


