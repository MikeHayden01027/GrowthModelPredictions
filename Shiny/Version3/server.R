#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
plotPred <- function(d, varsToPlot, isYOYGG, speciesGG ) {
                all = c('temp','flow','count')
                notPlot <- NA
                notPlot[1] <- all[!(all %in% varsToPlot)][1]
                notPlot[2] <- all[!(all %in% varsToPlot)][2]
                d %>% filter(isYOY == isYOYGG, eval(as.name(notPlot[1])) == 0, eval(as.name(notPlot[2])) == 0 ) %>%
                      distinct(eval(as.name(varsToPlot[1])), iter, isYOY, river, season, .keep_all = TRUE) }
  
tempOne <- reactive({ plotPred(p , 'temp' , input$age , 'bkt' ) %>% 
                  # filter(river %in% input$river , season %in% input$seasoN) %>% 
                   group_by(temp , river , season) %>%
                   summarise(predGr = mean(predGr)) %>%
                   ungroup() })
                     # mutate(temP = as.numeric(temp), 
                            # predGR = as.numeric(predGr)) })
  
tempTwo <- reactive({ plotPred(p , 'temp' , input$age , 'bkt' ) %>% 
                  filter(river %in% input$river , season %in% input$seasoN   ) %>% 
                  left_join(tempOne() , by = c('river' = 'river' , 'temp' = 'temp' , 'season' = 'season')) })
  
plotvis1 <- reactive({tempTwo() %>% ggvis( y = ~predGr.x , x = ~temp ) %>%
                                     group_by( iter,  river , season) %>%
                                     layer_lines(stroke = ~river , opacity := 0.2 ) %>%
                                     group_by( river , season) %>%
                                     layer_lines( y =~predGr.y , x =~temp , stroke = ~river , strokeWidth := 2) %>%
                                     layer_points(y =~predGr.y , x =~temp , shape = ~season , fill = ~river)  })
  
   plotvis1 %>% bind_shiny("plot1")
   
   
   
   flowOne <- reactive({ plotPred(p , 'flow' , input$age , 'bkt' ) %>% 
      # filter(river %in% input$river , season %in% input$seasoN) %>% 
       group_by(flow , river , season) %>%
       summarise(predGr = mean(predGr)) %>%
       ungroup() })
   # mutate(temP = as.numeric(temp), 
   # predGR = as.numeric(predGr)) })
   
   flowTwo <- reactive({ plotPred(p , 'flow' , input$age , 'bkt' ) %>% 
       filter(river %in% input$river , season %in% input$seasoN   ) %>% 
       left_join(flowOne() , by = c('river' = 'river' , 'flow' = 'flow' , 'season' = 'season')) })
   
   plotvis2 <- reactive({flowTwo() %>% ggvis( y = ~predGr.x , x = ~flow ) %>%
       group_by( iter,  river , season) %>%
       layer_lines(stroke = ~river , opacity := 0.2 ) %>%
       group_by( river , season) %>%
       layer_lines( y =~predGr.y , x =~flow , stroke = ~river , strokeWidth := 2) %>%
       layer_points(y =~predGr.y , x =~flow , shape = ~season , fill = ~river)  })
   
   plotvis2 %>% bind_shiny("plot2")
   
   
   
   countOne <- reactive({ plotPred(p , 'count' , input$age , 'bkt' ) %>% 
       filter(river %in% input$river , season %in% input$seasoN) %>% 
       group_by(count , river , season) %>%
       summarise(predGr = mean(predGr)) %>%
       ungroup() })
   # mutate(temP = as.numeric(temp), 
   # predGR = as.numeric(predGr)) })
   
   countTwo <- reactive({ plotPred(p , 'count' , input$age , 'bkt' ) %>% 
       filter(river %in% input$river , season %in% input$seasoN    ) %>% 
       left_join(countOne() , by = c('river' = 'river' , 'count' = 'count' , 'season' = 'season')) })
   
   plotvis3 <- reactive({countTwo() %>% ggvis( y = ~predGr.x , x = ~count ) %>%
       group_by( iter,  river , season) %>%
       layer_lines(stroke = ~river , opacity := 0.2 ) %>%
       group_by( river , season) %>%
       layer_lines( y =~predGr.y , x =~count , stroke = ~river , strokeWidth := 2) %>%
       layer_points(y =~predGr.y , x =~count , shape = ~season , fill = ~river)  })
   
   plotvis3 %>% bind_shiny("plot3")
   
})
