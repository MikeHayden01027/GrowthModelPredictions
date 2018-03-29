
# -----------------------------
# Load Packages
# -----------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggvis)
library(rlang)
library(here)
library(shiny)
library(shinycssloaders)
library(shinythemes)

# -----------------------------
# Load Functions
# -----------------------------

theme_Publication <- function(base_size=14, base_family="Arial") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            legend.direction = "vertical",
            legend.key.size= unit(1 , "cm"),
            legend.text = element_text(size = 15),
            legend.margin = unit(0.2, "cm"),
            legend.title = element_text(face="bold" , size = 14),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

# -----------------------------
# Set Working Directory and Load Data
# -----------------------------

setwd("/users/ben/modelPredictions/wbGrowth/")
load('westBrook_P_MainEffects.RData')
load('westBrook_P_Interactions.RData') 

# -----------------------------
# Set Factor Levels
# -----------------------------

pMainEffects$River<-factor(pMainEffects$River, levels=c('West Brook' , 'Open Small' , 'Open Large' , 'Isolated Small'))
pMainEffects$Species <- factor(pMainEffects$Species , levels = c("Brook Trout" , "Brown Trout" , "Atlantic Salmon"))
pMainEffects$Season <- factor( pMainEffects$Season , levels = c("Spring" , "Summer" , "Fall" , "Winter"))
shapes <- c(21 ,23 , 24 , 22)


# -----------------------------
# Shiny Server
# -----------------------------

shinyServer(function(input, output) {
  
# -----------------------------
# Main Effects
# -----------------------------  
  
data <- reactive({ pMainEffects %>% filter(River %in% input$riverA , 
                                           dataType == input$dataType1 ,
                                           Season %in% input$seasoNA , 
                                           Age %in% input$ageA , 
                                           Species %in% input$specieSA ) })
  
output$plot1A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    
    e <- data() %>% filter( type == 'Stream Temperature') %>% 
               ggplot( aes(y = predGrowTH , x = temp , shape = River)  ) +
                geom_line(aes(group = interaction(iter , Season , River , Species , Age) , colour = Species)  , alpha = 0.05 ) +
                geom_line(aes( y = predGrMean , x = temp , linetype = Season ,  group = interaction( Species , Season  , River, Age) , colour = Species) , size = 0.5) + 
                geom_point( aes( y = predGrMean , x = temp , group = interaction(Season , Species , River, Age)  , shape = River , fill = Species )  , size = 4 ) + 
                scale_shape_manual(values= shapes) +
                scale_colour_Publication()+ scale_fill_Publication() + 
                theme_Publication() + 
                guides(fill = FALSE) +
                labs(y = iterdataT , x = "Temp")+ 
                facet_grid(.~Age)

if(input$dataType1 == "Mean Growth mm/d"){
  e <- e + ylim(-0.3 , 1.1)
}

if(input$dataType1 == "Standard Deviation"){
  e <- e + ylim(-0.1 , 0.5)
}

if(input$dataType1 == "CV"){
  e <- e + ylim(-0.1 , 2.5)
}

e

})
  
  output$plot2A <- renderPlot({  
    
    iterdataT <- input$dataType1
  
    e <- data() %>% filter( type == 'Stream Flow' ) %>% 
               ggplot( aes(y = predGrowTH , x = flow , shape = River)  ) +
                geom_line(aes(group = interaction(iter , Season , River , Species , Age) , colour = Species)  , alpha = 0.05 ) +
                geom_line(aes( y = predGrMean , x = flow , linetype = Season ,  group = interaction( Species , Season  , River, Age) , colour = Species) , size = 0.5) + 
                geom_point( aes( y = predGrMean , x = flow , group = interaction(Season , Species , River, Age)  , shape = River , fill = Species )  , size = 4 ) + 
                scale_shape_manual(values= shapes) +
                scale_colour_Publication()+ scale_fill_Publication() + 
                theme_Publication() + ylim(-0.3 , 1.1) + 
                guides(fill = FALSE) +
                labs(y = iterdataT , x = "Stream Flow")+ 
                facet_grid(.~Age)
    
    if(input$dataType1 == "Mean Growth mm/d"){
      e <- e + ylim(-0.3 , 1.1)
    }
    
    if(input$dataType1 == "Standard Deviation"){
      e <- e + ylim(-0.1 , 0.5)
    }
    
    if(input$dataType1 == "CV"){
      e <- e + ylim(-0.1 , 2.5)
    }
    
    e
    
    })
  
  output$plot3A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    e <- data() %>% filter( type == 'Estimated Abundance' ) %>% 
               ggplot( aes(y = predGrowTH , x = count , shape = River)  ) +
                geom_line(aes(group = interaction(iter , Season , River , Species , Age) , colour = Species)  , alpha = 0.05 ) +
                geom_line(aes( y = predGrMean , x = count , linetype = Season ,  group = interaction( Species , Season  , River, Age) , colour = Species) , size = 0.5) + 
                geom_point( aes( y = predGrMean , x = count , group = interaction(Season , Species , River, Age)  , shape = River , fill = Species )  , size = 4 ) + 
                scale_shape_manual(values= shapes) +
                scale_colour_Publication()+ scale_fill_Publication() + 
                theme_Publication()  + 
                guides(fill = FALSE) +
                labs(y = iterdataT , x = "Estimated Abundance")+ 
                facet_grid(.~Age)
    
    if(input$dataType1 == "Mean Growth mm/d"){
      e <- e + ylim(-0.3 , 1.1)
    }
    
    if(input$dataType1 == "Standard Deviation"){
      e <- e + ylim(-0.1 , 0.5)
    }
    
    if(input$dataType1 == "CV"){
      e <- e + ylim(-0.1 , 2.5)
    }
    
    e
    
    })
  
  output$plot4A <- renderPlot({  
    
    iterdataT <- input$dataType1
    
    e <- data() %>% filter( type == 'Fish Length' ) %>% 
               ggplot( aes(y = predGrowTH , x = len , shape = River)  ) +
                geom_line(aes(group = interaction(iter , Season , River , Species , Age) , colour = Species)  , alpha = 0.05 ) +
                geom_line(aes( y = predGrMean , x = len , linetype = Season ,  group = interaction( Species , Season  , River, Age) , colour = Species) , size = 0.5) + 
                geom_point( aes( y = predGrMean , x = len , group = interaction(Season , Species , River, Age)  , shape = River , fill = Species )  , size = 4 ) + 
                scale_shape_manual(values= shapes) +
                scale_colour_Publication()+ scale_fill_Publication() + 
                theme_Publication() + ylim(-0.3 , 1.1) + 
                guides(fill = FALSE) +
                labs(y = iterdataT , x = "Length") + 
                facet_grid(.~Age)
    
    if(input$dataType1 == "Mean Growth mm/d"){
      e <- e + ylim(-0.3 , 1.1)
    }
    
    if(input$dataType1 == "Standard Deviation"){
      e <- e + ylim(-0.1 , 0.5)
    }
    
    if(input$dataType1 == "CV"){
      e <- e + ylim(-0.1 , 2.5)
    }
    
    e
    
    })
  
# -----------------------------
# Interaction Effects
# -----------------------------       

d1 <- reactive({ pInteractions %>% filter(dataType == input$dataType2 , iter < input$slider2B )  })
  
mainFilter <- reactive({ 
    
    ### Age  
  
    if( input$facet1 == "Age" & input$facet2 == "Season"){ 
      
      e <- d1()%>% filter(Age %in% input$ageAgeSeason ,
                          Species %in% input$speciesAgeSeason ,
                          river %in% input$riverAgeSeason, 
                          season %in% input$seasonAgeSeason) } 
    
    if( input$facet1 == "Age" & input$facet2 == "River"){
      
      e <- d1()%>% filter(Age %in% input$ageAgeRiver ,
                          Species %in% input$speciesAgeRiver ,
                          River %in% input$riverAgeRiver , 
                          Season %in% input$seasonAgeRiver) } 
    
    if( input$facet1 == "Age" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(Age %in% input$ageAgeSpecies ,
                          Species %in% input$speciesAgeSpecies ,
                          River %in% input$riverAgeSpecies , 
                          Season %in% input$seasonAgeSpecies) } 
    
    
    ### River
  
    if( input$facet1 == "River" & input$facet2 == "Season"){ 
      
      e <- d1()%>% filter(Age %in% input$ageRiverSeason ,
                          Species %in% input$speciesRiverSeason ,
                          River %in% input$riverRiverSeason, 
                          Season %in% input$seasonRiverSeason) } 
    
    if( input$facet1 == "River" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(Age %in% input$ageRiverAge ,
                          Species %in% input$speciesRiverAge ,
                          River %in% input$riverRiverAge , 
                          Season %in% input$seasonRiverAge) } 
    
    if( input$facet1 == "River" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(Age %in% input$ageRiverSpecies ,
                          Species %in% input$speciesRiverSpecies ,
                          River %in% input$riverRiverSpecies , 
                          Season %in% input$seasonRiverSpecies) } 
    
  
    ### Season
  
    if( input$facet1 == "Season" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(Age %in% input$ageSeasonRiver ,
                          Species %in% input$speciesSeasonRiver ,
                          River %in% input$riverSeasonRiver, 
                          Season %in% input$seasonSeasonRiver) } 
    
    if( input$facet1 == "Season" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(Age %in% input$ageSeasonAge ,
                          Species %in% input$speciesSeasonAge ,
                          River %in% input$riverSeasonAge , 
                          Season %in% input$seasonSeasonAge) } 
    
    if( input$facet1 == "Season" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(Age %in% input$ageSeasonSpecies ,
                          Species %in% input$speciesSeasonSpecies ,
                          River %in% input$riverSeasonSpecies , 
                          Season %in% input$seasonSeasonSpecies) } 
    
  
    ### Species
  
    if( input$facet1 == "Species" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(Age %in% input$ageSpeciesRiver ,
                          Species %in% input$speciesSpeciesRiver ,
                          River %in% input$riverSpeciesRiver, 
                          Season %in% input$seasonSpeciesRiver) } 
    
    if( input$facet1 == "Species" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(Age %in% input$ageSpeciesAge ,
                          Species %in% input$speciesSpeciesAge ,
                          River %in% input$riverSpeciesAge , 
                          Season %in% input$seasonSpeciesAge) } 
    
    if( input$facet1 == "Species" & input$facet2 == "Season"){
      
      e <- d1()%>% filter(Age %in% input$ageSpeciesSeason ,
                          Species %in% input$speciesSpeciesSeason  ,
                          River %in% input$riverSpeciesSeason  , 
                          Season %in% input$seasonSpeciesSeason ) } 
    
    
    ### NoneOne
  
    if( input$facet1 == "None" & input$facet2 == "River"){ 
      
      e <- d1()%>% filter(Age %in% input$ageNoneRiver ,
                          Species %in% input$speciesNoneRiver ,
                          River %in% input$riverNoneRiver, 
                          Season %in% input$seasonNoneRiver) } 
    
    if( input$facet1 == "None" & input$facet2 == "Age"){
      
      e <- d1()%>% filter(Age %in% input$ageNoneAge ,
                          Species %in% input$speciesNoneAge ,
                          River %in% input$riverNoneAge , 
                          Season %in% input$seasonNoneAge) } 
    
    if( input$facet1 == "None" & input$facet2 == "Season"){
      
      e <- d1()%>% filter(Age %in% input$ageNoneSeason ,
                          Species %in% input$speciesNoneSeason  ,
                          River %in% input$riverNoneSeason  , 
                          Season %in% input$seasonNoneSeason ) } 
    
    if( input$facet1 == "None" & input$facet2 == "Species"){
      
      e <- d1()%>% filter(Age %in% input$ageNoneSpecies ,
                          Species %in% input$speciesNoneSpecies  ,
                          River %in% input$riverNoneSpecies  , 
                          Season %in% input$seasonNoneSpecies ) } 
    
    
    ### NoneTwo
  
    if( input$facet1 == "River" & input$facet2 == "None"){ 
      
      e <- d1()%>% filter(Age %in% input$ageRiverNone ,
                          Species %in% input$speciesRiverNone ,
                          River %in% input$riverRiverNone, 
                          Season %in% input$seasonRiverNone) } 
    
    if( input$facet1 == "Age" & input$facet2 == "None"){
      
      e <- d1()%>% filter(Age %in% input$ageAgeNone ,
                          Species %in% input$speciesAgeNone ,
                          River %in% input$riverAgeNone , 
                          Season %in% input$seasonAgeNone) } 
    
    if( input$facet1 == "Season" & input$facet2 == "None"){
      
      e <- d1()%>% filter(Age %in% input$ageSeasonNone ,
                          Species %in% input$speciesSeasonNone  ,
                          River %in% input$riverSeasonNone  , 
                          Season %in% input$seasonSeasonNone ) } 
    
    if( input$facet1 == "Species" & input$facet2 == "None"){
      
      e <- d1()%>% filter(Age %in% input$ageSpeciesNone ,
                          Species %in% input$speciesSpeciesNone  ,
                          River %in% input$riverSpeciesNone  , 
                          Season %in% input$seasonSpeciesNone ) } 
    
    if( input$facet1 == "None" & input$facet2 == "None"){
      
      e <- d1()%>% filter(Age %in% input$ageNoneNone ,
                          Species %in% input$speciesNoneNone  ,
                          River %in% input$riverNoneNone  , 
                          Season %in% input$seasonNoneNone ) } 
  
    e
  })
  
  
  
  
  
  tempLengthCount <- reactive({mainFilter() %>% filter(len == input$changeInTempLengthOne ,
                                                       count == input$changeInTempCountOne )})
  
  tempLengthFlow <- reactive({mainFilter() %>% filter(len == input$changeInTempLengthTwo ,
                                                      flow == input$changeInTempFlowOne )})
  
  tempCountFlow <- reactive({mainFilter() %>% filter(count == input$changeInTempCountTwo ,
                                                     flow == input$changeInTempFlowTwo )})
  
  flowLengthTemp <- reactive({mainFilter() %>% filter(len == input$changeInFlowLengthOne ,
                                                      temp == input$changeInFlowTempOne )})
  
  flowLengthCount <- reactive({mainFilter() %>% filter(len == input$changeInFlowLengthTwo ,
                                                       count == input$changeInFlowCountOne )})
  
  flowTempCount <- reactive({mainFilter() %>% filter(temp == input$changeInFlowTempTwo ,
                                                     count == input$changeInFlowCountTwo )})
  
  countLengthTemp <- reactive({mainFilter() %>% filter(len == input$changeInCountLengthOne ,
                                                       temp == input$changeInCountTempOne )})
  
  countLengthFlow <- reactive({mainFilter() %>% filter(len == input$changeInCountLengthTwo ,
                                                       flow == input$changeInCountFlowOne )})
  
  countTempFlow <- reactive({mainFilter() %>% filter(temp == input$changeInCountTempTwo ,
                                                     flow == input$changeInCountFlowTwo )})
  
  lengthCountTemp <- reactive({mainFilter() %>% filter(count == input$changeInLengthCountOne ,
                                                       temp == input$changeInLengthTempOne )})
  
  lengthCountFlow <- reactive({mainFilter() %>% filter(count == input$changeInLengthCountTwo ,
                                                       flow == input$changeInLengthFlowOne )})
  
  lengthTempFlow  <- reactive({mainFilter() %>% filter(temp == input$changeInLengthTempTwo ,
                                                       flow == input$changeInLengthFlowTwo )})
  
  
  
  
  
  
  
  d5 <- reactive({ d1() %>% filter(predGrowTH > 100)})

  
  output$plot1B <- renderPlot({  
    x_axis <- input$aXis
    iterdataT <- input$dataType2
    iterColor <- input$color 
    gg <- d5() %>% ggplot(aes_string( x = x_axis , y = "predGrowTH" ))  
    
    if( input$aXis == "temp" & input$color == "flow"){
      gg <- gg + geom_line(data = tempLengthCount() , aes( group=interaction(  iter , River , Season , flow, Species , Age  ) , colour = flow) , alpha = 0.3) +
        geom_line(data = tempLengthCount() , aes( y = meanPrGrowth , x = temp , group = flow ) , size = 1) +
        geom_point(data = tempLengthCount() , aes( y = meanPrGrowth , x = temp , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Stream Flow") +
        guides(fill = FALSE)}
    
    if(input$aXis == "temp" & input$color == "count" ){
      gg <- gg + geom_line(data = tempLengthFlow() , aes( group=interaction(  iter , River , Season , count, Species , Age ) , colour = count), alpha = 0.3) +
        geom_line(data = tempLengthFlow() , aes( y = meanPrGrowth , x = temp , group = count ) , size = 1) +
        geom_point(data = tempLengthFlow() , aes( y = meanPrGrowth , x = temp , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Estimated Abundance") +
        guides(fill = FALSE)} 
    
    if(input$aXis == "temp" & input$color == "len" ){
      gg <- gg + geom_line(data = tempCountFlow() , aes( group=interaction(  iter , River , Season , Species , Age , len) , colour = len), alpha = 0.3) +
        geom_line(data = tempCountFlow() , aes( y = meanPrGrowth , x = temp , group = len ) , size = 1) +
        geom_point(data = tempCountFlow() , aes( y = meanPrGrowth , x = temp , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Temperature" , y = iterdataT , colour = "Fish Length") +
        guides(fill = FALSE) } 
    
    
    if( input$aXis == "flow" & input$color == "count"){
      gg <- gg + geom_line(data = flowLengthTemp() , aes( group=interaction(   iter , River , Season , Species , Age , count) , colour = count), alpha = 0.3) +
        geom_line(data = flowLengthTemp() , aes( y = meanPrGrowth , x = flow , group = count ) , size = 1) +
        geom_point(data = flowLengthTemp() , aes( y = meanPrGrowth , x = flow , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Estimated Abundance")+
        guides(fill = FALSE) }
    
    if(input$aXis == "flow" & input$color == "temp" ){
      gg <- gg + geom_line(data = flowLengthCount() , aes( group=interaction(  iter , River , Season , Species , Age , temp) , colour = temp), alpha = 0.3) +
        geom_line(data = flowLengthCount() , aes( y = meanPrGrowth , x = flow , group = temp ) , size = 1) +
        geom_point(data = flowLengthCount() , aes( y = meanPrGrowth , x = flow , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "flow" & input$color == "len" ){
      gg <- gg + geom_line(data = flowTempCount() , aes( group=interaction(  iter , River , Season ,  Species , Age , len) , colour = len), alpha = 0.3) +
        geom_line(data = flowTempCount() , aes( y = meanPrGrowth , x = flow , group = len ) , size = 1) +
        geom_point(data = flowTempCount() , aes( y = meanPrGrowth , x = flow , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Stream Flow" , y = iterdataT , colour = "Fish Length")+
        guides(fill = FALSE) }
    
    
    if( input$aXis == "count" & input$color == "flow"){
      gg <- gg + geom_line(data = countLengthTemp() , aes( group=interaction( iter , River , Season , flow , Species , Age ) , colour = flow), alpha = 0.3) +
        geom_line(data = countLengthTemp() , aes( y = meanPrGrowth , x = count , group = flow ) , size = 1) +
        geom_point(data = countLengthTemp() , aes( y = meanPrGrowth , x = count , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Stream Flow")+
        guides(fill = FALSE) }
    
    if(input$aXis == "count" & input$color == "temp" ){
      gg <- gg + geom_line(data = countLengthFlow() , aes( group=interaction(   iter , River , Season , temp , Species , Age ) , colour = temp), alpha = 0.3) +
        geom_line(data = countLengthFlow() , aes( y = meanPrGrowth , x = count , group = temp ) , size = 1) +
        geom_point(data = countLengthFlow() , aes( y = meanPrGrowth , x = count , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "count" & input$color == "len" ){
      gg <- gg + geom_line(data = countTempFlow() , aes( group=interaction(  iter , River , Season , Species , Age , len) , colour = len), alpha = 0.3) +
        geom_line(data = countTempFlow() , aes( y = meanPrGrowth , x = count , group = len ) , size = 1) +
        geom_point(data = countTempFlow() , aes( y = meanPrGrowth , x = count , group = len  , fill = len), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Estimated Abundance" , y = iterdataT , colour = "Fish Length")+
        guides(fill = FALSE) }
    
    
    if( input$aXis == "len" & input$color == "flow"){
      gg <- gg + geom_line(data = lengthCountTemp() , aes( group=interaction( iter , River , Season , flow, Species , Age ) , colour = flow), alpha = 0.3) +
        geom_line(data = lengthCountTemp() , aes( y = meanPrGrowth , x = len , group = flow ) , size = 1) +
        geom_point(data = lengthCountTemp() , aes( y = meanPrGrowth , x = len , group = flow  , fill = flow), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Stream Flow")+
        guides(fill = FALSE) }
    
    if(input$aXis == "len" & input$color == "temp" ){
      gg <- gg + geom_line(data = lengthCountFlow() , aes( group=interaction(  iter , River , Season ,  Species , Age , temp) , colour = temp), alpha = 0.3) +
        geom_line(data = lengthCountFlow() , aes( y = meanPrGrowth , x = len , group = temp ) , size = 1) +
        geom_point(data = lengthCountFlow() , aes( y = meanPrGrowth , x = len , group = temp  , fill = temp), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Stream Temperature")+
        guides(fill = FALSE) }
    
    if(input$aXis == "len" & input$color == "count" ){
      gg <- gg + geom_line(data = lengthTempFlow() , aes( group=interaction(  iter , River , Season , count , Species , Age) , colour = count), alpha = 0.3) +
        geom_line(data = lengthTempFlow() , aes( y = meanPrGrowth , x = len , group = count ) , size = 1) +
        geom_point(data = lengthTempFlow() , aes( y = meanPrGrowth , x = len , group = count  , fill = count), shape = 21 , colour = "black" , size = 4 , stroke = 1) +
        labs(x = "Fish Length" , y = iterdataT , colour = "Estimated Abundance")+
        guides(fill = FALSE) }
    
    
    
    
    gg <- gg  + 
      xlim(-1.5 , 1.5) 
    theme(
      axis.title.x = element_text(color="black", size=14, face="bold"),
      axis.title.y = element_text(color="black", size=14, face="bold")) + theme_bw()  
    
    
    
    if(input$facet1 == "Age" & input$facet2 == "River"){
      gg <- gg + facet_grid(River~Age)
    }
    
    if(input$facet1 == "Age" & input$facet2 == "Season"){
      gg <- gg + facet_grid(Season~Age)
    }
    
    if(input$facet1 == "Age" & input$facet2 == "Species"){
      gg <- gg + facet_grid(Species~Age)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Age"){
      gg <- gg + facet_grid(Age~River)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Season"){
      gg <- gg + facet_grid(Season~River)
    }
    
    if(input$facet1 == "River" & input$facet2 == "Species"){
      gg <- gg + facet_grid(Species~River)
    }
    
    
    if(input$facet1 == "Season" & input$facet2 == "Age"){
      gg <- gg + facet_grid(Age~Season)
    }
    
    if(input$facet1 == "Season" & input$facet2 == "River"){
      gg <- gg + facet_grid(River~Season)
    }
    
    if(input$facet1 == "Season" & input$facet2 == "Species"){
      gg <- gg + facet_grid(Species~Season)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "Age"){
      gg <- gg + facet_grid(Age~Species)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "River"){
      gg <- gg + facet_grid(River~Species)
    }
    
    if(input$facet1 == "Species" & input$facet2 == "Season"){
      gg <- gg + facet_grid(Season~Species)
    }
    
    
    
    
    
    if(input$facet1 == "Age" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~Age)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Age"){
      gg <- gg + facet_grid(Age~.)
    }
    
    
    if(input$facet1 == "River" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~River)
    }
    
    if(input$facet1 == "None" & input$facet2 == "River"){
      gg <- gg + facet_grid(River~.)
    }
    
    
    
    if(input$facet1 == "Season" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~Season)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Season"){
      gg <- gg + facet_grid(Season~.)
    }
    
    
    
    if(input$facet1 == "Species" & input$facet2 == "None"){
      gg <- gg + facet_grid(.~Species)
    }
    
    if(input$facet1 == "None" & input$facet2 == "Species"){
      gg <- gg + facet_grid(Species~.)
    }
    
    
    if(input$facet1 == "None" & input$facet2 == "None"){
      gg <- gg 
    }
    
    if(input$dataType2 == "Mean Growth mm/d"){
      gg <- gg + ylim(-0.3 , 1.1)
    }
    
    if(input$dataType2 == "Standard Deviation"){
      gg <- gg + ylim(-0.1 , 0.5)
    }
    
    if(input$dataType2 == "CV"){
      gg <- gg + ylim(-0.1 , 5)
    }
    
    
    gg <- gg + theme_Publication()
    
    
    gg
    
  } )
  
    
  
  
})