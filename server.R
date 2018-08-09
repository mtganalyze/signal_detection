
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

load("results/firstpicks.Rdata")
source('helpers.R')

shinyServer(function(input, output){
  output$fpp <- renderDataTable({
      fps$cards %>%
        filter(!is.na(rating)) %>% 
        select(name, rarity, rating, p1p1) %>%
        mutate(p1p1 = round(100 * p1p1, digits = 2)) %>% 
        arrange(desc(p1p1)) %>% 
        rename('FPP (in %)' = p1p1, Rarity = rarity, Rating = rating, Name = name) %>% 
        DT::datatable(rownames = NULL) %>%
        formatStyle(columns = c(1,4), fontWeight = 'bold')
  })
})
