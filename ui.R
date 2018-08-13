
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com


library(shiny)
library(DT)
library(dplyr)

shinyUI(fluidPage(
  # Application title
  titlePanel("M19: First Pick Probabilities (FPPs)"),
  mainPanel(
    DT::dataTableOutput("fpp"),
    p(""),
    a("for details check <<MTGANALYZE: Calculation of FPPs>>", href= "https://mtganalyze.github.io/post/picks-and-preferences/" , target = "_blank")
  )
))
