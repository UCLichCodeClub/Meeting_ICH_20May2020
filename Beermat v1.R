###Install packages
#install.packages("shiny")
library(shiny)
#install.packages("gsheet")
library(gsheet)
#install.packages("shinythemes")
library(shinythemes)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("plyr")
library(plyr)

ui <- fluidPage(
  tabsetPanel(
    tabPanel(title = "Data / Leaderboard",
      sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
        )
      )
    )
  )    
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)
