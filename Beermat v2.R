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
          radioButtons("sort", "Sort by (ascending):", 
            c("Pairs" = "pair", "Height (cm)" = "height", "Born in the UK" = "uk",
            "Successfully catching the first flip" = "flip1", "Number of successful flips from 20" = "flip20",
            "Number of successful flips in one minute" = "flipmin",
            "Time to 10 successful flips (seconds)" = "flip10sec",
            "Successfully catching the last flip" = "flip2", "Tallest of pair" = "tallest")),
          radioButtons("descending", "Ascending or descending:", 
            c("Ascending" = FALSE, "Descending" = TRUE))
        ),
        mainPanel(
          h2("Data and Leaderboard"),
          tableOutput("data")
        )
      )
    )
  )    
)

server <- function(input, output) {
  
  ###Read in the data and organise it and put it into the format
  ###that is required for the analysis (long format).
  dset <- reactive({
    #read in data from google spreadsheet
    dset1 <- as.data.frame(gsheet2tbl('docs.google.com/spreadsheets/d/1tSrAkifvSWJUNxdO0WjH4WA5HxVySAVqLaxvhCR-BBE/edit?usp=sharing'),)
    #give the variables more appropriate names
    names(dset1) <- c("date","pair", "height.tall", "height.short", "uk.tall", 
      "uk.short", "flip1.tall", "flip1.short", "flip20.tall", "flip20.short",
      "flipmin.tall", "flipmin.short", "flip10sec.tall", "flip10sec.short",
      "flip2.tall", "flip2.short")
    #round off some variables to an appropriate number of decimal places
    dset1[,c("pair", "height.tall", "height.short", "flip20.tall", "flip20.short", "flipmin.tall", "flipmin.short",
      "flip10sec.tall", "flip10sec.short")] <- round(dset1[,c("pair", "height.tall", "height.short", "flip20.tall",
      "flip20.short", "flipmin.tall", "flipmin.short","flip10sec.tall", "flip10sec.short")])
    #the rest of this code transforms the data from a wide to a long format
    dset.tall <- dset1[,c(1,2,which(grepl("tall",names(dset1))))]
    dset.tall$tallest <- "Yes"
    dset.short <- dset1[,c(1,2,which(grepl("short",names(dset1))))]
    dset.short$tallest <- "No"
    names(dset.tall) <- names(dset.short) <-
      c("date","pair", "height", "uk",
        "flip1", "flip20", "flipmin",
        "flip10sec", "flip2", "tallest")
    return(rbind(dset.short,dset.tall))
  })
  
  ###leaderboard output
  #the leaderboard table (that can be sorted based on users inputs)
  #this is the data that rest of the analysis is based on
  output$data <- renderTable({
    data.frame(Rank=1:nrow(dset()), dset()[order(dset()[,input$sort],decreasing = input$descending),-1])
  })
}

shinyApp(ui = ui, server = server)
