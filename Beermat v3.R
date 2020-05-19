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
            c("Ascending" = FALSE, "Descending" = TRUE)),
          uiOutput("dates")
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
  
  ###Derive a list of dates that the game has been played according to the input 
  ###data (to do this we need to read the data in)
  datescol <- reactive({
    dset.dates <- as.data.frame(gsheet2tbl(
      'docs.google.com/spreadsheets/d/1tSrAkifvSWJUNxdO0WjH4WA5HxVySAVqLaxvhCR-BBE/edit?usp=sharing'
      ))[,1]
    sort(unique(as.Date(dset.dates, 1, 
    unlist(lapply(strsplit(dset.dates, ''), function(x) which(x == ' ')))-1,
    format = "%m/%d/%Y")),decreasing = TRUE)
  })
  
  #checkboxes for dates to use for the analysis (default is most recent) and ordered
  #in reverse chornological order. These buttons have to be part of the output in the server
  #because they are based on the dates available in the data
  output$dates <- renderUI({
    checkboxGroupInput("dates", "Date of game:", choices = datescol(), 
      selected = max(datescol()))
  })
  
  ###Read in the data again, but this time organise it and put it into the format
  ###that is required for the analysis (long format). This also takes a subset of
  ###it depending on which date is selected by the user.
  dset <- reactive({
    dset1 <- as.data.frame(gsheet2tbl('docs.google.com/spreadsheets/d/1tSrAkifvSWJUNxdO0WjH4WA5HxVySAVqLaxvhCR-BBE/edit?usp=sharing'),)
    names(dset1) <- c("date","pair", "height.tall", "height.short", "uk.tall", 
      "uk.short", "flip1.tall", "flip1.short", "flip20.tall", "flip20.short",
      "flipmin.tall", "flipmin.short", "flip10sec.tall", "flip10sec.short",
      "flip2.tall", "flip2.short")
    dset1[,c("pair", "height.tall", "height.short", "flip20.tall", "flip20.short", "flipmin.tall", "flipmin.short",
      "flip10sec.tall", "flip10sec.short")] <- round(dset1[,c("pair", "height.tall", "height.short", "flip20.tall",
      "flip20.short", "flipmin.tall", "flipmin.short","flip10sec.tall", "flip10sec.short")])
    dset1$date <- as.Date(dset1$date, 1,
      unlist(lapply(strsplit(dset1$date, ''), function(x) which(x == ' ')))-1,
      format = "%m/%d/%Y")
    dset1 <- dset1[order(dset1$date,decreasing = TRUE),]
    ldata <- list()
    for (i in 1:length(input$dates)) {
      ldata[[i]] <- dset1[dset1$date == input$dates[i],]
    }
    dset1 <- rbind.fill(ldata)
    # if (length(input$dates)==1) dset1 <- dset1[dset1$date == input$dates,]
    # if (length(input$dates)>1) dset1 <- dset1[dset1$date %in% input$dates,]
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
  #the leaderboard table (that can be sorted based on users inputs)#
  #this is the data that rest of the analysis is based on
  output$data <- renderTable({
    if (!is.null(input$dates)) {
      data.frame(Rank=1:nrow(dset()), dset()[order(dset()[,input$sort],decreasing = input$descending),-1])
    }
  })
  
  #warning message to display if no date is selected and therefore no data.
  output$nodata <- renderText({
    if (is.null(input$dates)) {
      HTML(paste0("Please select 'date of game' on the left hand side to choose some data."))
    }
  })
}

shinyApp(ui = ui, server = server)
