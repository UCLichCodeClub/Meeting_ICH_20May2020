###Install shiny package
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

ui <- fluidPage(theme = shinytheme("cosmo"),
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
          tableOutput("data"),
          htmlOutput(outputId = "nodata")
        )
      )
    ),
    tabPanel(title = "Graphs (ch2)",
      sidebarLayout(
        sidebarPanel(
          radioButtons("graphtype", "Type of graph:", 
            c("Scatter plot"="scatter","Scatter plot with groups"="scatterg", "Histogram"="hist",
              "Dot Plot"="dot", "Dot plot with groups"="dotg" )),
          uiOutput("varnumeric"),
          uiOutput("varfactor2"),
          uiOutput("varnumeric2"),
          uiOutput("varfactor"),
          uiOutput("bins")
        ),
        mainPanel(
          h2("Chapter 2: Graphical Displays of the Data"),
          "Please choose a type of graph and select the variables on the left hand side.",
          plotOutput(outputId = "graph", width=400, height=400)
        )
      )
    ),
    tabPanel(title = "Summary Stats (ch3)",
        sidebarLayout(
          sidebarPanel(
            radioButtons("variablechoice", "Choose a variable:", 
              c("Height (cm)" = "height", "Born in the UK" = "uk",
              "Successfully catching the first flip" = "flip1", "Number of successful flips from 20" = "flip20",
              "Number of successful flips in one minute" = "flipmin",
              "Time to 10 successful flips (seconds)" = "flip10sec",
              "Successfully catching the last flip" = "flip2"))
        ),
        mainPanel(
          h2("Chapter 3: Summary Statistics for a Single Variable"),
          "Please choose a variable to summarise of the left hand side.",
          fluidRow(htmlOutput(outputId="textch3.1")),
          tags$br(),
          verbatimTextOutput(outputId = "tablech3"),
          verbatimTextOutput(outputId = "tablech3.2"),
          htmlOutput(outputId="textch3.2")
        )
      )
    ),
    tabPanel(title = "Diffs and Assoc (ch4)",
        sidebarLayout(
          sidebarPanel(
            radioButtons("variablecombchoice", "Choose the types of variable:", 
              c("Two categoric variables" = "cat2", 
              "One numberic, one categoric" = "cat1num1",
              "Two numeric variables"="num2")),
            uiOutput("varnumeric1ch3"),
            uiOutput("varfactor1ch3"),
            uiOutput("varnumeric2ch3"),
            uiOutput("varfactor2ch3")
          ),
        mainPanel(
          h2("Chapter 4: Quantifying Differences and Associations"),
          "Please choose the types of variables, and select specific variables on the left hand side.",
          #output if two categoric
          htmlOutput(outputId = "textch4.1"),
          verbatimTextOutput(outputId = "tablech4"),
          htmlOutput(outputId = "textch4.2"),
          verbatimTextOutput(outputId = "tablech4.2"),
          htmlOutput(outputId = "textch4.3"),
          #output if one numeric and one categoric
          htmlOutput(outputId = "textch4.4"),
          verbatimTextOutput(outputId = "tablech4.3"),
          htmlOutput(outputId = "textch4.5"),
          verbatimTextOutput(outputId = "tablech4.4"),
          htmlOutput(outputId = "textch4.6"),
          htmlOutput(outputId = "textch4.7"),
          plotOutput(outputId = "graphch4", width=300, height=300)
        )
      )
    ),
    tabPanel(title = "Statistical Inference (ch5+6)",
        sidebarLayout(
          sidebarPanel(
            radioButtons("testchoice", "Choose a test:", 
              c("One-sample t-test"="ttest1","Two-samples t-test"="ttest2",
              "Paired t-test"="ttestp","Chi-squared test"="chi", 
              "McNmear's test"="mcnemar", "Fishers exact test"="fishers",
              "Mann Whitney U test"="mw", "Wilcoxon one-sample test"="wilcox1",
              "Wilcoxon paired test"="wilcoxp")),
            uiOutput("varnumericch56"),
            uiOutput("varcatch56"),
            uiOutput("vargroup2ch56"),
            uiOutput("varpairingch56"),
            uiOutput("varmcnemars1ch56"),
            uiOutput("varmcnemars2ch56"),
            uiOutput("hyp")
          ),
        mainPanel(
          h2("Chapters 5 and 6: Statistical Inference (confidence intervals and p-values)"),
          "Please choose a test and select the appropriate variables on the left hand side.",
          htmlOutput(outputId = "textch56.ttest1")
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
  
  ###A function to turn variable names into their labels for nicer, consistent output
  labels.func <- function(x) {
    labout <- NA
    if (x=="pair") labout <- "pair"
    if (x=="height") labout <- "height (cm)"
    if (x=="uk") labout <- "born in the UK"
    if (x=="flip1") labout <- "catch the first flip"
    if (x=="flip20") labout <- "number of flips from 20"
    if (x=="flipmin") labout <- "number of flips in one minute"
    if (x=="flip10sec") labout <- "time to 10 flips (seconds)"
    if (x=="flip2") labout <- "catch the last flip"
    if (x=="tallest") labout <- "tallest of pair"
    return(labout)
  }
  
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
  
  ####graph outputs
  #input for numeric x axis (that only appears for certain plots)
  #input needed within the server because it depends on which graph they select
  output$varnumeric <- renderUI({
    if (input$graphtype %in% c("scatter","scatterg","hist")) {
      radioButtons("xvar", "Numeric x-axis variable:", 
        c("Height (cm)" = "height", "Number of successful flips from 20" = "flip20",
        "Number of successful flips in one minute" = "flipmin",
        "Time to 10 successful flips (seconds)" = "flip10sec"))
    }
  })
  #input for numeric y axis (that only appears for certain plots)
  output$varnumeric2 <- renderUI({
    if (input$graphtype %in% c("scatter","scatterg","dot","dotg")) {
      radioButtons("yvar", "Numeric y-axis variable:", 
         c("Height (cm)" = "height", "Number of successful flips from 20" = "flip20",
         "Number of successful flips in one minute" = "flipmin",
         "Time to 10 successful flips (seconds)" = "flip10sec"))
    }
  })
  #input for grouping variable (that only appears for certain plots)
  output$varfactor <- renderUI({
    if (input$graphtype %in% c("scatterg","dotg")) {
      radioButtons("gvar", "Grouping variable:", 
          c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
          "Successfully catching the last flip" = "flip2"))
    }
  })
  #input for categoric x-axis variable (that only appears for dot plot)
  output$varfactor2 <- renderUI({
    if (input$graphtype %in% c("dot","dotg")) {
      radioButtons("gvar2", "Categoric x-axis variable:", 
        c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
        "Successfully catching the last flip" = "flip2"))
    }
  })
  #input for number of bins if histogram
  output$bins <- renderUI({
    if (input$graphtype=="hist") {
      numericInput("bins", "Number of bins (0 = default):", value = 0, step = 1)
    }
  })
  #output for graph, one output that changes depending on the variable selected
  output$graph <- renderPlot({
    if (input$graphtype=="scatter") { #scatter plot
      par(mar=c(5,5,1,1))
      plot(x = with(dset(), get(input$xvar)), 
        y = with(dset(), get(input$yvar)), cex=1.5, cex.lab=1.2,cex.axis=1.2,
        xlab=labels.func(input$xvar), ylab=labels.func(input$yvar))
    } else if (input$graphtype=="scatterg") { #scatter plot with groups
      par(mar=c(5,5,1,1))
      plot(x = with(dset(), get(input$xvar)), 
        y = with(dset(), get(input$yvar)),
        xlab=labels.func(input$xvar), ylab=labels.func(input$yvar),
        pch=c(18,21)[as.factor(with(dset(), get(input$gvar)))], cex=1.5,
        cex.lab=1.2,cex.axis=1.2)
      legend("topleft", title=labels.func(input$gvar),
        legend = c("Yes","No"), pch=c(21,18), cex=1.5)
    } else if (input$graphtype=="hist") { #histogram
      hist(x = with(dset(), get(input$xvar)), xlab=labels.func(input$xvar), main="",
           breaks=if (input$bins==0) "Sturges" else input$bins)
    } else if (input$graphtype=="dot") { #dotplot
      par(mar=c(5,5,1,1))
      plot(x = jitter(as.numeric(as.factor(with(dset(), get(input$gvar2)))),amount = 0.075), 
        y = with(dset(), get(input$yvar)),
        xaxt="n", cex=1.5, cex.lab=1.2,cex.axis=1.2,
        xlab=labels.func(input$gvar2), ylab=labels.func(input$yvar))
        axis(1, at=c(1,2), labels=c("No","Yes"))
    } else if (input$graphtype=="dotg") { #dot plot with groups
      par(mar=c(5,5,1,1))
      plot(x = jitter(as.numeric(as.factor(with(dset(), get(input$gvar2)))),amount = 0.075), 
        y = with(dset(), get(input$yvar)),
        xaxt="n", cex=1.5, cex.lab=1.2,cex.axis=1.2,
        xlab=labels.func(input$gvar2), ylab=labels.func(input$yvar),
        pch=c(18,21)[as.factor(with(dset(), get(input$gvar)))])
      axis(1, at=c(1,2), labels=c("No","Yes"))
      legend("top", title=labels.func(input$gvar),
        legend = c("Yes","No"), pch=c(21,18), cex=1.5)
    }
  })
  
  ###summary statistics of one variable
  #text at the beginning to describe what statistics are produced
  output$textch3.1 <- renderText({
    HTML(paste0("<br>Summary statistics for the variable '",
      labels.func(input$variablechoice),"' are presented below. These include",
      if (input$variablechoice %in% c("uk","flip1","flip2")) " the frequencies and percentages. " else 
        " the minimum, first quartile, median, mean, third quartile and maximum. ",
      if (any(is.na(dset()[,input$variablechoice]))) {
        paste0("These statistics are calculated from the non-missing values and we have ", 
        sum(is.na(dset()[,input$variablechoice]))," missing value(s).")
      } else "There are no missing values for this variable."))
  })
  #main table of results (regardless of whether it is numeric or categoric)
  output$tablech3 <- renderPrint({
    if (input$variablechoice %in% c("flip1","uk","flip2")) {
      tch3 <- table(with(dset(), get(input$variablechoice)))
      print(data.frame(No=tch3[1],Yes=tch3[2],row.names = "Frequencies"))
    } else {
      summary(with(dset(), get(input$variablechoice)))
    }
  })
  #second table of results if categoric (percentages)
  output$tablech3.2 <- renderPrint({
    if (input$variablechoice %in% c("flip1","uk","flip2")) {
      tch3 <- table(with(dset(), get(input$variablechoice)))
      tch3.p <- round(prop.table(tch3)*100,1)
      print(data.frame(No=tch3.p[1],Yes=tch3.p[2],row.names = "Percentages"))
    }
  })
  #further explanation (odds or 95% range)
  output$textch3.2 <- renderText({
    if (input$variablechoice %in% c("flip1","uk","flip2")) {
      tch3 <- table(with(dset(), get(input$variablechoice)))
      HTML(paste0("The variable could also be summarised as odds. 
       The odds of '",labels.func(input$variablechoice),"' are ",tch3[2],"/",tch3[1],"=",round(tch3[2]/tch3[1],3),"."))
    } else {
      sdch3 <- sd(with(dset(), get(input$variablechoice)))
      meanch3 <- mean(with(dset(), get(input$variablechoice)))
      HTML(paste0("The standard deviation of '", labels.func(input$variablechoice), 
        "' is ", signif(sdch3,4),". If we assume this data is normally distributed then we expect 95% of values to lie in the range:<br/><br/>",
        "mean +/- 1.96 x SD = ",
        round(meanch3,1), " +/- 1.96 x ", signif(sdch3,4), " = ",
        round(meanch3-1.96*sdch3,1), " to ", round(meanch3+1.96*sdch3,1),".<br/><br/>",
        "We expect 2.5% of values to lie outside of this range on each side. Check the distribution through a histogram on the graphs tab."))
    }
  })
  
  ####assocations and difference (ch4)
  #numeric outcome variable
  output$varnumeric1ch3 <- renderUI({
    if (input$variablecombchoice %in% c("cat1num1","num2")) {
      radioButtons("numvar1", "Numeric outcome variable:", 
        c("Height (cm)" = "height", "Number of successful flips from 20" = "flip20",
        "Number of successful flips in one minute" = "flipmin",
        "Time to 10 successful flips (seconds)" = "flip10sec"))
    }
  })
  #numeric exposure variable (for associations)
  output$varnumeric2ch3 <- renderUI({
    if (input$variablecombchoice=="num2") {
      radioButtons("numvar2", "Numeric exposure variable:", 
        c("Height (cm)" = "height", "Number of successful flips from 20" = "flip20",
        "Number of successful flips in one minute" = "flipmin",
        "Time to 10 successful flips (seconds)" = "flip10sec"))
    }
  })
  #categoric outcome variable
  output$varfactor1ch3 <- renderUI({
    if (input$variablecombchoice=="cat2") {
      radioButtons("catvar1", "Categoric outcome variable:",
          c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
          "Successfully catching the last flip" = "flip2"))
    }
  })
  #grouping variable
  output$varfactor2ch3 <- renderUI({
    if (input$variablecombchoice %in% c("cat2","cat1num1")) {
      radioButtons("catvar2", "Grouping variable:",
          c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
          "Successfully catching the last flip" = "flip2"))
    }
  })
  #output for two categoric variables
  output$textch4.1 <- renderText({
    if (input$variablecombchoice=="cat2") {
      HTML(paste0("<br/>Initially, the best way to present the data of two categoric variables is through a 2x2 table. Here is a table of frequencies:<br/>"))
    }
  })
  output$tablech4 <- renderPrint({
    if (input$variablecombchoice=="cat2") {
      with(dset(), table(get(input$catvar2), get(input$catvar1),
        dnn = c(labels.func(input$catvar2),labels.func(input$catvar1))))
    }
  })
  output$textch4.2 <- renderText({
    if (input$variablecombchoice=="cat2") {
      HTML(paste0("And here is the equivalent table of row percentages:"))
    }
  })
  output$tablech4.2 <- renderPrint({
    if (input$variablecombchoice=="cat2") {
      round(prop.table(with(dset(), table(get(input$catvar2), get(input$catvar1),
        dnn = c(labels.func(input$catvar2),labels.func(input$catvar1)))), 
        margin = 1)*100,1)
    }
  })
  output$textch4.3 <- renderText({
    if (input$variablecombchoice=="cat2") {
      tch4 <- with(dset(), table(get(input$catvar2), get(input$catvar1)))
      tch4p <- prop.table(with(dset(), 
        table(get(input$catvar2), get(input$catvar1))), 
        margin = 1)*100
      HTML(paste0(tch4[2,2]," of ",tch4[2,2]+tch4[2,1]," (",round(tch4p[2,2],1),"%) ",
        if (input$catvar1 %in% c("flip1","flip2")) "were able to '" else "were '",
        labels.func(input$catvar1), "' out of those that were ",
        if (input$catvar2 %in% c("flip1","flip2")) "able to '" else "'",
        labels.func(input$catvar2),"'. ",
        tch4[1,2]," of ",tch4[1,2]+tch4[1,1]," (",round(tch4p[1,2],1),"%) ",
        if (input$catvar1 %in% c("flip1","flip2")) "were able to '" else "were '",
        labels.func(input$catvar1), "' out of those that were <b>not</b> ",
        if (input$catvar2 %in% c("flip1","flip2")) "able to '" else "'",
        labels.func(input$catvar2),
        "'. These percentages can be compared as a risk difference or a relative risk:
        <br><ul><li><b>Risk difference</b> = ",round(tch4p[2,2],1), "% - ", round(tch4p[1,2],1),"% = ", 
        round(tch4p[1,2] - tch4p[2,2],1),"%</li>",
        "<li> <b>Relative risk</b> = ",round(tch4p[2,2],1), "% / ", round(tch4p[1,2],1),"% = ",
        round(tch4p[2,2]/tch4p[1,2],2),
        "</li></ul>The odds of ",
        if (input$catvar1 %in% c("flip1","flip2")) "being able to '" else "being '",
        labels.func(input$catvar1), "' out of those that were ",
        if (input$catvar2 %in% c("flip1","flip2")) "able to '" else "'",
        labels.func(input$catvar2),"' are ",tch4[2,2], " / ",tch4[2,1]," = ",
        round(tch4[2,2] / tch4[2,1],2),". The odds of ",
        if (input$catvar1 %in% c("flip1","flip2")) "being able to '" else "being '",
        labels.func(input$catvar1), "' out of those that were ",
        if (input$catvar2 %in% c("flip1","flip2")) "<b>not</b> able to '" else "<b>not</b> '",
        labels.func(input$catvar2),"' are ",tch4[1,2], " / ",tch4[1,1]," = ",
        round(tch4[1,2] / tch4[1,1],2),
        ". These odds can be compared via an odds ratio: <ul><li><b>Odds ratio</b> = ",
        round(tch4[2,2] / tch4[2,1],2), " / ", round(tch4[1,2] / tch4[1,1],2)," = ",
        round((tch4[2,2] / tch4[2,1]) / (tch4[1,2] / tch4[1,1]),2)))
    }
  })
    #numeric outcome and categoric grouping variable
  output$textch4.4 <- renderText({
    if (input$variablecombchoice=="cat1num1") {
      HTML(paste0("<br>Here is the summary of ",labels.func(input$numvar1), " for those",
        if (input$catvar2 %in% c("flip1","flip2")) " able to '" else " '",
        labels.func(input$catvar2),"' (n = ",
        with(dset(), table(get(input$catvar2))[2])," with ", 
      with(dset(), sum(is.na(get(input$numvar1)[get(input$catvar2)=="Yes"])))," missing values):"))
    }
  })
  output$textch4.5 <- renderText({
    if (input$variablecombchoice=="cat1num1") {
      HTML(paste0("Here is the summary of ",labels.func(input$numvar1), 
      " for those <b>not</b>",
      if (input$catvar2 %in% c("flip1","flip2")) " able to '" else " '",
      labels.func(input$catvar2),"' (n = ",
      with(dset(), table(get(input$catvar2))[1])," with ", 
      with(dset(), sum(is.na(get(input$numvar1)[get(input$catvar2)=="No"])))," missing values):"))
    }
  })
  output$tablech4.3 <- renderPrint({
    if (input$variablecombchoice=="cat1num1") {
      summary(with(dset(), get(input$numvar1)[get(input$catvar2)=="Yes"]))
    }
  })
  output$tablech4.4 <- renderPrint({
    if (input$variablecombchoice=="cat1num1") {
      summary(with(dset(), get(input$numvar1)[get(input$catvar2)=="No"]))    
    }
  })
  output$textch4.6 <- renderText({
    if (input$variablecombchoice=="cat1num1") {
      mean1 <- mean(with(dset(), get(input$numvar1)[get(input$catvar2)=="Yes"]),na.rm=T)
      mean2 <- mean(with(dset(), get(input$numvar1)[get(input$catvar2)=="No"]),na.rm=T)
      median1 <- median(with(dset(), get(input$numvar1)[get(input$catvar2)=="Yes"]),na.rm=T)
      median2 <- median(with(dset(), get(input$numvar1)[get(input$catvar2)=="No"]),na.rm=T)
      HTML(paste0("The groups can be compared either through a mean difference 
      (if the normality assumption holds - see the graphs tab to investigate) or 
      median difference (if the normality assumption doesn't hold): <ul><li>
      <b>Mean difference </b> = ",
      round(mean1,1)," - ", round(mean2,1)," = ", round(mean1 - mean2,1), 
      "</li><li><b>Median difference</b> = ",
      round(median1,1)," - ", round(median2,1)," = ", round(median1 - median2,1),
      "</li></ul>"
      ))
    }
  })
  output$textch4.7 <- renderText({
    if (input$variablecombchoice=="num2") {
      HTML(paste0("<br>When dealing with two numeric variables, it may be useful
      to summarise the relationship between them through a correlation coefficient.<br>
      Pearson's correlation coefficient (assuming the relationship is linear) between '",
      labels.func(input$numvar1), "' and '", labels.func(input$numvar2),"' is ",
      round(with(dset(),cor(get(input$numvar1),get(input$numvar2))),2),
      ". If we are not prepared to assume a linear relationship,  we can report spearman's correlation coefficient, which is ",
      round(with(dset(),cor(get(input$numvar1),get(input$numvar2),method = "spearman")),2),"."
      ))
    }
  })
  output$graphch4 <- renderPlot({
    if (input$variablecombchoice=="num2") {
      par(mar=c(5,5,1,1))
      plot(x = with(dset(), get(input$numvar2)), 
        y = with(dset(), get(input$numvar1)),
        xlab=labels.func(input$numvar2), ylab=labels.func(input$numvar1))
    }
  })
  ###Chapter 5 and 6 - statistical inference
  #numeric outcome variable
  output$varnumericch56 <- renderUI({
    if (input$testchoice %in% c("ttest1","ttest2","ttestp","mw","wilcox1","wilcoxp")) {
      radioButtons("numvar1", "Numeric outcome variable:", 
        c("Height (cm)" = "height", "Number of successful flips from 20" = "flip20",
        "Number of successful flips in one minute" = "flipmin",
        "Time to 10 successful flips (seconds)" = "flip10sec"))
    }
  })
  output$vargroup2ch56 <- renderUI({
    if (input$testchoice %in% c("ttest2","chi","fishers","mw")) {
      radioButtons("catch4", "Grouping variable:", 
         c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
          "Successfully catching the last flip" = "flip2"))
    }
  })
  output$varcatch56 <- renderUI({
    if (input$testchoice %in% c("chi","fishers")) {
      radioButtons("groupch4", "Categoric outcome variable:", 
         c("Born in the UK" = "uk", "Successfully catching the first flip" = "flip1",
          "Successfully catching the last flip" = "flip2"))
    }
  })
  output$varpairingch56 <- renderUI({
    if (input$testchoice %in% c("ttestp","wilcoxp")) {
      radioButtons("numpair", "Pairing variable:", 
        c("Tallest/shortest within pair" = "tallest"))
    }
  })
  output$varmcnemars1ch56 <- renderUI({
    if (input$testchoice=="mcnemar") {
      radioButtons("mcnemar1", "Categoric variable 1:", 
        c("Successfully catching the first flip" = "flip1"))
    }
  })
  output$varmcnemars2ch56 <- renderUI({
    if (input$testchoice=="mcnemar") {
      radioButtons("mcnemar2", "Categoric variable 2:", 
        c("Successfully catching the last flip" = "flip2"))
    }
  })
  #input for hypothesis (default is zero, or the mean of the single variable)
  output$hyp <- renderUI({
    if (!(input$graphtype %in% c("chi","mcnemar"))) {
      numericInput("hyp", "Hypothesis:", 
        value = if (input$graphtype %in% c("ttest1","wilcox1")) {round(mean(with(dset(), get(input$numvar1))))} else {0}, 
        step = 1)
    }
  })
  output$textch56.ttest1 <- renderText({
    if (input$testchoice=="ttest1") {
      obsmean <- mean(with(dset(), get(input$numvar1)))
      obssd <- with(dset(),sd(get(input$numvar1),na.rm = T))
      sampsize <- with(dset(),sum(!is.na(get(input$numvar1))))
      obsse <- obssd / sqrt(sampsize)
      seaway <- (obsmean - input$hyp) / obsse
      pval <- round(t.test(dset1$height.tall)$p.value,3)
      if (pval<0.001) pval <- "<0.001"
      HTML(paste0("<br>The one sample t-test is appropriate for testing whether the mean 
        of a single sample of numeric data is compatible with some hypothesised mean.
        This is a parametric test and assumes the following:<ul><li>Sample size of at 
        least 20 (actual sample size is ",sum(!is.na(with(dset(),get(input$numvar1)))),
        ") </li><li>Normally distributed data (check the graphs tab to make this assessment)
        </li></ul> Hypothesised mean = ", input$hyp,
        "<br> Observed mean = ", round(obsmean,1),
        "<br> Standard error = SD / sqrt(n) = ", round(obssd,3),
        " / sqrt(",sampsize,") = ", round(obsse,3),
        "<br><br> The observed mean is (", round(obsmean,1)," - ",input$hyp,") / ",round(obsse,3),
        " = ", round(seaway,3) ," standard errors away from the hypothesised mean. ",
        "The corresponding p-value is ",pval,"."))
    }
  })
}

shinyApp(ui = ui, server = server)
