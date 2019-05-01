list.of.packages <-
  c("shiny",
    "ggplot2",
    "RColorBrewer",
    "PMCMR",
    "shinyjs",
    "shinydashboard",
    "readxl")
new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(PMCMR)
library(shinyjs)
library(shinydashboard)
library(readxl)

theme_set(theme_classic(base_size = 14))
ui <- fluidPage(
  titlePanel(fluidRow(
    column(
      8,
      HTML(
        "<h1><b>Vi</b>sualization and <b>St</b>atistical <b>A</b>nalysis"
      )
    ),
    column(3, img(height = 80, src = "expresso_alpha.png"), align = "right")
  )),
  tags$head(tags$style(
    HTML("hr {border-top: 1px solid #000000;}")
  )),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", ("Select file")),
      textInput("Label", ("Enter Label for file")),
      actionButton("addFile", "Upload"),
      checkboxInput("nonFeeders", "Remove non-feeding flies", FALSE),
      actionButton("refresh", "Refresh"),
      hr(),
      h4("Number of data points (flies):"),
      tableOutput("n_table"),
      hr(),
      downloadButton("download", " Download all plots")
    ),
    
    
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Loaded Data", textOutput("n"), tableOutput("contents")),
        tabPanel("Total Volume", plotOutput("totVol")),
        tabPanel("Average Volume", plotOutput("avVol")),
        tabPanel("Latency", plotOutput("latency")),
        tabPanel("Events", plotOutput("events")),
        tabPanel("Percent Fed", plotOutput("percFed")),
        tabPanel("Boutrate", plotOutput("boutrate")),
        tabPanel(
          "Significance Tests",
          br(),
          radioButtons(
            "sig_test",
            "Choose the feature to perform significance testing on:",
            c(
              "Total volume" = "sig_tot",
              "Latency" = "sig_lat",
              "Average volume" = "sig_ave",
              "Events" = "sig_eve",
              "Bout Rate" = "sig_br"
            )
          ),
          textOutput("tests"),
          br(),
          tableOutput("test_table")
        )
        
      )
    )
    
  )
  
  
)


appendedDataFrame <- function(df, Label) {
  labelList <- rep(Label, nrow(df))
  return(labelList)
}



server <- function(input, output) {
  feed_data <- reactiveValues(data = NULL)
  feed_data_eaters <- reactiveValues(data = NULL)
  feed_data_all <- reactiveValues(data = NULL)
  bout_data <- reactiveValues(data = NULL)
  box_width = 0.35
  observeEvent(input$addFile,
               {
                 if (is.null(input$file))
                   return(NULL)
                 inFile <- input$file
                 feed_data_temp <-
                   read_excel(inFile$datapath, sheet = 'Summary')
                 #adding data for bout rate (this quantity is always calculated for the eaters)
                 feed_data_temp_events <-
                   read_excel(inFile$datapath, sheet = 'Events')
                 feed_data_temp_events$boutrate <-
                   feed_data_temp_events$`Volume [nL]` / feed_data_temp_events$DurationIdx
                 bout_mean <-
                   aggregate(
                     feed_data_temp_events$boutrate,
                     by = list(
                       feed_data_temp_events$Filename,
                       feed_data_temp_events$XP,
                       feed_data_temp_events$Channel
                     ),
                     mean
                   )
                 bout_mean$label <-
                   appendedDataFrame(bout_mean, input$Label)
                 bout_data$data <- rbind(bout_data$data, bout_mean)
                 feed_data_temp$label <-
                   appendedDataFrame(feed_data_temp, input$Label)
                 feed_data_temp$AveBoutVol = feed_data_temp$`Total Volume [nL]` / feed_data_temp$`Number of Events`
                 feed_data_temp$AveBoutVol[is.nan(feed_data_temp$AveBoutVol)] = 0
                 
                 feed_data$data <-
                   rbind(feed_data$data, feed_data_temp)
                 feed <- feed_data$data #to initialize
                 feed_data_eaters$data <-
                   feed[feed$`Total Volume [nL]` != 0, ]
                 feed_data_all$data <- feed
               })
  
  
  observeEvent(input$refresh, {
    if (input$nonFeeders == TRUE)
      feed_data$data <- feed_data_eaters$data
    else if (input$nonFeeders == FALSE)
      feed_data$data <- feed_data_all$data
  })
  
  
  
  #To display the uploaded dataset with labels
  data_table <-
    eventReactive(c(input$addFile, input$refresh), {
      feed_data$data
      #bout_data$data
    })
  output$contents <- renderTable({
    data_table()
  })
  
  #To get the N values
  n_data <- eventReactive(c(input$addFile, input$refresh), {
    if (is.null(input$file))
      return(NULL)
    feed <- feed_data$data
    counts <- data.frame(table(feed$label))
    colnames(counts) <- c("Type", "n")
    return(counts)
  })
  output$n_table <- renderTable(n_data())
  
  #Plot for percent fed
  data_plot_percFed <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      count_all <-
        data.frame(table((feed_data_all$data)$label))
      count_eaters <-
        data.frame(table((feed_data_eaters$data)$label))
      df = merge(count_all, count_eaters, by = "Var1")
      colnames(df) <-
        c("label", "num_all", "num_fed")
      df$percfed <- df$num_fed / df$num_all
      ggplot(df, aes(
        x = as.factor(df$label),
        y = df$percfed,
        fill = label
      )) + geom_bar(
        stat = "identity",
        width = box_width,
        color = "black",
        size = 1
      ) + xlab("Label") + ylab("Percent of flies fed") + scale_fill_brewer(palette =
                                                                             "Pastel2") + ylim(0, 1) + theme(legend.position = "none")
    })
  output$percFed <- renderPlot({
    data_plot_percFed()
  })
  
  #Plot for events
  data_plot_events <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      ggplot(feed,
             aes(
               x = as.factor(feed$label),
               y = feed$`Number of Events`,
               fill = label
             )) + geom_boxplot(width = box_width, lwd = 1) + xlab("Label") + ylab("Number of meal bouts") +
        scale_fill_brewer(palette = "Pastel2")
      
    })
  output$events <- renderPlot({
    data_plot_events()
  })
  
  #Plot for total volume
  data_plot_totvol <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      ggplot(feed,
             aes(
               x = as.factor(feed$label),
               y = feed$`Total Volume [nL]`,
               fill = label
             )) + geom_boxplot(width = box_width, lwd = 1) + xlab("Label") + ylab("Total consumption per fly [nL]") +
        scale_fill_brewer(palette = "Pastel2")
      
    })
  output$totVol <- renderPlot({
    data_plot_totvol()
  })
  
  #Plot for latency
  data_plot_latency <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      ggplot(feed, aes(
        x = as.factor(feed$label),
        y = feed$Latency,
        fill = label
      )) + geom_boxplot(width = box_width, lwd = 1) + xlab("Label") + ylab("Latency [s]") +
        scale_fill_brewer(palette = "Pastel2")
      
    })
  output$latency <- renderPlot({
    data_plot_latency()
  })
  
  #Plot for average volume
  data_plot_avVol <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      ggplot(feed, aes(
        x = as.factor(feed$label),
        y = feed$AveBoutVol,
        fill = label
      )) + geom_boxplot(width = box_width, lwd = 1) + xlab("Label") + ylab("Average bout volume [nL]") +
        scale_fill_brewer(palette = "Pastel2")
      
    })
  output$avVol <- renderPlot({
    data_plot_avVol()
  })
  
  #Plot for bout rate
  data_plot_boutrate <-
    eventReactive(c(input$addFile, input$refresh), {
      if (is.null(input$file))
        return(NULL)
      brdat <- bout_data$data
      ggplot(brdat, aes(
        x = as.factor(brdat$label),
        y = brdat$x,
        fill = label
      )) + geom_boxplot(width = box_width, lwd = 1) + xlab("Label") + ylab("Average bout rate [nl/s]") + ylim(0,15)+
        scale_fill_brewer(palette = "Pastel2")
      
    })
  output$boutrate <- renderPlot({
    data_plot_boutrate()
  })
  
  #for significance testing
  
  significance_test <-
    eventReactive(c(input$addFile, input$refresh, input$sig_test), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      if (length(unique(feed$label)) < 2)
        return(NULL)
      opt_all = get_choice()
      t = kruskal.test(opt_all$opt_data ~ as.factor(opt_all$opt_label))
      paste("P-value for Kruskal-Wallis rank sum test is ", t$p.value)
      #posthoc.kruskal.nemenyi.test(x= feed$Total.Volume..nL., g=as.factor(feed$label) , method = "Turkey")
    })
  
  
  significance_test_posthoc <-
    eventReactive(c(input$addFile, input$refresh, input$sig_test), {
      if (is.null(input$file))
        return(NULL)
      feed <- feed_data$data
      if (length(unique(feed$label)) < 2)
        return(NULL)
      opt_all = get_choice()
      t = posthoc.kruskal.nemenyi.test(x = opt_all$opt_data ,
                                       g = as.factor(opt_all$opt_label) ,
                                       method = "Turkey")
      classes =  c(unlist(dimnames((t$p.value))[1]))
      op = data.frame(t$p.value)
      cbind(classes, op)
      
      
    })
  
  get_choice <-
    eventReactive(c(input$addFile, input$refresh, input$sig_test), {
      feed <- feed_data$data
      opt_data = switch(
        input$sig_test,
        sig_vol = feed$`Total Volume [nL]`,
        sig_lat = feed$Latency,
        sig_ave = feed$AveBoutVol,
        sig_eve = feed$`Number of Events`,
        sig_br = (bout_data$data)$x,
        feed$`Total Volume [nL]`
      )
      opt_label = switch(
        input$sig_test,
        sig_vol = feed$label,
        sig_lat = feed$label,
        sig_ave = feed$label,
        sig_eve = feed$label,
        sig_br = (bout_data$data)$label,
        feed$label
      )
      opt_combined <- list(opt_label = opt_label, opt_data = opt_data)
      
    })
  output$tests <- renderText({
    significance_test()
  })
  output$test_table <-
    renderTable({
      (significance_test_posthoc())
    }, digits = 6)
  
  #download all plots
  output$download <-
    downloadHandler(
      filename = function() {
        paste('Vista-', Sys.Date(), Sys.time(), '.pdf', sep = '')
      },
      contentType = "application/pdf",
      content = function(file) {
        pdf(paste(file))
        print(data_plot_percFed())
        print(data_plot_events())
        print(data_plot_latency())
        print(data_plot_totvol())
        print(data_plot_avVol())
        print(data_plot_boutrate())
        dev.off()
      }
    )
  
  
}
shinyApp(ui = ui, server = server)
