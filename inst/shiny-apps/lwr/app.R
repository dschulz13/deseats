library(shiny)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Decomposition of Time Series with Locally Weighted Regression"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
              tabPanel("I. Data import",
                 fileInput("upload", "Upload a data file", buttonLabel = "Upload...", multiple = FALSE),
                 uiOutput("filesettings"),
                 uiOutput("varselection"),
                 uiOutput("logtransf")
              ),
              tabPanel("II. Decomposition",
                 uiOutput("decompmethod"),
                 uiOutput("decompsettings")
              ),
              tabPanel("III. Data export",
                 uiOutput("export")
              ),
              tabPanel("Extra: Plot settings",
                 sliderInput("figheight", "Plot height:", min = 100, max = 1800, step = 20, value = 600),
                 sliderInput("figwidth", "Plot width:", min = 100, max = 1800, step = 20, value = 1200)      
              )
            )#,
            
            # ,
            # downloadButton("dlData", label = "Download decomposed data")            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Data frame",
              tableOutput("datatable")         
            ),
            tabPanel("Plot of observations",
              uiOutput("obsplot")
            ),
            tabPanel("Plot of fitted values",
              uiOutput("fitplot")
            )            
          )
        )
    )
)

# Define server logic
server <- function(input, output) {
  
    output$filesettings <- renderUI({
      req(input$upload)
      list(
        checkboxInput("header", label = "Header?", value = TRUE),
        selectInput("sep", "Separator:", choices = c(",", ";", "."), selected = ",")
      )
    })
    
    dataset <- reactive({
      req(input$sep)
      req(input$header)
      
      ext <- tools::file_ext(input$upload$name)
      switch(
        ext,
        csv = read.csv(input$upload$datapath, header = input$header, sep = input$sep),
        txt = read.table(input$upload$datapath, header = input$header, sep = input$sep),
        validate("Infalid file type provided. Please only upload .csv or .txt files.")
      )
    })    
    
    output$datatable <- renderTable({
      req(dataset())
      print.data.frame(dataset())
    })
    
    choices <- reactive({
      req(dataset())
      c("", names(dataset()))
    })
    
    output$varselection <- renderUI({
      req(choices())
      
      list(
        selectInput("tsvar", "Select time series variable:", 
                    choices = choices(), selected = choices()[[1]]),
        selectInput("timevar", "Select time variable:", 
                    choices = choices(), selected = choices()[[1]])        
      )
    })
    
    tseries <- reactive({
      req(input$tsvar)
      req(input$timevar)
      
      t <- as.character(input$timevar)
      y <- as.character(input$tsvar)
      
      yt <- dataset()[[y]]
      tt <- as.Date(dataset()[[t]], 
                    tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y"))
      
      out <- zoo::zoo(yt, tt)
      out
    })
    
    output$logtransf <- renderUI({
      req(tseries())
      list(
        checkboxInput("logs", label = "Log-transformation?", value = FALSE)
      )
    })
    
    ts_tr <- reactive({
      req(tseries())
      
      if (isTRUE(input$logs)) {
        out <- log(tseries())
      } else if (isFALSE(input$logs)) {
        out <- tseries()
      }
      out
    })
    
    ts_name_tr <- reactive({
      req(ts_tr())
      
      if (isTRUE(input$logs)) {
        out <- paste0("log(", input$tsvar, ")")
      } else if (isFALSE(input$logs)) {
        out <- input$tsvar
      }
      out      
    })
    
    output$obsplot <- renderUI({
      req(ts_tr())
      req(input$figheight)
      req(input$figwidth)
      
      plotOutput("plotobs", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })
    
    output$plotobs <- renderPlot({
      req(ts_tr())
      req(input$figheight)
      req(input$figwidth)      
      
      zoo::autoplot.zoo(ts_tr()) +
        ggplot2::theme_bw() +
        ggplot2::ylab(ts_name_tr()) +
        ggplot2::xlab("Time") +
        ggplot2::ggtitle(paste0('Plot of the time series "', ts_name_tr(), '"')) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
    })
    
    output$decompmethod <- renderUI({
      req(tseries())
      selectInput("method", "Decomposition method:",
                  choices = c("Data-driven locally weighted regression", "BV4.1"),
                  selected = "Data-driven locally weighted regression")
    })
    
    output$decompsettings <- renderUI({
      req(input$method)
      list(
            sliderInput("opoly",
                        "Order of local polynomial (for the trend):",
                        min = 1,
                        max = 3,
                        value = 1,
                        step = 2),
            selectInput("kfun",
                        "Kernel function:",
                        choices = c(
                          "uniform",
                          "epanechnikov",
                          "bisquare",
                          "triweight"
                        ), selected = "epanechnikov", multiple = FALSE),  
            sliderInput("bwidth",
                        "Bandwidth:",
                        min = 0.01,
                        max = 0.4,
                        value = 0.1,
                        step = 0.005),
            actionButton("automate", "Automatically select optimal bandwidth")        
      )
    })    
  
    opts <- reactive({
      req(input$opoly)
      req(input$kfun)
      req(input$bwidth)
      deseats::set_options(order_poly = input$opoly,
                           kernel_fun = input$kfun,
                           bwidth = input$bwidth)
    })
  
    ts_ts <- reactive({
      #t <- zoo::as.Date.ts(time(ts_tr()))
      ts(zoo::coredata(ts_tr), frequency = 4, start = 1970)#, start = t[[1]], end = tail(t, 1))
    })
    
    observeEvent(input$automate, {
      req(ts_tr())
      req(opts())
      op <- opts()
      op@bwidth <- NA_real_
      est <- deseats::deseats(ts_ts(), op, bwidth_start = 0.1)
      bopt <- est@bwidth
      updateSliderInput(inputId = "bwidth", value = bopt)
    })
  
    est_result <- reactive({
        req(input$bwidth)
        req(opts)
        est <- deseats::deseats(ts_ts(), opts())  
        est
    })  
  
    output$fitplot <- renderUI({
      req(est_result())
      plotOutput("plotfitted", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })
    
    output$plotfitted <- renderPlot({

        req(est_result())
        
        autoplot(est_result(), which = 3) +
          ggplot2::ggtitle("Observations together with fitted values") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5)) +
          ggplot2::ylab(" ")


    })
    
    output$dlData <- downloadHandler(
      filename = function() {
        paste0("decomposed data - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        .t <- time(est_result()@decomp)
        .df <- as.data.frame(est_result()@decomp)
        .df[["Time"]] <- .t
        .df <- .df[, c(5, 1:4)]
        write.table(.df, file, quote = FALSE, sep = ",", row.names = FALSE,
                    col.names = TRUE)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
