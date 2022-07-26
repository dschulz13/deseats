library(shiny)

month_f <- function(month) {
  switch(month,
    January = 1,
    February = 2,
    March = 3,
    April = 4,
    May = 5,
    June = 6,
    July = 7,
    August = 8,
    September = 9,
    October = 10,
    November = 11,
    December = 12
  )
}

quarter_f <- function(quarter) {
  switch(quarter,
    Q1 = 1,
    Q2 = 2,
    Q3 = 3,
    Q4 = 4
  )
}

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
                 uiOutput("freq"),
                 uiOutput("startdate"),
                 uiOutput("logtransf")
              ),
              tabPanel("II. Decomposition",
                 uiOutput("decompmethod"),
                 uiOutput("decompsettings")
              ),
              tabPanel("III. Data export",
                 downloadButton("dlData", label = "Download decomposed data") 
              )
            )#,
            
            # ,
            # downloadButton("dlData", label = "Download decomposed data")            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Data frames",
              uiOutput("nentries"),
              tabsetPanel(
                tabPanel("Input data",
                  uiOutput("dfinput")  
                ),
                tabPanel("Decomposed data",
                  uiOutput("dfoutput")  
                )
              )
            ),
            tabPanel("Plots",
              sliderInput("figheight", "Plot height:", min = 100, max = 1800, step = 20, value = 500),
              sliderInput("figwidth", "Plot width:", min = 100, max = 1800, step = 20, value = 1200),                     
              tabsetPanel(
                tabPanel("Plot of observations",
                  uiOutput("obsplot")
                ),              
                tabPanel("Fitted values",
                  uiOutput("fitplot")      
                ),
                tabPanel("Residuals",
                  uiOutput("resplot")
                )
              )
              
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
    
    output$nentries <- renderUI({
      req(dataset())
      sliderInput("nvals", "Entries to show:", min = 1, max = length(dataset()[, 1]), step = 1, value = 5)
    })
    
    output$dfinput <- renderUI({
      req(dataset())
      tableOutput("datatable")   
    })
    
    output$datatable <- renderTable({
      req(dataset())
      req(input$nvals)
      print.data.frame(dataset()[1:input$nvals, , drop = FALSE])
    })
    
    choices <- reactive({
      req(dataset())
      c("", names(dataset()))
    })
    
    output$varselection <- renderUI({
      req(choices())
      
      list(
        selectInput("tsvar", "Select time series variable:", 
                    choices = choices(), selected = choices()[[1]])       
      )
    })
    
    output$freq <- renderUI({
      req(input$tsvar)
      
      sliderInput("frequ", "Seasonal period:", min = 4, max = 12, step = 8,
                  value = 4)
    })
    
    output$startdate <- renderUI({
      req(input$frequ)
      
      a <- sliderInput("year", "Year of the first observation:", min = 1800, max = 2020,
                  value = 1950, step = 1, sep = "")
      if (input$frequ == 4) {
        b <- selectInput("subd", "Quarter of the first observation:",
                         choices = paste0("Q", 1:4), selected = "Q1")
      } else if (input$frequ == 12) {
        b <- selectInput("subd", "Month of the first observation:",
                         choices = month.name, selected = "January")        
      }
      list(a, b)
    })
    
    tseries <- reactive({
      req(input$subd)
      
      subd <- input$subd
      if (input$frequ == 4) {
        subd <- quarter_f(subd)
      } else if (input$frequ == 12) {
        subd <- month_f(subd)
      }
      y <- as.character(input$tsvar)
      
      yt <- ts(dataset()[[y]], start = c(input$year, subd), frequency = input$frequ)
      yt
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
      
      if (input$method == "Data-driven locally weighted regression") {
        out <- list(
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
      } else if (input$method == "BV4.1") {
        out <- list()
      }
      out

    })    
  
    opts <- reactive({
      req(input$opoly)
      req(input$kfun)
      req(input$bwidth)
      deseats::set_options(order_poly = input$opoly,
                           kernel_fun = input$kfun,
                           bwidth = input$bwidth)
    })
    
    observeEvent(input$automate, {
      req(ts_tr())
      req(opts())
      op <- opts()
      op@bwidth <- NA_real_
      est <- deseats::deseats(ts_tr(), op, bwidth_start = 0.1)
      bopt <- est@bwidth
      updateSliderInput(inputId = "bwidth", value = bopt)
    })
  
    est_result <- reactive({
      
        req(input$method)
      
        if (input$method == "Data-driven locally weighted regression") {
          req(input$bwidth)
          req(opts)
          est <- deseats::deseats(ts_tr(), opts())           
        } else if (input$method == "BV4.1") {
          est <- deseats::BV4.1(ts_tr())
        }
 
        est
    })  
  
    output$fitplot <- renderUI({
      req(est_result())
      plotOutput("plotfitted", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })

    output$plotfitted <- renderPlot({

        req(est_result())
        
        deseats::autoplot(est_result(), which = 3) +
          ggplot2::ggtitle(paste0('Observations "', ts_name_tr(), '" togerther with estimated trend and seasonality')) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
          ggplot2::ylab("Observation and trend + seasonality")


    })    
    
    output$resplot <- renderUI({
      req(est_result())
      plotOutput("plotresid", height = paste0(input$figheight, "px"), width = paste0(input$figwidth, "px"))
    })        
    
    output$plotresid <- renderPlot({

        req(est_result())
        
        deseats::autoplot(est_result(), which = 4) +
          ggplot2::ggtitle(paste0('Estimated remainder in "', ts_name_tr(), '"')) +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")) +
          ggplot2::ylab("Detrended and deseasonalized observation")


    })
    
    output$dfoutput <- renderUI({
      req(est_result())
      req(input$nvals)
      
      tableOutput("outp")
    })
    
    dfout <- reactive({
      req(est_result)
      
      .df <- as.data.frame(est_result()@decomp)
      if (isTRUE(input$logtransf)) {
        .df <- exp(.df)
      } else if (isFALSE(input$logtransf)) {
        .df <- .df
      }
      .df$`Time point` <- stats::time(est_result()@decomp)
      .df <- .df[c(5, 1:4)]      
      .df
    })
    
    output$outp <- renderTable({
      req(dfout())
      req(input$nvals)

      print.data.frame(dfout()[1:input$nvals, , drop = FALSE])
    })
    
    output$dlData <- downloadHandler(
      filename = function() {
        paste0("decomposed data - ",  input$tsvar, " - ", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(dfout())
        write.table(dfout(), file, quote = FALSE, sep = ",", row.names = FALSE,
                    col.names = TRUE)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
