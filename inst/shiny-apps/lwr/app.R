library(shiny)

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Decomposition of Time Series with Locally Weighted Regression"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            fileInput("upload", "Upload a data file", buttonLabel = "Upload...", multiple = FALSE),
            checkboxInput("logtr", "Log-tranformation?", value = FALSE),          
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
            actionButton("automate", "Automatically select optimal bandwidth"),
            sliderInput("height", "Figure height:", min = 100, max = 1000, step = 20, value = 700),
            downloadButton("dlData", label = "Download decomposed data")            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          uiOutput("uiOut")
        )
    )
)

# Define server logic
server <- function(input, output) {

    dataset <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(
        ext,
        csv = read.csv(input$upload$datapath),
        txt = read.table(input$upload$datapath),
        validate("Infalid file type provided. Please only upload .csv or .txt files.")
      )
    })
  
    output$uiOut <- renderUI({
      req(est_result())
      req(input$height)
      plotOutput("distPlot", height = paste0(input$height, "px"))
    })
  
    Xt <- reactive({
      out <- DeSeaTS::HOUSESALES
      if (isTRUE(input$logtr)) {
        out <- log(out)
      }
      out
    })
  
    opts <- reactive({
      req(input$opoly)
      req(input$kfun)
      req(input$bwidth)
      DeSeaTS::set_options(order_poly = input$opoly,
                           kernel_fun = input$kfun,
                           bwidth = input$bwidth)
    })
  
    bwidth <- reactive({
      req(input$automate)
      
    })
  
    observeEvent(input$automate, {
      req(Xt())
      req(opts())
      op <- opts()
      op@bwidth <- NA_real_
      est <- DeSeaTS::deseats(Xt(), op, bwidth_start = 0.1)
      bopt <- est@bwidth
      updateSliderInput(inputId = "bwidth", value = bopt)
    })
  
    est_result <- reactive({
        req(input$bwidth)
        req(opts)
        est <- DeSeaTS::deseats(Xt(), opts())  
        est
    })  
  
    output$distPlot <- renderPlot({

        req(est_result())
        
        p1 <- autoplot(est_result(), which = 3) +
          ggplot2::ggtitle("Observations together with fitted values") +
          ggplot2::theme_bw() +
          ggplot2::theme(legend.position = "bottom",
                         plot.title = ggplot2::element_text(hjust = 0.5)) +
          ggplot2::ylab(" ")
        p2 <- autoplot(est_result(), which = 4) +
          ggplot2::ggtitle("Residual series") +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2, align = "v",
                          common.legend = FALSE)

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
