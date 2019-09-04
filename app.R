library(shiny)
library(markdown)
library(BAStag)
library(TwGeos)
library(SGAT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("VWSGapp", id = "navbar",
             
             tabPanel("Light data",

                      titlePanel("Select raw light recordings"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("filetype", 
                                       label = "Select your filetype before browsing for your file",
                                       choices = list(".lux",
                                                      ".lig"
                                       ),
                                       selected = ".lux"),
                          fileInput("filename",
                                    label = "Browse for your file",
                                    accept = c(".lux",
                                               ".lig")
                          ),
                          
                          numericInput("offset", 
                                       label = "Offset", 
                                       value = 1,  #default value
                                       step  = 1), #"steps" with arrow buttons.
                          br(),
                          
                          numericInput("lon.calib", 
                                       label = "Deployment longitude", 
                                       value = NULL, #default value
                                       step = 0.00001), #"steps" with arrow buttons.
                          numericInput("lat.calib", 
                                       label = "Deployment latitude",  
                                       value = NULL,
                                       step = 0.00001)
                        ),
                        
                        mainPanel(
                          h4("Draw rectangle to inspect specific light recordings"),
                          uiOutput("selectLightSeries"),
                          plotOutput("lightSeries")
                        )
                
                  ) ## end Sidebar Panel
                  
             ), ## tabPanel (Data)
             tabPanel("Twilight",
                      
                      titlePanel("Twilight Annotation"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("threshold", 
                                       label = h4("Threshold"), 
                                       value = 1,  #default value
                                       step  = 0.25), #"steps" with arrow buttons.
                          br(),
                          
                          conditionalPanel(
                            condition = "output.Step1 == true",
                            h4("1.Select range"),
                            actionButton("Step1", "accept", icon = icon("check"))
                          ),
                          
                          br(),
                          
                          conditionalPanel(
                            condition = "output.Step2 !== true",
                            h4("2. Twilight selection"),
                            actionButton("Step2", "accept", icon = icon("check"))
                          )
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "output.Step1 == true && output.Step2 == true",
                                h4("Draw rectangle to select range"),
                                plotOutput("lightRange", height = 50),
                                uiOutput("selectRangePlot")
                          ),
                            conditionalPanel(
                              condition = "output.Step2 !== true",
                              h4("Select points during the night"),
                              uiOutput("nightSelectPlot")
                          )
                          )
                      )
            ), ## tabPanel (Twilight)
            tabPanel("Calibration"),
            tabPanel("Breeding site"),
            tabPanel("Breeding performance"),
            tabPanel("Location estimates")
            
      ) ## navbarPage
) ## ui


server <- function(input, output, session) {
  
  values <- reactiveValues()
  
  #########################
  #Read in a dataset from a file.
  raw <- reactive({
    
    req(input$filename)
    inFile <- input$filename
    
    if (is.null(inFile)) {
      return(NULL)
      } else
    {
      if (input$filetype == ".lig") {
        return(NULL)
      } else 
      {
        if (input$filetype == ".lux") {
          
          tab <- readMTlux(inFile$datapath) #read the data into a dataframe called d.lig
          tab$Light<-log(tab$Light)
          
          output$Step1 <- reactive(ncol(raw())>0)
          outputOptions(output, c("Step1"), suspendWhenHidden = FALSE)
          
          return(tab)
          
        } 
      }
    }
   })
  
  observe({
    if(!is.null(input$filename)) {
      if(is.null(input$select_range_brush$xmin)) {
      min <- format(raw()$Date[1], "%Y-%m-%d")
      max <- format(raw()$Date[nrow(raw())], "%Y-%m-%d")
      } else {
        min <- format(as.POSIXct(input$select_range_brush$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        max <- format(as.POSIXct(input$select_range_brush$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      }
    } else {
      min <- "2000-01-01"
      max <- "2025-12-31"
    }
  })
  
  
  #############################
  #### Select Data Plots ######
  #############################
  
  output$lightImage0 <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
      
      if(all(!is.na(c(input$lon.calib, input$lat.calib)))) {
        tsimageDeploymentLines(raw()$Date, input$lon.calib, input$lat.calib, offset = input$offset,
                               lwd = 4, col = adjustcolor("orange", alpha.f = 0.6))
      }
    }
    
  })
  
  
  output$selectLightSeries  <- renderUI({
    plotOutput("lightImage0", height=400,
               brush = brushOpts(id = "select_range_brush0")
    )
  })
  
  output$lightSeries <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      
      if(!is.null(input$filename)) {
        if(is.null(input$select_range_brush0$xmin)) {
          min <- format(raw()$Date[1], "%Y-%m-%d")
          max <- format(raw()$Date[nrow(raw())], "%Y-%m-%d")
        } else {
          min <- format(as.POSIXct(input$select_range_brush0$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
          max <- format(as.POSIXct(input$select_range_brush0$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        }
      } else {
        min <- "2000-01-01"
        max <- "2025-12-31"
      }
      
      
      range <- as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
               as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d")
      
      with(raw()[range,], plot(Date, Light, type = "o", pch = 16, cex = 0.25, xlab = "", ylab = "log light", las = 1,
                               ylim = range(Light, na.rm = T)))
    }
    
  })

  
  #############################
  #### Select Range Plots #####
  #############################
  
  observe({
    if(!is.null(input$select_range_brush1)) {
      values$selectedRange <- input$select_range_brush1
    }
      output$Step2 <- reactive(input$Step1 %% 2 == 0)
      outputOptions(output, c("Step2"), suspendWhenHidden = FALSE)
  })
  
  
  output$lightRange <- renderPlot({
    
    if(is.null(input$filename) | (input$Step1 > 0 & input$Step1 %% 2 != 0)) {
      opar <- par(mar = c(0, 4, 1, 2))
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
      par(opar)
    } else {
      
      if(!is.null(values$selectedRange)) {
        
        min <- format(as.POSIXct(values$selectedRange$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        max <- format(as.POSIXct(values$selectedRange$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        
        range <- which(as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
                       as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d"))
        opar <- par(mar = c(0, 4, 1, 2))
        plot(NA, xlim = c(1, nrow(raw())), ylim = c(0,1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", xaxs = "i")
        rect(min(range), 0, max(range), 1, col = "firebrick", border = NA)
        par(opar)
        
      } else {
        
        opar <- par(mar = c(0, 4, 1, 2))
        plot(NA, xlim = c(1, nrow(raw())), ylim = c(0,1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", xaxs = "i")
        rect(1, 0, nrow(raw()), 1, col = "firebrick", border = NA)
        par(opar)
        
      }
    }
    
  })
  
  
  output$lightImage1 <- renderPlot({
    
    if(is.null(input$filename) ) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      
      if(!is.null(values$selectedRange) & input$Step1 %% 2 != 0) {
        
        min <- format(as.POSIXct(values$selectedRange$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        max <- format(as.POSIXct(values$selectedRange$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        
        range <- as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
                 as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d")
        
        raw_select <- raw()[range,]
        opar <- par(mar = c(3, 4, 1, 2))
        lightImage(raw_select, offset = input$offset, zlim = c(0, 10), dt = 120)
        par(opar)
      } else {
        raw_select <- raw()
        opar <- par(mar = c(3, 4, 1, 2))
        lightImage(raw_select, offset = input$offset, zlim = c(0, 10), dt = 120)
        par(opar)
      }
    }
    
  })
  
  
  output$selectRangePlot <- renderUI({
    plotOutput("lightImage1", height=500,
               brush = brushOpts(id = "select_range_brush1", resetOnNew = TRUE)
    )
  })
  
  
  observe({
    if(input$Step1 %% 2 != 0) {
    
    updateActionButton(session, "Step1",
                       label = "reset",
                       icon = icon("times"))
    } else {
    updateActionButton(session, "Step1",
                       label = "accept",
                       icon = icon("check"))
    }
  })
  
  
  raw_select <- reactive({
    
    if(!is.null(values$selectedRange) & input$Step1 %% 2 != 0) {
      
      min <- format(as.POSIXct(values$selectedRange$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      max <- format(as.POSIXct(values$selectedRange$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      
      range <- as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
        as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d")
      
      raw_select <- raw()[range,]
      
      return(raw_select)
      
    } else return(NULL)
    

  })
  
  
  #############################
  #### Twilight Annotation ####
  #############################
  
  output$nightSelect <- renderPlot({
    
    if(is.null(input$filename) | input$Step1 %% 2 != 0) {
      
      opar <- par(mar = c(3, 4, 1, 2))
      lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
      par(opar)
        
    }
    
  })
  
  output$nightSelectPlot <- renderUI({
    plotOutput("nightSelect", height=400,
               brush = brushOpts(id = "select_range_brush1", resetOnNew = TRUE)
    )
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

