library(shiny)
library(markdown)
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
                                       step = 0.00001),
                            
                          conditionalPanel(
                            condition = "output.dateSlider == true",
                            sliderInput("Dates",
                                      "Dates:",
                                      min   = as.Date("1970-01-01", "%Y-%m-%d"),
                                      max   = as.Date("1970-12-30", "%Y-%m-%d"),
                                      value = as.Date(c("1970-01-01", "1970-12-30"), "%Y-%m-%d"),
                                      timeFormat="%Y-%m-%d")
                            
                        )
                          
                        ),
                        
                        mainPanel(
                          plotOutput("lightImage0"),
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
                            condition = "output.dateSlider == true",
                            h4("1. Step: Select range"),
                            actionButton("Step1.1", "accept"),
                            actionButton("Step1.2", "reset")
                          )
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "output.dateSlider == true",
                                h4("Draw rectangle to select range"),
                                uiOutput("selectRangePlot")
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
  
  
  #########################
  #Read in a dataset from a file.
  raw <- reactive({
    
    #req() ensures that if file hasn't been read in yet,
    #the rest of the code doesn't crash with errors.
    #https://shiny.rstudio.com/articles/req.html
    req(input$filename)
    inFile <- input$filename
    
    #Nesting ifelse shows what to do if inFile is null (no entry)
    #and what to do for each input radio button type.
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
          
          output$dateSlider <- reactive(ncol(raw())>0)
          outputOptions(output, c("dateSlider"), suspendWhenHidden = FALSE)
          
          return(tab)
          
        } 
      }
    }
   })
  
  observe({
    if(!is.null(input$filename)) {
      min <- format(raw()$Date[1], "%Y-%m-%d")
      max <- format(raw()$Date[nrow(raw())], "%Y-%m-%d")
    } else {
      min <- "2000-01-01"
      max <- "2025-12-31"
    }
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "Dates",
                      min   = as.Date(min, "%Y-%m-%d"),
                      max   = as.Date(max, "%Y-%m-%d"),
                      value = as.Date(c(min, max), "%Y-%m-%d"))
    updateSliderInput(session, "Range",
                      min   = as.Date(min, "%Y-%m-%d"),
                      max   = as.Date(max, "%Y-%m-%d"),
                      value = as.Date(c(min, max), "%Y-%m-%d"))
  })
  
  output$lightImage0 <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
      
      if(all(!is.na(c(input$lon.calib, input$lat.calib)))) {
        tsimageDeploymentLines(raw()$Date, input$lon.calib, input$lat.calib, offset = input$offset,
                               lwd = 4, col = adjustcolor("orange", alpha.f = 0.6))
      }
      
      abline(v = as.POSIXct(input$Dates, format = "%Y-%m-%d"), lty = 2, lwd = 2, col = adjustcolor("orange", alpha.f = 0.7))
    }
    
  })
  
  
  output$lightImage1 <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
    }
    
  })
  
  
  output$selectRangePlot <- renderUI({
    plotOutput("lightImage1", height=500,
               brush = brushOpts(id = "select_range_brush")
    )
  })
  
  
  output$lightSeries <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
        
      range <- as.POSIXct(raw()$Date) > as.POSIXct(input$Dates[1], format = "%Y-%m-%d") &
               as.POSIXct(raw()$Date) < as.POSIXct(input$Dates[2], format = "%Y-%m-%d")
      
      with(raw()[range,], plot(Date, Light, type = "o", pch = 16, cex = 0.25, xlab = "", ylab = "log light", las = 1,
                                 ylim = range(Light, na.rm = T)))
    }
    
  })
}




# Run the application 
shinyApp(ui = ui, server = server)

