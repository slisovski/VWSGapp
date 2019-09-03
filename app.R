library(shiny)
library(markdown)
library(TwGeos)
library(SGAT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  navbarPage("VWSGapp",
             
             tabPanel("Light data",
                      

                      titlePanel("Select your data"),
                      

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
                                      min   = as.Date("2016-01-01","%Y-%m-%d"),
                                      max   = as.Date("2016-12-01","%Y-%m-%d"),
                                      value = c(as.Date("2016-02-01"), as.Date("2016-11-01")),
                                      timeFormat="%Y-%m-%d")
                          )
                        ),
                        
                        mainPanel(
                          plotOutput("lightImage0"),
                          plotOutput("lightSeries")
                        )
                        
                        
                
                
                  ) ## end Sidebar Panel 
             ) ## tabPanel
             
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
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
    if (is.null(inFile)) {return(NULL)} else
    {
      if (input$filetype == ".lig") {
        return(NULL)
      } else 
      {
        if (input$filetype == ".lux") {
          
          raw <- readMTlux(inFile$datapath) #read the data into a dataframe called d.lig
          raw$Light<-log(raw$Light)
          
          output$dateSlider <- reactive(ncol(raw())>0)
          outputOptions(output, c("dateSlider", "min"), suspendWhenHidden = FALSE)
          
          return(raw)
          
        } 
      }
    }
   })
  
  
  output$lightImage0 <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      offset <- 1
      lightImage(raw(), offset = offset, zlim = c(0, 10), dt = 120)
      
      if(all(!is.na(c(input$lon.calib, input$lat.calib)))) {
        tsimageDeploymentLines(raw()$Date, input$lon.calib, input$lat.calib, offset = offset,
                               lwd = 4, col = adjustcolor("orange", alpha.f = 0.6))
      }
    }
    
  })
  
  
  output$lightSeries <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      plot(raw()$Date, raw()$Light, type = "o", pch = 16, cex = 0.25, xlab = "", ylab = "log light", las = 1)
    }
    
  })
 
  
   
}








# Run the application 
shinyApp(ui = ui, server = server)

