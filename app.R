library(shiny)
library(markdown)
# library(BAStag)
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
                            condition = "output.accept_range == true",
                            h4("1.Select range"),
                            actionButton("accept_range", "accept", icon = icon("check"))
                          ),
                          
                          br(),
                          
                          conditionalPanel(
                            condition = "output.Step2 !== true",
                            h4("2. Twilight selection"),
                            actionButton("accept_twl", "accept", icon = icon("check"))
                          ),
                          
                          br(),
                          
                          conditionalPanel(
                            condition = "output.Step3 == true",
                            h4("3. Twilight edit"),
                            actionButton("accept_edit", "accept", icon = icon("check"))
                          )
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "output.accept_range == true && output.Step2 == true",
                                plotOutput("lightRange", height = 50),
                                uiOutput("selectRangePlot")
                          ),
                            conditionalPanel(
                              condition = "output.Step2 !== true && output.Step3 !== true",
                              plotOutput("nightSelect", height=400,
                                         click = clickOpts(id = "night_click")),
                              plotOutput("twilights", height=400)
                          )
                          ,
                          conditionalPanel(
                            condition = "output.Step3 == true",
                            plotOutput("twilightAll", height=400),
                            plotOutput("selectedTwilight", height=400)
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
  
  #############################
  #### Read Data ##############
  ##########################
  
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
          
          output$accept_range <- reactive(ncol(raw())>0)
          outputOptions(output, c("accept_range"), suspendWhenHidden = FALSE)
          
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
  ##########################
  
  output$lightImage0 <- renderPlot({
    
    if(is.null(input$filename)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
      mtext("Draw rectangle to inspect period of light recordings", cex = 1.5, line = 3)
      
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
  ##########################
  
  selectedRange <- reactiveValues(min = NULL, max = NULL)
  
  observe({
    if(!is.null(input$select_range_brush1)) {
      selectedRange$min <- input$select_range_brush1$xmin
      selectedRange$max <- input$select_range_brush1$xmax
    }
      output$Step2 <- reactive(input$accept_range %% 2 == 0)
      outputOptions(output, c("Step2"), suspendWhenHidden = FALSE)
    
    if(input$accept_range>0 & (input$accept_range %% 2 == 0)) {
      selectedRange$min <- NULL
      selectedRange$max <- NULL
    }
  })
  
  
  output$lightRange <- renderPlot({
    
    if(is.null(input$filename) | (input$accept_range > 0 & input$accept_range %% 2 != 0)) {
      opar <- par(mar = c(0, 4, 1, 2))
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
      par(opar)
    } else {
      
      if(!is.null(selectedRange$min)) {
        
        min <- format(as.POSIXct(selectedRange$min, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        max <- format(as.POSIXct(selectedRange$max, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        
        range <- which(as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
                       as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d"))
        opar <- par(mar = c(0, 4, 2, 2))
        plot(NA, xlim = c(1, nrow(raw())), ylim = c(0,1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", xaxs = "i")
        rect(min(range), 0, max(range), 1, col = "firebrick", border = NA)
        mtext("Draw rectangle to select range", 3, cex = 1.5, line = 0.9)
        par(opar)
        
      } else {
        
        opar <- par(mar = c(0, 4, 2, 2))
        plot(NA, xlim = c(1, nrow(raw())), ylim = c(0,1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", xaxs = "i")
        mtext("Draw rectangle to select range", 3, cex = 1.5, line = 0.9)
        par(opar)
        
      }
    }
    
  })
  
  output$lightImage1 <- renderPlot({
    
    if(is.null(input$filename) ) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      
      if(!is.null(selectedRange$min) & input$accept_range %% 2 != 0) {
        
        min <- format(as.POSIXct(selectedRange$min, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        max <- format(as.POSIXct(selectedRange$max, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        
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
    if(input$accept_range %% 2 != 0) {
    
    updateActionButton(session, "accept_range",
                       label = "reset",
                       icon = icon("times"))
    } else {
      if(is.null(selectedRange$min)) {
        updateActionButton(session, "accept_range",
                           label = "no range selected",
                           icon = icon(""))
      } else {
      updateActionButton(session, "accept_range",
                       label = "accept",
                       icon = icon("check"))
      }
    }
  })
  
  
  raw_select <- reactive({
    
    if(!is.null(selectedRange$min) & input$accept_range %% 2 != 0) {
      
      min <- format(as.POSIXct(selectedRange$min, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      max <- format(as.POSIXct(selectedRange$max, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      
      range <- as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
               as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d")
      
      tab <- raw()[range,]
      
      return(tab)
      
    } else return(NULL)
    

  })
  
  
  
  
  
  
  
  #############################
  #### Find twilights #########
  ##########################
  
  nightClick <- reactiveValues(x=NULL, y=NULL)

  observeEvent(nightSelect_slow(), {
    nightClick$x <- c(nightClick$x, nightSelect_slow()$x)
    nightClick$y <- c(nightClick$y, nightSelect_slow()$y)
    
    output$Step3 <- reactive(input$accept_twl %% 2 != 0)
    outputOptions(output, c("Step3"), suspendWhenHidden = FALSE)
    
  })
  
  
  
  output$nightSelect <- renderPlot({
    
    if(is.null(input$filename) | input$accept_range %% 2 != 0) {
      
      opar <- par(mar = c(3, 4, 3, 2))
      lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
      mtext("Select points during the night", 3, line = 1.9, cex = 1.5)
      
      if(any(!is.null(nightClick$x))) {
        # mtext(length(nightClick$x), 3, line = -3, col = "red")
        points(nightClick$x,
               nightClick$y, pch = 16, cex = 2, col = "darkgreen")
      }

      par(opar)
    }
    
  })
  
  nightSelect_slow <- debounce(reactive(input$night_click), 300)
  
  
  output$twilights <- renderPlot({
    
    if(is.null(input$filename) | input$accept_range %% 2 != 0) {
      
      opar <- par(mar = c(3, 4, 3, 2))
      lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
      
      if(any(!is.null(nightClick$x))) {

        day                 <- .POSIXct(nightClick$x[!is.null(nightClick$x)], "GMT")
        hour                <- nightClick$y[!is.null(nightClick$y)]%%24 *60*60
        
        seed                <- .POSIXct(day + hour, "GMT")
        
        twilights           <- findTwilights(raw_select(), threshold = input$threshold, 
                                             include = seed,
                                             extend = 0, dark.min = 0)
        twilights$Deleted   <- logical(nrow(twilights))
        twilights$Marker    <- integer(nrow(twilights))
        twilights$Inserted  <- logical(nrow(twilights))
        
        tsimagePoints(twilights$Twilight, offset = input$offset, pch = 16, cex = 1.5,
                      col = ifelse(twilights$Rise, "dodgerblue", "firebrick"))
      }
      
      par(opar)
    }
    
  })
  
  
  observe({
    if(input$accept_twl %% 2 != 0) {
      
      updateActionButton(session, "accept_twl",
                         label = "reset",
                         icon = icon("times"))
    } else {
      if(all(is.null(nightClick$x))) {
        updateActionButton(session, "accept_twl",
                           label = "no range selected",
                           icon = icon(""))
      } else {
        updateActionButton(session, "accept_twl",
                           label = "accept",
                           icon = icon("check"))
      }
    }
  })
  
  
  
  twl <- reactive({
    
    if(any(!is.null(nightClick$x))) {
      
      day                 <- .POSIXct(nightClick$x[!is.null(nightClick$x)], "GMT")
      hour                <- nightClick$y[!is.null(nightClick$y)]%%24 *60*60
      seed                <- .POSIXct(day + hour, "GMT")
      
      twilights           <- findTwilights(raw_select(), threshold = input$threshold, 
                                           include = seed,
                                           extend = 0, dark.min = 0)
      twilights$Deleted   <- logical(nrow(twilights))
      twilights$Marker    <- integer(nrow(twilights))
      twilights$Inserted  <- logical(nrow(twilights))
      
      return(twilights)
      
    } else return(data.frame(Twilight = as.POSIXct("1970-01-01 05:05:05", tz = "GMT"), Rise = TRUE))
    
  })
  
  
  #############################
  #### Edit twilights #########
  ##########################
  
  output$twilightAll <- renderPlot({
    
      ts <- 3
    
      opar <- par(mar = c(3, 4, 3, 2))
      
      lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
      tsimagePoints(twl()$Twilight, offset = input$offset, pch = 16, cex = 1.15,
                    col = ifelse(twl()$Rise, "dodgerblue", "firebrick"))
      tsimagePoints(twl()$Twilight[ts], offset = input$offset, pch = "x", cex = 1.15,
                    col = "darkgrey")
      par(opar)
    
  })
  
  output$selectedTwilight <- renderPlot({
    
    ts <- 3
    
    ts0 <- raw_select()[raw_select()$Date >= twl()$Twilight[ts]-12*60*60 & raw_select()$Date <= twl()$Twilight[ts]+12*60*60,]
    
    opar <- par(mar = c(3, 4, 3, 2))
    plot(1,1)
    plot(ts0$Date, ts0$Light, type = "l", col = "black")
    par(opar)
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

