library(shiny)
library(markdown)
library(shinyWidgets)
library(TwGeos)
library(SGAT)

ui <- fluidPage(
  tags$head(
    
  tags$style(HTML("
                    .col-sm-4 { width: 20%;}
                    .col-sm-8 { width: 80%;}
                    
                    hr {
                    display: block; height: 4px;
                    border: 1; border-top: 2px solid #ccc;
                    margin: 1em 0; padding: 0; 
                    }
                    "))
    
  ),
  
  tags$script('
              $(document).on("keydown", function (e) {
              Shiny.onInputChange("down", [e.which]);
              Shiny.onInputChange("trigger", Math.random());
              });'
  ),

  navbarPage("VWSGapp", id = "navbar",
             
             ######################
             ##### 1. Light data ##
             ###################
             tabPanel("Light data",

                      titlePanel("Select raw light recordings"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          numericInput("lon.calib", 
                                       label = "Deployment longitude", 
                                       value = NULL,
                                       step = 0.00001),
                          numericInput("lat.calib", 
                                       label = "Deployment latitude",  
                                       value = NULL,
                                       step = 0.00001),
                          hr(),

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
                          
                          conditionalPanel(
                            condition = "output.Deployment == true",
                            actionButton("deployment_line", "Add deployment line")
                          ),
                          
                          hr(),
                          
                          numericInput("threshold", 
                                       label = h4("Threshold"), 
                                       value = 1,
                                       step  = 0.25),
                          
                          hr(),
                          
                          actionButton("show1", "Help")
                        ),
                        
                        mainPanel(
                          uiOutput("selectLightSeries"),
                          plotOutput("lightSeries")
                        )
                        
                      ) ## end Sidebar Panel
                  
             ), ## tabPanel (Data)
             
             
             ######################
             ##### 2. Twilight ####
             ###################
             tabPanel("Twilight",
                      
                      titlePanel("Twilight Annotation"),
                      
                      sidebarLayout(
                        sidebarPanel(

                          h4("1.Select range"),
                          conditionalPanel(
                            condition = "input.accept_twl==false",
                            materialSwitch(
                              inputId = "accept_range",
                              label   = "accept", 
                              status  = "primary",
                              value   = FALSE,
                              right   = TRUE
                              )
                          ),
                          
                          br(),
                          
                          conditionalPanel(
                            condition = "input.accept_range & input.accept_edits == false",
                            h4("2. Twilight selection"),
                            materialSwitch(
                              inputId = "accept_twl",
                              label   = "accept", 
                              status  = "primary",
                              value   = FALSE,
                              right   = TRUE
                            )
                          ),

                          br(),
                          
                          conditionalPanel(
                            condition = "input.accept_twl",
                            h4("3. Twilight edit"),
                            materialSwitch(
                              inputId = "accept_edits",
                              label   = "accept", 
                              status  = "primary",
                              value   = FALSE,
                              right   = TRUE
                            )
                          ),
                          
                          br(),

                          conditionalPanel(
                            condition = "input.accept_edits",
                            h4("Download Twilights"),
                            downloadButton("downloadTwl", "Download")
                          ),
                          
                          hr(),
                          
                          actionButton("show2", "Help")
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.accept_range == false",
                                uiOutput("selectRangePlot")
                           ),
                          conditionalPanel(
                            condition = "input.accept_range == true & input.accept_twl == false",
                            plotOutput("nightSelect", height=400,
                                       click = clickOpts(id = "night_click")),
                            plotOutput("twilights", height=400)
                          )
                          ,
                          conditionalPanel(
                            condition = "input.accept_twl == true",
                            uiOutput("selectTwilightPlot"),
                            plotOutput("selectedTwilight", height=400,
                                       click = clickOpts(id = "new_edit", clip = FALSE))
                          )
                        )
                      )
            ), ## tabPanel (Twilight)
            
             ######################
             ##### 3. Calibration #
             ######################
            
             tabPanel("Calibration"),
             tabPanel("Location estimates"),
             tabPanel("Breeding site"),
             tabPanel("Breeding performance")
            
      ) ## navbarPage
) ## ui


server <- function(input, output, session) {
  
  #############################
  #### 1. Read Data ###########
  ##########################
  

  
  raw <- reactive({
    
    req(input$filename)
    inFile <- input$filename
    
    filetype <- substr(input$filename$datapat, nchar(input$filename$datapat)-3, nchar(input$filename$datapat))    
    
    if(is.null(inFile)) {
      return(NULL)
    } else {
      if (filetype == ".lig") {
        tab <- readLig(inFile$datapat)
        
        output$accept_range <- reactive(ncol(raw())>0)
        outputOptions(output, c("accept_range"), suspendWhenHidden = FALSE)
        
        return(tab)
      } else {
        if (filetype == ".lux") {
          
          tab <- readMTlux(inFile$datapath)
          tab$Light<-log(tab$Light)
          
          output$accept_range <- reactive(ncol(raw())>0)
          outputOptions(output, c("accept_range"), suspendWhenHidden = FALSE)
          
          return(tab)
          
        }
      
      }
    }
   })
  
  observeEvent(input$show1, {
    showModal(modalDialog(
      title = "Inspect raw data recordings",
      "In this first step you can upload your raw data files (so far, the old .lig files from the BAStags and the .lux files from Migrate Technology Ltd. are supported 
                             file types). Two plots will appear that help to inspect the data. In the top panel, you can see the light recordings per 24 hours (rows) over all days (days).
                             It is recommended to change the offset parameter (up/down) to make sure that the night (dark period) is centered. You can also add the deployment longitude/latitude
                             to add the sunrise and sunset times for this location. This can help to already see when the bird departed/arrived at this site. However, more importantly, it helps
                             to see if there is a time zone issue or a significant clock drift in the dataset: The orange deployment line is not similar to the sunrise/sunset times during the periods
                             when the bird must have been at the site (time zone issue) or the discrepancy is different durint the start and end of the time series (clock drift).",
      easyClose = TRUE
    ))
  })
  
  observe({
    if(!is.null(raw())) {
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
  #### 2.1 Select Data Plots ##
  ##########################
  
  
  observe({
    if(is.null(raw())) {
      selectedRange$min <- input$select_range_brush1$xmin
      selectedRange$max <- input$select_range_brush1$xmax
    }
    
    output$Deployment <- reactive(!is.null(input$filename) & all(!is.na(c(input$lon.calib, input$lat.calib))))
    outputOptions(output, c("Deployment"), suspendWhenHidden = FALSE)
    
    if(input$deployment_line %% 2 == 0) {
      
      updateActionButton(session, "deployment_line",
                         label = "Delete deployment line")
    } else {
      updateActionButton(session, "deployment_line",
                         label = "Add deployment line")
    }
    
    
    if(input$accept_range>0 & (input$accept_range %% 2 == 0)) {
      selectedRange$min <- NULL
      selectedRange$max <- NULL
    }
  })
  
  output$lightImage0 <- renderPlot({
    
    if(is.null(raw())) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else { 
      
      lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
      mtext("Draw rectangle to inspect period of light recordings", cex = 1.5, line = 3)
      
      if(input$deployment_line %% 2 == 0 & all(!is.na(c(input$lon.calib, input$lat.calib)))) {
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
    
    if(is.null(raw())) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
      
      if(!is.null(raw())) {
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
                               ylim = c(0, max(raw()$Light, na.rm = T))))
      abline(h = input$threshold, lty = 2, col = "orange")
    }
    
  })

  
  
  
  #############################
  #### 2.2 Select Range Plots #
  ##########################
  
  observeEvent(input$show2, {
    showModal(modalDialog(
      title = "Twilight Annotation",
      HTML("This step allows the user to interactively search for and edit twilight times corresponding to a given light threshold. The process consists of three stages: <br>
      <br>
       1) Subset – selection of a subset of the data for processing<br>
       2) Search – semi-automated search for the twilights<br>
       3) Edit – optioanlly, individual twilights are manually adjusted based on the light profiles.<br>
      <br>
      In the first stage the user can restrict the data to a subset for processing. The plot shows a light image ans the user can draw a rectangle to select the range. By choosing 'accept' the range will be applied.
      In the second step, the user needs to choose a datetime during the night by clicking into the night blob of the upper panel. If OK the sunrise (red) and sunset (blue) times (e.g. the twilight times) will appear in the lower panel. Several clicks might be nessesary to get all twiligths.
      After accepting this step, the user will get a light image with all identified, and a lower panel with the ligth recordings of the selected twilight (star in upper panel). The user may adjust individual twilights based on the observed light profile. 
      The light profile for that twilight in the second window, together with the profiles for the preceeding and following days. A left click in the second window proposes a new location for the current twilight, but no change is made until the edit is accepted with the 'a' key. 
      Twiligth can also be deleted by pressing the 'd' key (undo by pressing'd' again."),
      easyClose = TRUE
    ))
  })
  
  selectedRange <- reactiveValues(min = NULL, max = NULL)
  output$test   <- renderText({input$select_range_brush1$xmin})
  
  observe({
    
    if(!is.null(input$select_range_brush1)) {
      selectedRange$min <- input$select_range_brush1$xmin
      selectedRange$max <- input$select_range_brush1$xmax
    }
    
    # if(!is.null(input$select_range_brush1) & !input$accept_range) {
    #   selectedRange$min <- NULL
    #   selectedRange$max <- NULL
    # }
    
  })

  
  output$lightImage1 <- renderPlot({
    
    if(is.null(input$filename) ) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else {
        opar <- par(mar = c(3, 4, 4, 2))
        lightImage(raw(), offset = input$offset, zlim = c(0, 10), dt = 120)
        mtext("Draw rectangle to select range", cex = 1.5, line = 3)
        par(opar)
    }
  })
  
  
  output$selectRangePlot <- renderUI({
    plotOutput("lightImage1", height=500,
               brush = brushOpts(id = "select_range_brush1", resetOnNew = TRUE)
    )
  })
  
  
  observe({
    if(input$accept_range) {
        updateActionButton(session, "accept_range",
                       label = "reset")
    } else {
      if(is.null(input$select_range_brush1$xmin)) {
        updateActionButton(session, "accept_range",
                           label = "no range selected")
      } else {
        updateActionButton(session, "accept_range",
                          label = "accept")
      }
    }
  })

  raw_select <- reactive({

    if(!is.null(selectedRange$min) & input$accept_range) {

      min <- format(as.POSIXct(selectedRange$min, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
      max <- format(as.POSIXct(selectedRange$max, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")

      range <- as.POSIXct(raw()$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
               as.POSIXct(raw()$Date) < as.POSIXct(max, format = "%Y-%m-%d")

      tab <- raw()[range,]

      return(tab)

    } else return(NULL)


  })

  #############################
  #### 2.3 Find twilights #####
  ##########################
  
    nightClick <- reactiveValues(x=NULL, y=NULL)
    resetTwl   <- reactiveValues(accept = 0)
    
    observeEvent(nightSelect_slow(), {
      nightClick$x <- c(nightClick$x, nightSelect_slow()$x)
      nightClick$y <- c(nightClick$y, nightSelect_slow()$y)
      
    })
    
    observeEvent(input$accept_twl, {
      if(!input$accept_twl & resetTwl$accept>0) {
        nightClick$x <- NULL
        nightClick$y <- NULL
      }
    })
    
    output$nightSelect <- renderPlot({
  
        opar <- par(mar = c(3, 4, 3, 2))
        lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
        mtext("Select points during the night", 3, line = 1.9, cex = 1.5)
  
        if(any(!is.null(nightClick$x))) {
          points(nightClick$x,
                 nightClick$y, pch = 16, cex = 2, col = "darkgreen")
        }
        par(opar)
  
    })
  
    nightSelect_slow <- debounce(reactive(input$night_click), 300)
    
    
    output$twilights <- renderPlot({
  
      if(is.null(input$filename) | input$accept_range) {
  
        opar <- par(mar = c(3, 4, 3, 2))
        lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
  
        if(any(!is.null(nightClick$x))) {
  
          seed                <- as.POSIXct(as.numeric(nightClick$x[!is.null(nightClick$x)]),
                                            origin = "1970-01-01", t = "GMT")
  
          twilights           <- findTwilights(raw_select(), threshold = input$threshold,
                                               include = seed,
                                               extend = 0, dark.min = 0)
          twilights$Deleted   <- rep(FALSE, nrow(twilights))
          twilights$Marker    <- integer(nrow(twilights))
          twilights$Inserted  <- rep(FALSE, nrow(twilights))
  
          tsimagePoints(twilights$Twilight, offset = input$offset, pch = 16, cex = 1.5,
                        col = ifelse(twilights$Rise, "dodgerblue", "firebrick"))
        }
  
        par(opar)
      }
  
    })
  
    observe({
      if(input$accept_twl) {
  
          updateActionButton(session, "accept_twl",
                           label = "reset")
  
      } else {
        if(all(is.null(nightClick$x))) {
          updateActionButton(session, "accept_twl",
                             label = "no twilights selected")
      } else {
          updateActionButton(session, "accept_twl",
                             label = "accept")
      }
      }
    })
    
    
    observeEvent(input$accept_twl, {
      resetTwl$accept <- resetTwl$accept + 1
      output$accept <- renderText({resetTwl$accept})
    })
    
    observeEvent(input$accept_range, {
      resetTwl$accept <- 0
    })
    
    twl <- reactive({
  
      if(any(!is.null(nightClick$x))) {
        seed                <- as.POSIXct(as.numeric(nightClick$x[!is.null(nightClick$x)]),
                                          origin = "1970-01-01", t = "GMT")
        twilights           <- findTwilights(raw_select(), threshold = input$threshold,
                                             include = seed,
                                             extend = 0, dark.min = 0)
        twilights$Deleted   <- logical(nrow(twilights))
        twilights$Twilight3 <- twilights$Twilight
  
        return(twilights)
  
      } else return(data.frame(Twilight = as.POSIXct("1970-01-01 05:05:05", tz = "GMT"), Rise = TRUE))
  
    })
  
  #############################
  #### 2.4 Edit twilights #####
  ##########################
  
    edit    <- reactiveValues(new = NULL, twilight = NULL, rise = NULL, deleted = NULL, key = NULL, event = 1)
  
    observe({
      if(!is.null(twl())) {
        edit$twilight <- as.POSIXct(as.numeric(isolate(twl()$Twilight)), origin = "1970-01-01", tz = "GMT")
        edit$rise     <- isolate(twl()$Rise)
        edit$deleted  <- isolate(twl()$Deleted)
      }
    })
  
    observe({
      if(!is.null(input$select_ts)) {
      edit$event <- which.min(abs(edit$twilight - as.POSIXct(as.numeric(input$select_ts$x), origin = "1970-01-01", tz = "GMT")))
      }
    })
    
    observeEvent(input$trigger,{
      edit$key <- input$down
      if(!is.null(edit$new) && edit$key==65) {
        edit$twilight[edit$event] <- edit$new
        edit$new <- NULL
      }
      if(edit$key==68) {
        edit$deleted[edit$event] <- !edit$deleted[edit$event]
        edit$new <- NULL
      }
      if(edit$key==37 && edit$event>1) {
        edit$event <- edit$event-1
      }
      if(edit$key==39 && edit$event<length(edit$twilight)) {
        edit$event <- edit$event+1
      }
    })

    observeEvent(twlEdit_slow(), {
      edit$new <- as.POSIXct(as.numeric(twlEdit_slow()$x), origin = "1970-01-01", tz = "GMT")
    })
  
    output$twilightAll <- renderPlot({
  
        opar <- par(mar = c(3, 4, 3, 2))
  
        lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120)
        tsimagePoints(edit$twilight, offset = input$offset, pch = 16, cex = 1.2,
                      col = ifelse(edit$deleted, "grey40", ifelse(edit$rise, "dodgerblue", "firebrick")))
        tsimagePoints(edit$twilight[edit$event], offset = input$offset, pch = "x", cex = 1.2,
                      col = "darkgrey")
        par(opar)
  
    })

    output$selectTwilightPlot <- renderUI({
      plotOutput("twilightAll", height=400,
                 click = clickOpts(id = "select_ts", clip = FALSE)
      )
    })

    output$selectedTwilight <- renderPlot({
  
      tsT <- raw_select()
      ts0 <- tsT[tsT$Date >=  twl()$Twilight[edit$event]-12*60*60 &
                 tsT$Date <=  twl()$Twilight[edit$event]+12*60*60,]
      ts1 <- tsT[tsT$Date >= (twl()$Twilight3[edit$event]-12*60*60)+24*60*60 &
                 tsT$Date <= (twl()$Twilight3[edit$event]+12*60*60)+24*60*60,]
      ts2 <- tsT[tsT$Date >= (twl()$Twilight3[edit$event]-12*60*60)-24*60*60 &
                 tsT$Date <= (twl()$Twilight3[edit$event]+12*60*60)-24*60*60,]
  
      twlT <- twl()
      twl0 <- twlT[twlT$Twilight >= twlT$Twilight[edit$event]-12*60*60 &
                   twlT$Twilight <= twlT$Twilight[edit$event]+12*60*60 &
                   twlT$Rise     == twlT$Rise[edit$event],]
  
      opar <- par(mar = c(3, 4, 3, 2), las = 1)
      plot(ts0$Date, ts0$Light, type = "l", col = "black", lwd = 1.6, xlab = "Date", ylab = "Light")
      par(new = T)
      if(nrow(ts1)>1) plot(ts1$Date, ts1$Light, type = "l", col = adjustcolor("darkgreen", alpha.f = 0.7), lwd = 1.3, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      par(new = T)
      if(nrow(ts2)>1) plot(ts2$Date, ts2$Light, type = "l", col = adjustcolor("darkorchid2", alpha.f = 0.7), lwd = 1.3, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  
      par(new = TRUE)
      plot(ts0$Date, ts0$Light, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
      abline(h = input$threshold, lty = 2, col = "grey20")
      abline(v = edit$twilight[edit$event], col = "grey80", lwd = 1.5, lty = ifelse(edit$deleted[edit$event], 3, 1))
  
      if(!is.null(edit$new)) {
        points(edit$new, input$threshold, pch = 16, cex = 1.8, col = ifelse(edit$deleted[edit$event], "grey40", "red"))
      } else points(edit$twilight[edit$event], input$threshold, pch = 16, cex = 1.8, col = ifelse(edit$deleted[edit$event], "grey40", "red"))
  
      par(opar)
  
    })

    twlEdit_slow <- debounce(reactive(input$new_edit), 300)


    twl_final <- reactive({
      
      if(input$accept_edits) {
        tab <- data.frame(Twilight  = edit$twilight,
                          Rise      = edit$rise,
                          Deleted   = edit$deleted)
        tab$Marker    <- NA
        tab$Inserted  <- FALSE
        tab$Twilight3 <- isolate(twl()$Twilight3)
        tab$Marker3   <- NA

        return(tab)
      } else {
        return(NULL)
      }
    })
    
    output$downloadTwl <- downloadHandler(
      filename = function() {
        paste("ID.csv", sep = "")
      },
      content = function(file) {
        write.csv(twl_final(), file, row.names = FALSE)
      }
    )
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

