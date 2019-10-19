library(shiny)
library(markdown)
library(shinyWidgets)
library(MASS)
library(TwGeos)
library(SGAT)
library(leaflet)

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
                          
                          numericInput("lon.calib0", 
                                       label = "Deployment longitude", 
                                       value = NULL,
                                       step = 0.00001),
                          numericInput("lat.calib0", 
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
                          
                          h4("2. Twilight selection"),
                          conditionalPanel(
                            condition = "input.accept_range & input.accept_edits == false",
                            materialSwitch(
                              inputId = "accept_twl",
                              label   = "accept", 
                              status  = "primary",
                              value   = FALSE,
                              right   = TRUE
                            )
                          ),

                          br(),
                          
                          h4("3. Twilight edit"),
                          conditionalPanel(
                            condition = "input.accept_twl",
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
             ###################
             tabPanel("Calibration",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          numericInput("lon.calib", 
                                       label = "Calibration longitude", 
                                       value = NULL,
                                       step = 0.00001),
                          numericInput("lat.calib", 
                                       label = "Calibration latitude",  
                                       value = NULL,
                                       step = 0.00001),
                          hr(),
                          
                          h4("1.Select calibration period(s)"),
                          
                          conditionalPanel(
                            condition = "input.accept_twl",
                            fluidRow(column(width=8, verbatimTextOutput("calib_range1")),
                                     column(width=2, materialSwitch(
                                       inputId = "lock_calib1",
                                       label   = "", 
                                       status  = "success",
                                       value   = FALSE,
                                       right   = TRUE
                                     ))),
                            fluidRow(column(width=8, verbatimTextOutput("calib_range2")),
                                     column(width=2, materialSwitch(
                                       inputId = "lock_calib2",
                                       label   = "", 
                                       status  = "success",
                                       value   = FALSE,
                                       right   = TRUE
                                     ))),
                          
                            hr(),
                            
                            pickerInput(
                              inputId = "CalibMethod",
                              label = "Calibration Method", 
                              choices = c("log-norm", "gamma"),
                              selected = "gamma"
                            ),
                            
                            fluidRow(column(width=5, verbatimTextOutput("z1")),
                                     column(width=5, h5("zenith"))),
                            fluidRow(column(width=5, verbatimTextOutput("z0")),
                                     column(width=5, h5("TwModel 1"))),
                            fluidRow(column(width=5, verbatimTextOutput("shape")),
                                     column(width=5, h5("TwModel 2"))),
                            fluidRow(column(width=5, verbatimTextOutput("rate")),
                                     column(width=5, h5("TwModel 3"))),
                                  
                          hr()
                          ),
                          
                          actionButton("show3", "Help")
                          
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.accept_twl == true",
                            plotOutput("twilightCalib", height = 200),
                            fluidRow(align="center", uiOutput("calibSlider1")),
                            fluidRow(align="center", uiOutput("calibSlider2")),
                            fluidRow(align="center", plotOutput("calibration", width = 550))
                          )
                        )
                      )
             ),
             ######################
             ##### 4. Locations ###
             ###################
             tabPanel("Location estimates",
                      leafletOutput("map", width = "100%", height = 750)
             ),
             #####
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
    
    output$Deployment <- reactive(!is.null(input$filename) & all(!is.na(c(input$lon.calib0, input$lat.calib0))))
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
      
      if(input$deployment_line %% 2 == 0 & all(!is.na(c(input$lon.calib0, input$lat.calib0)))) {
        tsimageDeploymentLines(raw()$Date, input$lon.calib0, input$lat.calib0, offset = input$offset,
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

  observe({
    if(!is.null(input$select_range_brush1)) {
      selectedRange$min <- input$select_range_brush1$xmin
      selectedRange$max <- input$select_range_brush1$xmax
    }
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
        paste(substr(input$filename$datapat, 1, nchar(input$filename$datapat)-3), "_twl.csv", sep = "")
      },
      content = function(file) {
        write.csv(twl_final(), file, row.names = FALSE)
      }
    )
    
  #############################
  #### 3.1 Calibration ########
  #########################

  observeEvent(input$show3, {
      showModal(modalDialog(
        title = "Calibration",
        HTML("TBA"),
        easyClose = TRUE
      ))
  })
    
  observeEvent(input$lon.calib0, {
    updateNumericInput(session, "lon.calib", value = input$lon.calib0)
  })
  observeEvent(input$lat.calib0, {
    updateNumericInput(session, "lat.calib", value = input$lat.calib0)
  })

  selectedCalibRange1 <- reactiveValues(min = NULL, max = NULL)
  selectedCalibRange2 <- reactiveValues(min = NULL, max = NULL)  

  observe({
    if(input$lock_calib1) {
      selectedCalibRange1$min <- input$calibRange1[1]
      selectedCalibRange1$max <- input$calibRange1[2]
      output$calib_range1     <- renderText({paste0(format(selectedCalibRange1$min, "%Y/%m/%d"), " - ", format(selectedCalibRange1$max, "%Y/%m/%d"))})
    } else {
      selectedCalibRange1$min <- NULL
      selectedCalibRange1$max <- NULL
      output$calib_range1      <- renderText({"not active"})
    }
    
    if(input$lock_calib2) {
      selectedCalibRange2$min <- input$calibRange2[1]
      selectedCalibRange2$max <- input$calibRange2[2]
      output$calib_range2      <- renderText({paste0(format(selectedCalibRange2$min, "%Y/%m/%d"), " - ", format(selectedCalibRange2$max, "%Y/%m/%d"))})
    } else {
      selectedCalibRange2$min <- NULL
      selectedCalibRange2$max <- NULL
      output$calib_range2      <- renderText({"not active"})
    }
    
  })
  
  output$calibSlider1 <- renderUI({
    sliderInput("calibRange1", "",
                min = min(twl_final()$Twilight, na.rm = TRUE),
                max = max(twl_final()$Twilight, na.rm = TRUE),
                value = range(twl_final()$Twilight, na.rm = TRUE),
                ticks = FALSE,
                timeFormat = "%F",
                width = '96%')
  })
  
  output$calibSlider2 <- renderUI({
    sliderInput("calibRange2", "",
                min = min(twl_final()$Twilight, na.rm = TRUE),
                max = max(twl_final()$Twilight, na.rm = TRUE),
                value = range(twl_final()$Twilight, na.rm = TRUE),
                ticks = FALSE,
                timeFormat = "%F",
                width = '96%')
  })
  
  output$twilightCalib <- renderPlot({
    
    opar <- par(mar = c(0, 0, 0, 0))
    
    lightImage(raw_select(), offset = input$offset, zlim = c(0, 10), dt = 120, xaxt = "n", yaxt = "n")
    tsimageDeploymentLines(raw_select()$Date, input$lon.calib, input$lat.calib, offset = input$offset,
                           lwd = 4, col = adjustcolor("orange", alpha.f = 0.6))
    tsimagePoints(twl_final()$Twilight, offset = input$offset, pch = 16, cex = 1.2,
                  col = ifelse(twl_final()$Deleted, "grey40", ifelse(twl_final()$Rise, "dodgerblue", "firebrick")))
    
    if(input$lock_calib1) {
      rect(as.numeric(selectedCalibRange1$min), -25, as.numeric(selectedCalibRange1$max), 25, border = NA, col = adjustcolor("orange", alpha.f = 0.25))
    }
    if(input$lock_calib2) {
      rect(as.numeric(selectedCalibRange2$min), -25, as.numeric(selectedCalibRange2$max), 25, border = NA, col = adjustcolor("orange", alpha.f = 0.25))
    }
    
    par(opar)
    
  })
  
  calib_twl <- reactive({
  
    if(any(c(input$lock_calib1, input$lock_calib2))) {
      
    if(input$lock_calib1 & !input$lock_calib2) {
      tab <- subset(twl_final(), Twilight>=selectedCalibRange1$min & Twilight<=selectedCalibRange1$max)
    }
    if(!input$lock_calib1 & input$lock_calib2) {
      tab <- subset(twl_final(), Twilight>=selectedCalibRange2$min & Twilight<=selectedCalibRange2$max)
    }
    if(input$lock_calib1 & input$lock_calib2) {
      tab <- subset(twl_final(), (Twilight>=selectedCalibRange1$min & Twilight<=selectedCalibRange1$max) |
                                 (Twilight>=selectedCalibRange2$min & Twilight<=selectedCalibRange2$max))
    }
      return(tab)
    } else return(NULL)

  })
  
  calibTab <- reactive({
    
    if(!is.null(calib_twl())) {
    
    tab <- data.frame(Twilight = calib_twl()$Twilight, Rise = calib_twl()$Rise)
    sun <- solar(tab[, 1])
    z <- refracted(zenith(sun, input$lon.calib, input$lat.calib))
    
    inc = 0
    
    repeat {
      twl_t <- twilight(tab[, 1], input$lon.calib, input$lat.calib, rise = tab[, 2], zenith = max(z) + inc)
      twl_dev <- ifelse(tab$Rise, as.numeric(difftime(tab[, 1], twl_t, units = "mins")), as.numeric(difftime(twl_t, tab[, 1], units = "mins")))
      if (all(twl_dev >= 0) | inc>2) break  else inc <- inc + 0.01
    }
    
        if(all(twl_dev>=0 & twl_dev<5*60)) {
        z0 <- max(z) + inc
        seq <- seq(0, max(twl_dev), length = 100)
        if (input$CalibMethod == "log-norm") {
          fitml_ng <- suppressWarnings(fitdistr(twl_dev, "log-normal"))
          lns <- dlnorm(seq, fitml_ng$estimate[1], fitml_ng$estimate[2])
        }
        if(input$CalibMethod== "gamma") {
          fitml_ng <- suppressWarnings(fitdistr(twl_dev, "gamma"))
          lns <- dgamma(seq, fitml_ng$estimate[1], fitml_ng$estimate[2])
        }
        
        z1 <- median(z)
        
        return(list(twlDev = twl_dev, z = z, method = input$CalibMethod, z1 = z1, z0 = z0, shape = fitml_ng$estimate[1], rate = fitml_ng$estimate[2]))
        } else return(NULL)
    } else return(NULL)
  })
  
  output$calibration <- renderPlot({
    
    if(!is.null(calibTab()) & !is.null(input$lon.calib) & !is.null(input$lat.calib)) {
      
      
      opar <- par(mar = c(4, 4, 1, 1))
      hist(calibTab()$twlDev, freq = F, breaks = 26, main = "", xlab = "twilight error (min)", col = "grey85")
      
      seq <- seq(0, max(calibTab()$twlDev), length = 100)
      if(input$CalibMethod== "gamma") {
        lines(seq, dgamma(seq, calibTab()$shape, calibTab()$rate), col = "firebrick", lwd = 3, lty = 2)
      } else {
        lines(seq, dlnorm(seq, calibTab()$shape, calibTab()$rate), col = "firebrick", lwd = 3, lty = 2)
      }
      
      diffz <- as.data.frame(cbind(min = apply(cbind(calib_twl()$Twilight,
                            twilight(calib_twl()$Twilight, input$lon.calib, input$lat.calib, rise = calib_twl()$Rise, zenith = calibTab()$z0)), 1,
                            function(x) abs(x[1] - x[2]))/60, z = calibTab()$z))
      mod <- lm(min ~ z, data = diffz)

      points(predict(mod, newdata = data.frame(z = calibTab()$z0)), 0,
                     pch = 21, cex = 5, bg = "white", lwd = 2, xpd = TRUE)
      text(predict(mod, newdata = data.frame(z = calibTab()$z0)), 0, "z0")

      points(predict(mod, newdata = data.frame(z = calibTab()$z1)), 0,
             pch = 21, cex = 5, bg = "white", lwd = 2, xpd = TRUE)
      text(predict(mod, newdata = data.frame(z = calibTab()$z1)), 0, "z1")

      par(opar)
      
    } else plot(1,1, xaxt = "n", yaxt = "n", type = "n", bty = "n", xlab = "", ylab = "")
    
  })
  
  output$z0    <- renderText({ifelse(!is.null(calibTab()), calibTab()$z0, "not defined")})
  output$z1    <- renderText({ifelse(!is.null(calibTab()), calibTab()$z1, "not defined")})
  output$shape <- renderText({ifelse(!is.null(calibTab()), calibTab()$shape, "not defined")})
  output$rate  <- renderText({ifelse(!is.null(calibTab()), calibTab()$rate, "not defined")})
    

  #############################
  #### 4. Locations ###########
  #######################
  
  coords <- reactive({
    if(!is.null(calibTab())) {
      tab <- with(twl_final()[!twl_final()$Deleted, ], thresholdLocation(Twilight, Rise, zenith = calibTab()$z1)$x)
      return(tab)
    } else {
      return(NULL)
    }
  })


  output$map <- renderLeaflet({

    if(is.null(coords())) {
      leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = FALSE)
                       
      ) 
      } else {
         leaflet() %>%
           addProviderTiles(providers$Esri.WorldGrayCanvas,
                           options = providerTileOptions(noWrap = FALSE)
          ) %>%
          addPolylines(data = coords()[!is.nan(coords()[,2]),], color = adjustcolor("orange", alpha.f = 0.75), weight = 4, ) %>%
          addCircleMarkers(data = coords()[!is.nan(coords()[,2]),], color = "orange", weight = 1)
      }
  })
  
  }



# Run the application 
shinyApp(ui = ui, server = server)