library(shiny)
library(markdown)
library(shinyWidgets)
library(MASS)
library(TwGeos)
library(SGAT)
library(leaflet)

setClass(
  "VWSGapp",
  slots = c(ID       = "character",
            Mdata    = "list", 
            Raw      = "data.frame",
            Twl      = "data.frame")
)

LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}

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

             ########################
             ##### 1. Load Project ##
             ########################
             tabPanel("StartPage",
                      
                      fluidRow(
                        column(3,
                               h3("New tag"),
                               textInput("loggerID",
                                         label = "Logger ID:",
                                         value = "")  
                               ),
                        column(5,
                              h3("Load project"),
                              fileInput(inputId = "projectFile",
                                          label = "Existing project",
                                          multiple = FALSE,
                                          accept = ".RData")
                        ),
                        column(6,
                               conditionalPanel(
                                 condition = "output.fromFile == true",
                                 actionButton("toAnalysis", "Create Project")
                               )
                        )
                      )
                      
             ), ## tabPanel
             
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
                          
                          textOutput("rawLight"),
                          conditionalPanel(
                                condition = "output.fromFile == false",
                                fileInput("filename",
                                          label = "Browse for your file",
                                          accept = c(".lux",
                                                     ".lig")
                                )
                          ),
                          numericInput("offset", 
                                       label = "Offset", 
                                       value = 1,  #default value
                                       step  = 1), #"steps" with arrow buttons.
                          
                          br(),
                          
                          conditionalPanel(
                            condition = "output.deployment == true",
                            actionButton("deployment_line", "Add deployment line")
                          ),
                          
                          hr(),
                          
                          numericInput("threshold", 
                                       label = h4("Threshold"), 
                                       value = 1,
                                       step  = 0.25),
                          
                          hr(),
                          
                          downloadButton("savePrj", "Save project"),
                          
                          hr(),
                          
                          actionButton("show1", "Help"),
                          textOutput("test")
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
             ) ## tabPanel (Twilight)
             
      ) ## navbarPage
) ## ui


server <- function(input, output, session) {
  
  ind    <- reactiveValues(acceptS4 = FALSE, rawTab = FALSE, twlTab = FALSE)
  
  dat    <- reactiveValues(ID = NULL,
                           Mdata = list(lon.calib = NULL,
                                        lat.calob = NULL,
                                        offset    = NULL,
                                        threshold = NULL),
                           Raw = NULL,
                           Twl = NULL)
  
  observe({
    if(is.null(dat$Raw)) {
      hideTab(inputId = "navbar", target = "Twilight")
    } else {
      showTab(inputId = "navbar", target = "Twilight")
    }
  })
  
  
  #############################
  #### 0. Create Project ######
  ##########################
  
  observe({
    if(is.null(dat$ID) & input$toAnalysis %% 2 == 0) {
      hideTab(inputId = "navbar", target = "Light data")
    } else {
      showTab(inputId = "navbar", target = "Light data")
    }
    
    output$fromFile <- reactive(input$loggerID != "" & !ind$acceptS4)
    outputOptions(output, c("fromFile"), suspendWhenHidden = FALSE)
  })
  
  observeEvent(input$projectFile, {
    env <- reactiveFileReader(1000, session, input$projectFile$datapath, LoadToEnvironment)
    if(class(env()[[names(env())[1]]]) != "VWSGapp") {
      sendSweetAlert(
              session = session,
              title = "Error",
              text = "Loaded file is not a VWSGapp project.",
              type = "error"
            )
    } else {
      
      dat$ID <- env()[[names(env())[1]]]@ID
      
      if(!is.null(env()[[names(env())[1]]]@Mdata$lon.calib)) {
        dat$Mdata$lon.calib <- env()[[names(env())[1]]]@Mdata$lon.calib
        updateNumericInput(session, "lon.calib0",  value = dat$Mdata$lon.calib)
      }
      
      if(!is.null(env()[[names(env())[1]]]@Mdata$lat.calib)) {
        dat$Mdata$lat.calib <- env()[[names(env())[1]]]@Mdata$lat.calib
        updateNumericInput(session, "lat.calib0",  value = dat$Mdata$lat.calib)
      }
      
      if(!is.null(env()[[names(env())[1]]]@Mdata$offset))    {
        dat$Mdata$offset <- env()[[names(env())[1]]]@Mdata$offset
        updateNumericInput(session, "Offset",  value = dat$Mdata$offset)
      }
      
      if(!is.null(env()[[names(env())[1]]]@Mdata$threshold))    {
        dat$Mdata$threshold <- env()[[names(env())[1]]]@Mdata$threshold
        updateNumericInput(session, "threshold",  value = dat$Mdata$threshold)
      }
      
      if(nrow(env()[[names(env())[1]]]@Raw)>0) dat$Raw <- env()[[names(env())[1]]]@Raw
      
      updateTextInput(session, "loggerID", value = env()[[names(env())[1]]]@ID)
      ind$acceptS4 <- TRUE
      
      output$fromFile  <- reactive(TRUE)
      outputOptions(output, "fromFile", suspendWhenHidden = FALSE)
    }
  })
  
  
  observeEvent(input$Offset, {
    dat$Mdata$offset <- input$Offset
    })
  observeEvent(input$threshold, {
    dat$Mdata$threshold <- input$threshold
  })
  observeEvent(input$lon.calib0, {
    dat$Mdata$lon.calob <- input$lon.calib0
  })
  observeEvent(input$lat.calib0, {
    dat$Mdata$offset <- input$lat.calib0
  })

  
  
  # output$savePrj <- downloadHandler(
  #   filename = function() {
  #     paste(input$loggerID, "_", Sys.Date(), ".RData", sep="")
  #   },
  #   content = function(file) {
  #     save(dat$resOut, file = file)
  #   }
  # )
  
  
  #############################
  #### 1. Read Data ###########
  ##########################

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

  observeEvent(input$filename, {
    
    inFile <- input$filename
    filetype <- substr(input$filename$datapat, nchar(input$filename$datapat)-3, nchar(input$filename$datapat))

      if (filetype == ".lig") {
        dat$Raw <- readLig(inFile$datapat)

        output$accept_range <- reactive(ncol(raw())>0)
        outputOptions(output, c("accept_range"), suspendWhenHidden = FALSE)
      
        
      }
      if (filetype == ".lux") {
        dat$Raw <-        readMTlux(inFile$datapath)
        dat$Raw$Light <- log(dat$Raw$Light)

        output$accept_range <- reactive(ncol(raw())>0)
        outputOptions(output, c("accept_range"), suspendWhenHidden = FALSE)
      }
    
  })
  
  observe({
    if(!is.null(dat$Raw)) {
      if(is.null(input$select_range_brush$xmin)) {
        min <- format(dat$Raw$Date[1], "%Y-%m-%d")
        max <- format(dat$Raw$Date[nrow(dat$Raw)], "%Y-%m-%d")
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
    
    if(is.null(dat$Raw)) {
      selectedRange$min <- input$select_range_brush1$xmin
      selectedRange$max <- input$select_range_brush1$xmax
    }
    
    output$deployment <- reactive(!is.null(input$filename) & all(!is.na(c(input$lon.calib0, input$lat.calib0))))
    outputOptions(output, c("deployment"), suspendWhenHidden = FALSE)
    
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
    
    if(is.null(dat$Raw)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    } else { 
      
      lightImage(dat$Raw, offset = input$offset, zlim = c(0, 10), dt = 120)
      mtext("Draw rectangle to inspect period of light recordings", cex = 1.5, line = 3)
      
      if(input$deployment_line %% 2 == 0 & all(!is.na(c(input$lon.calib0, input$lat.calib0)))) {
        tsimageDeploymentLines(dat$Raw$Date, input$lon.calib0, input$lat.calib0, offset = input$offset,
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
    
    if(is.null(dat$Raw)) {
      plot(1,1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
      min <- "2000-01-01"
      max <- "2025-12-31"
    } else {

        if(is.null(input$select_range_brush0$xmin)) {
          min <- format(dat$Raw$Date[1], "%Y-%m-%d")
          max <- format(dat$Raw$Date[nrow(dat$Raw)], "%Y-%m-%d")
        } else {
          min <- format(as.POSIXct(input$select_range_brush0$xmin, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
          max <- format(as.POSIXct(input$select_range_brush0$xmax, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")
        }
      
      range <- as.POSIXct(dat$Raw$Date) > as.POSIXct(min, format = "%Y-%m-%d") &
        as.POSIXct(dat$Raw$Date) < as.POSIXct(max, format = "%Y-%m-%d")
      
      with(dat$Raw[range,], plot(Date, Light, type = "o", pch = 16, cex = 0.25, xlab = "", ylab = "log light", las = 1,
                               ylim = c(0, max(dat$Raw$Light, na.rm = T))))
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
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)