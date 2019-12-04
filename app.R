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
                                 condition = "input.loggerID != ''",
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
                      
             ) ## tabPanel (Data)
            
             
      ) ## navbarPage
) ## ui


server <- function(input, output, session) {
  
  output <- reactiveValues(acceptS4 = FALSE)
  dat    <- reactiveValues(resOut = NULL)
  
  observe({
    if(is.null(dat$resOut)) {
      hideTab(inputId = "navbar", target = "Light data")
    } else {
      showTab(inputId = "navbar", target = "Light data")
    }
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
      updateTextInput(session, "loggerID", value = env()[[names(env())[1]]]@ID)
      output$acceptS4 <- TRUE
    }
  })

  
  resOut <- reactive({
    if(output$acceptS4) {
      outS01 <- reactiveFileReader(1000, session, input$projectFile$datapath, LoadToEnvironment)
      outS1  <- outS01()[[names(outS01())[1]]]
      return(outS1)
    } else {
      if(input$toAnalysis %% 2 == 1) {
        outS1 <- new("VWSGapp", ID = input$loggerID,
                                Mdata = list(lon.calib = NULL,
                                             lat.calib = NULL,
                                             offset    = NULL,
                                             threshold = NULL))
        return(outS1)
      } else return(NULL)
    }
  })
  
  observe({
    dat$resOut <- resOut()
  })
  
  
  ###### Raw data ####
  
  observe({
    if(!is.null(dat$resOut)) {
    if(!is.null(dat$resOut@Mdata$lon.calib)) {
      updateNumericInput(session, "lon.calib0",  value = dat$resOut@Mdata$lon.calib)
    }
    if(!is.null(dat$resOut@Mdata$lat.calib)) {
      updateNumericInput(session, "lat.calib0", value = dat$resOut@Mdata$lat.calib)
    }
    if(!is.null(dat$resOut@Mdata$offset)) {
      updateNumericInput(session, "Offset", value = dat$resOut@Mdata$offset)
    } 
    if(!is.null(dat$resOut@Mdata$threshold)) {
      updateNumericInput(session, "threshold", value = dat$resOut@Mdata$threshold)
    } 
    }
  })
  
  observeEvent(input$offset, {
    if(!is.null(dat$resOut)) {
    dat$resOut@Mdata$offset <- input$offset
    }})
  
  observeEvent(input$threshold, {
    if(!is.null(dat$resOut)) {
      dat$resOut@Mdata$threshold <- input$threshold
    }})
    

  
  
}



# Run the application 
shinyApp(ui = ui, server = server)