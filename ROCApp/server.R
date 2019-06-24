
options(shiny.maxRequestSize = 9*10240^2)

function(input, output, session) {
  
  # App consists of the following pages:
  # 1) Overview
  # 2) Load data
  # 3) Specify inputs
  # 4) View data and map
  # 5) View results
  # 6) Download
  
  #----------------------#
  #-- 1. Overview -------#
  #----------------------#
  
  output$overviewPage <- renderUI({
    
    fluidPage(
      
      h1("Risk of Contact Tool"),
      
      p("This web-app implements the US Forest Service's Risk of Contact tool, 
        which quantifies the chance that a bighorn sheep crosses a domestic 
        sheep grazing allotment. This web-app implementation aims to:"),
      
      tags$ol(
        
        tags$li("Give users an easy interface to integrate data 
                required to run the Risk of Contact tool; and "),
        
        tags$li("Produce synthetic, reproducable, fully documented 
                        results of the Risk of Contact analysis.")),
      
      img(src = "MtnViewEweGrp.png", height = 400, width = 900),
      
      p("The app consists of the following elements"),
      
      # link to markdown page with description of app infrastructure
      
      includeMarkdown("description.md")
      
    )
    
  })
  
  
  
  
  #------------------#
  #-- 2. Load data --#
  #------------------#
  
  output$loadDataPage <- renderUI({
    
    # specify page title
    
    titlePanel("Load data")
    
    # set up sidebar navigation panel
    
    navlistPanel(  
      
 
      # conditional panel: on the Overview tab, include data upload material
      tabPanel("Allotment boundaries",
            
               # function to navigate to, and read in geodatabase of allotment boun(daries
               fileInput('ALLOTS', 'Read in allotment boundaries as ESRI shapefile (extension .shp)',
                         accept = c(
                           '.gdb',
                           'shp.'
                         )
               ), 
               
               directoryInput('ALLOTSdirectory', label = 'If allotment boundaries are stored as .gdb file, navigate to appropriate directory
                   (For now, located at data/file-geodatabase/Payette.gdb).')
               

      ), # close Allotment boundaries tab
      
      # conditional panel: on the Overview tab, include data upload material
      tabPanel("Core herd home range boundaries",
               
               # function to navigate to, and read in geodatabase of allotment boundaries
               fileInput('CHHR', 
                         
               'Read in bighorn core herd home range boundaries as geodatabase (".gdb") file.',
               
               accept = c('.gdb')
               ),
               
               # Conditional panel that opens AFTER csv is loaded for specifying field names
               conditionalPanel(
                 condition = "output.bighornDataUploaded == true",
                 # drop-downs for user to specify long, lat, id, and pop fields.
                 'Specify field names for uploaded dataset',
                 selectInput("bighornRawNamesPop",
                             label = "Population", ""),
                 selectInput("bighornRawNamesLong",
                             label = "Longitude", ""),
                 selectInput("bighornRawNamesLat",
                             label = "Latitude", ""),
                 selectInput("bighornRawNamesID",
                             label = "Individual ID", "")
               )
               
       ), # close Core herd home range boundaries tab
      
      tabPanel("Habitat layer", 
               #includeMarkdown("habitat.md"),
               
               # 1) allow user to upload habitat layer
               
               fileInput('hab_tif', 'Read in habitat layer as .tif file',
                         
                         # list all file types accepted in uploaded
                         accept = c(
                           'tif'
                         ) 
                         
               )
      ),
      
      tabPanel("Bighorn locations",
               
               # function to navigate to, and read in csv
               fileInput('bighornData', 'Read in .csv file of local bighorn locations (if available)',
                         
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )
                         
               ),
               
               # Conditional panel that opens AFTER csv is loaded
               conditionalPanel(
                 
                 condition = "output.bighornDataUploaded == true",
                 
                 # drop-downs for user to specify long, lat, id, and pop fields.
                 'Specify field names for uploaded dataset',
                 
                 selectInput("bighornRawNamesPop",
                             label = "Population", ""),
                 
                 selectInput("bighornRawNamesLong",
                             label = "Longitude", ""),
                 
                 selectInput("bighornRawNamesLat",
                             label = "Latitude", ""),
                 
                 selectInput("bighornRawNamesID",
                             label = "Individual ID", "")
                 
               )

      ), # close bighorn data tab
      
      tabPanel("Season",
               
               tags$hr(),
               
               dateRangeInput('onDate', 'Livestock dates on allotment', start = "2017-01-01", end = "2017-12-30"),
               
               actionButton("addLivestockDates", "Apply livestock dates"),
               
               dateRangeInput('RutDate', 'Bighorn rut', start = "2017-01-01", end = "2017-12-30"),
               
               actionButton("addBighornDates", "Apply bighorn dates")
               
      ) # close Season tab
      
    ) # close navlistPanel
    
  })
  
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$ALLOTSdirectory
    },
    handlerExpr = {
      if (input$ALLOTSdirectory > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        
        # update the widget value
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  

  observe({
    updateSelectInput(
      session,
      "bighornRawNamesLong",
      choices = names(bighornData2()))
  })
  
  observe({
    updateSelectInput(
      session,
      "bighornRawNamesLat",
      choices = names(bighornData2()))
  })
  
  observe({
    updateSelectInput(
      session,
      "bighornRawNamesID",
      choices = names(bighornData2()))
  })
  
  observe({
    updateSelectInput(
      session,
      "bighornRawNamesPop",
      choices = names(bighornData2()))
  })
  
  # reactive test for whether bighornData have been uploaded on Overview panel.
  # Prompts conditionalPanel with field specifications to open.
  
  ## store data reactively
  bighornData2 <- reactive({
    req(input$bighornData)
    inFile <- input$bighornData
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    
    updateSelectInput(session, inputId = 'lat', label = 'Latitude',
                      choices = names(data), selected = names(data))
    updateSelectInput(session, inputId = 'long', label = 'Longitude',
                      choices = names(data), selected = names(data)[2])
    updateSelectInput(session, inputId = 'bighornRawNamesLong', label = 'Herd',
                      choices = names(data), selected = names(data)[2])
    updateSelectInput(session, inputId = 'bighornRawNamesLong', label = 'Longitude',
                      choices = names(data), selected = names(data)[2])
    updateSelectInput(session, inputId = 'bighornRawNamesLat', label = 'Latitude',
                      choices = names(data), selected = names(data)[2])
    updateSelectInput(session, inputId = 'bighornRawNamesID', label = 'SheepID',
                      choices = names(data), selected = names(data)[2])
    
    return(data)
  })
  
  output$bighornDataUploaded <- reactive({
    return(!is.null(bighornData2()))
  })
  outputOptions(output, 'bighornDataUploaded', suspendWhenHidden=FALSE)
  
  # reactive test for whether Population field has been selected.
  # Prompts conditionalPanel with population name selection to open.
  bighornHerdNamesFun <- reactive({
    req(input$bighornRawNamesPop)
    PopField <- input$bighornRawNamesPop
    bighorn.dat.temp <- bighornData2()
    HerdNames <- levels(factor(bighorn.dat.temp[PopField][, 1]))
    return(HerdNames)
  })
  
  ## function that reads user-selected field names back in to the dataframe
  # to specify lat, long, individ, pop; and subsets to those fields
  preppedbighornData <- reactive({
    inFile <- bighornData2()
    longIn <- input$bighornRawNamesLong
    latIn <- input$bighornRawNamesLat
    idIn <- input$bighornRawNamesID
    popIn <- input$bighornRawNamesPop
    names.vec <- c(longIn, latIn, idIn, popIn)
    outFile <- subset(inFile, select = names.vec)
    inFile$NEW <- inFile$longIn
    
    return(outFile)
  })
  
  
  
  #-----------------------#
  #-- 3. Specify inputs --#
  #-----------------------#
  output$specifyInputsPage <- renderUI({
    
    titlePanel("Specify input values")
    
    navlistPanel(
      
      tabPanel("Herd", 

               selectInput("bighornHerdToUse",
                           label = "Herd to Use", "")
               
      ),
      
      actionButton("runHerdSpatAnalysis", "Run Spatial Analysis")

    ) # close navlistPanel
    
  }) # close specifyInputsPage
  
  dir <- reactive(input$dir)
  
  output$dir <- renderPrint(dir())
  
  # path
  path <- reactive({
    
    home <- normalizePath("~")
    
    file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    
  })
  
  # files
  output$files <- renderPrint(list.files(path()))
  
  # conduct bighorn spatial analysis
  runBighornSpatAnalysis <- eventReactive(input$runHerdSpatAnalysis, {
    
    gdb <- system.file(paste(input$input$directory, ".gdb", sep = ""),
                       package = "BHSdataPNF")
    
    ALLOTS <- input.ALLOTS
    
    CHHR <- input.CHHR
    
    HAB <- raster(input.hab_tif)
    
    hab_pref <- data.frame(ID = c(1, 2, 5),
                           VAL = c(1, 0.177, 0.029))
    
    ## ## Optionally plot layers to check that they're correctly registered
    ## plot(HAB) ## (takes ~30 seconds)
    ## plot(CHHR["Id"], add = TRUE,  col = "red")
    ## plot(ALLOTS["UNIT_NAME"], add = TRUE)
    
    
    ##----------------------------------------------------------------------
    ## (2) Run a risk of contact analysis
    #outputDir <- "~/Desktop"
    outputDir <- output$dir
    
    RES <- compute_ROCT(CHHR, 
                        ALLOTS, 
                        HAB,
                        HabPrefTable = hab_pref,
                        nRing = 35,
                        HerdSize = 100,
                        allot_id_col = "UNIT_NAME",
                        return_all_allots = TRUE,
                        foray_prob_raster_dir = outputDir)
    
  })
  
  output$bighornSpaceCHHRCoverage <- renderText({
    
    round(runBighornSpatAnalysis()[[1]], 3)
    
  })
  
  
  
  
  
  #--------------------------#
  #-- 4. View data and map --#
  #--------------------------#
  output$viewDataPage <- renderUI({
    # specify page title
    
    titlePanel("View data and map")
    
    # set up sidebar navigation panel
    
    navlistPanel(  
      
      tabPanel("Map", leafletOutput("bighornMap",width="80%",height="1000px"))
      
    ) # close navlistPanel
    
  })
  
  

  #---------------------#
  #-- 5. View results --#
  #---------------------#
  output$viewResultsPage <- renderUI({
    # specify page title
    
    titlePanel("RoC Results")
    
    # set up sidebar navigation panel
    
    navlistPanel(  
      
      tabPanel("Map", leafletOutput("bighornMap",width="80%",height="1000px"))
      
    ) # close navlistPanel
    
  })
  
  
    
  #-----------------#
  #-- 6. Download --#
  #-----------------#
  output$downloadPage <- renderUI({
    # specify page title
    
    titlePanel("Download results and generating code")
    
    # set up sidebar navigation panel
    
    navlistPanel(  
      
      #tabPanel("Map", leafletOutput("bighornMap",width="80%",height="1000px"))
      # Input: Choose dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Button
      downloadButton("downloadData", "Download")
      
    ) # close navlistPanel
    
  })
  
  #-- Download page required functions
  
  # Reactive value for selected dataset ----
  downloadDataInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  # Table of selected dataset ----
  
  output$table <- renderTable({
    
    downloadDataInput()
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(downloadDataInput(), file, row.names = FALSE)
    }
    
  )
  
}
  