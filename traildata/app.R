#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)



ui <- fluidPage(
    tags$head(HTML("<title>Trail Data Selector</title>")),

    # Application title
    titlePanel(
        fluidRow(
            column(8, "Trail Data Selector"),
            column(4, img(height = 50, src="traillogo.png"))
            )
               ),
    HTML("This app is to extract relevant data from the TRAIL project for analysis. <br/>
       It requires the latest version of the database file to be uploaded, before selecting relevant variables from the menu."),
    br(),

    fluidRow(
        column(2, 
               h4("Choose XLSX File"),
               HTML("<em>Please make sure you use the most up-to-date version of the data.</em>"),
               fileInput("file1", "",
                         multiple = FALSE)
               ),
        column(2,
               h4("Select Timepoints"),
               checkboxGroupInput("timepoints", "", choices = c("Baseline" = "T00", 
                                                                "6 months" = "T06", 
                                                                "12 months" = "T12", 
                                                                "18 months" = "T18",
                                                                "24 months" = "T24",
                                                                "30 months" = "T30",
                                                                "36 months" = "T36",
                                                                "42 months" = "T42",
                                                                "48 months" = "T48"),
                                  selected = "T00")
        ),
        column(2, 
               h4("Select PROMs"),
               checkboxGroupInput("proms", "Variables", choices = c("KOOS" = "koos", 
                                                                    "SPEX" = "spex", 
                                                                    "Knee Self-Efficacy Scale" = "kses", 
                                                                    "Tampa" = "tsk",
                                                                    "ASSQ" = "assq",
                                                                    "Patient Acceptable Symptom Scale (PASS)" = "pass",
                                                                    "VISA-Achilles" = "visaa"
                                                                         ))
               ),
        column(2,
               h4("Select Physical Measures"),
               checkboxGroupInput("physical", "Variables", choices = c("Anthropometrics" = "anthro",
                                                                       "Clinical Measures" = "clinical",
                                                                       "Vicon Info" = "vicon",
                                                                       "Physical Function" = "function",
                                                                       "Biodex" = "biodex"
               ))
                ),
        column(2,
               h4("Select Other Information"),
               checkboxGroupInput("other", "Variables", choices = c("Demographics" = "demographics",
                                                                    "Sport and Running Information" = "sport",
                                                                    "Symptoms" = "symptoms",
                                                                    "Knee OA Beliefs" = "koabeliefs",
                                                                    "Women's Factors" = "womens",
                                                                    "Shoe informaiton" = "shoe",
                                                                    "Minimalist Shoe Index" = "msi",
                                                                    "Injury History" = "injhistory",
                                                                    "Monthly Pain Data" = "monthlypain"
                                                                    ))
        ),
        column(2,
               h4("Select Groups"),
               checkboxGroupInput("group", "Groups", choices = c("Surgical" = "Surgery",
                                                                       "Control" = "Control"),
                                  selected = c("Surgery", "Control"))
        ),
        column(2,
               h4("Pre-Trail Data"),
               checkboxGroupInput("prebaseline", "First data instance, with extra participants", choices = c("Include" = "tp"))
        ),
    ),
    sidebarLayout(
        sidebarPanel(style = "background-color:#ffeae8;",
        h4("Download File"),
               downloadButton("zipdownload","Download")
        ),
        mainPanel()
    )
    # Sidebar with a slider input for number of bins 
)

# Define server 
server <- function(input, output) {
    
    varnamegroup <- read.csv("namegroups.csv", header = TRUE, na = "")
    
    data <-  reactive({
        req(input$file1) # wait until file uploaded
        
        readxl::read_xlsx(input$file1$datapath, 
                          sheet = 1, na = "NA")
    })
    
    injury <-  reactive({
        req(input$file1) # wait until file uploaded
        
        readxl::read_xlsx(input$file1$datapath, 
                          sheet = 2, na = "NA")
    })
    
    monthlypain <-  reactive({
        req(input$file1) # wait until file uploaded
        
        readxl::read_xlsx(input$file1$datapath, 
                          sheet = 3, na = "NA")
    })
    
    predata <-  reactive({
        req(input$file1) # wait until file uploaded
        
        readxl::read_xlsx(input$file1$datapath, 
                          sheet = 4, na = "NA")
    })
    
    namesout <- reactive({
        varnamegroup %>%
            dplyr::filter(namegroup %in% c(input$proms, input$physical, input$other))
    })
    
    dataout <- reactive({
        data() %>%
            dplyr::filter(timepoint %in% input$timepoints) %>%
            dplyr::filter(group %in% input$group) %>%
            dplyr::select(id, studyentry_date, labtest_date, dob, sex, group, surgerytype, date_surgerytype, age, knee_reference, dominantlimb, 
                          height, weight, timepoint, timepoint_date, any_of(namesout()$varname))
        
    })
    
    injuryout <- reactive({
        injury() %>%
            dplyr::filter(group %in% input$group)
    })
    
    monthlypainout <- reactive({
        monthlypain() %>%
            dplyr::filter(group %in% input$group)
    })
    
    predataout <- reactive({
        predata() %>%
            dplyr::filter(group %in% input$group) %>%
            dplyr::select(id, studyentry_date, labtest_date, dob, sex, group, surgerytype, date_surgerytype, age, knee_reference, dominantlimb, 
                          timepoint, timepoint_date, any_of(namesout()$varname))
        
    })
    
    # Add Data file if Data are selected in menu
    file1 <- reactive({
        if(length(input$timepoints) > 0 & (length(input$proms) > 0 | length(input$physical) > 0 | length(input$other) > 0)) { c(paste(paste("Trail Data", Sys.Date(), sep = "_"), "csv", sep = ".")) }
    })
    
    # Add Injury testing file if selected in menu
    
    file2 <- reactive({
        ifelse(match("injhistory", input$other), c(paste(paste("Injury Information", Sys.Date(), sep = "_"), "csv", sep = ".")), c())
    })
    
    file3 <- reactive({
        ifelse(match("monthlypain", input$other), c(paste(paste("Monthly Pain", Sys.Date(), sep = "_"), "csv", sep = ".")), c())
    })
    
    file4 <- reactive({
        ifelse(match("tp", input$prebaseline), c(paste(paste("Pre-Trail", Sys.Date(), sep = "_"), "csv", sep = ".")), c())
    })
    
    # Create list of which files to download
    files <- reactive({
        c(file1(), file2(), file3(), file4())
    })
    
    output$zipdownload = downloadHandler(
        filename = paste(paste("Trail Data", Sys.Date(), sep = "_"), "zip", sep = "."),
        content = function( file){
            
            # Set temporary working directory
            owd <- setwd( tempdir())
            on.exit( setwd( owd))
            
            # Save the files 
            write_csv(dataout(), paste(paste("Trail Data", Sys.Date(), sep = "_"), "csv", sep = "."))
            write_csv(injuryout(), paste(paste("Injury Information", Sys.Date(), sep = "_"), "csv", sep = "."))
            write_csv(monthlypainout(), paste(paste("Monthly Pain", Sys.Date(), sep = "_"), "csv", sep = "."))
            write_csv(predataout(), paste(paste("Pre-Trail", Sys.Date(), sep = "_"), "csv", sep = "."))
            
            # Zip them up
            zip(file, files())
        },
        contentType = "application/zip"
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
