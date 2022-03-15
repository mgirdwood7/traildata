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


# Define UI for application that draws a histogram
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
               h4("Choose CSV File"),
               HTML("<em>Please make sure you use the most up-to-date version of the data.</em>"),
               fileInput("file1", "",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               ),
        column(2,
               h4("Select Timepoints"),
               checkboxGroupInput("timepoints", "", choices = c("Pre-Baseline (Recruitment)" = "TP1",
                                                                "Baseline" = "T00", 
                                                                "6 months" = "T06", 
                                                                "12 months" = "T12", 
                                                                "18 months" = "T18",
                                                                "24 months" = "T24"),
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
                                                                    "Injury History" = "injhistory"),
                                  selected = "demographics")
        ),
        column(2,
               h4("Select Groups"),
               checkboxGroupInput("group", "Groups", choices = c("Surgical" = "Surgical",
                                                                       "Control" = "Control"),
                                  selected = "Surgical")
        ),
    ),
    sidebarLayout(
        sidebarPanel(style = "background-color:#ffeae8;",
        h4("Download File"),
               downloadButton("downloadData","Download")
        ),
        mainPanel()
    )
    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    varnamegroup <- read.csv("namegroups.csv", header = TRUE, na = "")
    
    data <- reactive({
        req(input$file1) # wait until file uploaded
        
        read.csv(input$file1$datapath, 
                     header = TRUE, 
                     na = NA)
    })
    
    namesout <- reactive({
        varnamegroup %>%
            dplyr::filter(namegroup %in% c(input$proms, input$physical, input$other))
    })
    
    dataout <- reactive({
        data() %>%
            dplyr::filter(timepoint %in% input$timepoints) %>%
            dplyr::filter(group %in% input$group)
            dplyr::select(UUID, Date, timepoint, any_of(namesout()$varname))
        
    })
    
    output$head <- renderTable({
        head(data())
    })
    
    output$downloadData <- downloadHandler(
        filename=(paste("Trail Data", Sys.Date(), ".csv")),
        content=function(file){
            write_csv(dataout(), file)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
