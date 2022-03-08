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

    # Application title
    titlePanel("Trail Data Selector"),

    fluidRow(
        column(2,
               fileInput("file1", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               ),
        column(2, 
               h4("Select Variables"),
               checkboxGroupInput("varchoices", "Variables", choices = c("KOOS" = "koos", 
                                                                         "SPEX" = "spex", 
                                                                         "Knee Self-Efficacy Scale" = "kses", 
                                                                         "Tampa" = "tsk",
                                                                         "ASSQ" = "assq",
                                                                         "Patient Acceptable Symptom Scale (PASS)" = "pass",
                                                                         "VISA-Achilles" = "visaa",
                                                                         "Lab Testing" = "lab"
                                                                         ))
               ),
        column(2,
               h4("Select Timepoints"),
               checkboxGroupInput("timepoints", "", choices = c("Baseline" = "T00", "6 months" = "T06", "12 months" = "T12", "24 months" = "T24"))
               ),
        column(2, 
               tableOutput("head"))
    ),
    fluidRow(
        column(10,
               h4("Download File"),
               downloadButton("downloadData","Download")
        )
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
            dplyr::filter(namegroup %in% input$varchoices)
    })
    
    dataout <- reactive({
        data() %>%
            dplyr::filter(timepoint %in% input$timepoints) %>%
            dplyr::select(UUID, Date, timepoint, any_of(namesout()$varname))
        
    })
    
    output$head <- renderTable({
        head(data())
    })
    
    output$downloadData <- downloadHandler(
        filename=("trailtest.csv"),
        content=function(file){
            write_csv(dataout(), file)
        }
    )

}

# Run the application 
shinyApp(ui = ui, server = server)
