library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(stringr)
library(tools)
library(readxl)


CFCdata <- read_excel("https://github.com/beccaellenhall/funwithR/blob/master/CFC%20Data/CharterEnrollmentSampleData-2.xlsx", 
     sheet = "ComSch_Enrollment")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("CFC Dataset", windowTitle = "CSC463 Final Presentation R_Hall"),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Inputs
    sidebarPanel(
      
    wellPanel(
      h3("Plotting"),

      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Department"          = "Department", 
                              "Actual Value" = "ActualValue", 
                              "Forecast Value"        = "ForecastValue", 
                              "Variance Percentage"       = "VariancePerc", 
                              "Forecast Date"              = "ForecastDate"), 
                  selected = "VariancePerc"),

      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Department"          = "Department", 
                              "Actual Value" = "ActualValue", 
                              "Forecast Value"        = "ForecastValue", 
                              "Variance Percentage"       = "VariancePerc", 
                              "Forecast Date"              = "ForecastDate"), 
                  selected = "ForecastDate")
    ),
    
    wellPanel(
      h3("Select Color"),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Department" = "Department", 
                              "Forecast Date" = "ForecastDate", 
                              "Actual Value" = "ActualValue"),
                  selected = "Department")
    ),
      ###### Download File
    wellPanel(
      h3("Download File"),
      
            # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype for download:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),
      
      # Select variables to download
      checkboxGroupInput(inputId = "selected_var",
                  label = "Select variables to download:",
                  choices = names(CFCdata),
                  selected = c("title"))
      ############
    ),
      wellPanel(
            # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE)
      )
      ),

    # Outputs
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  
                  #New tab panel for Scatterplot
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "scatterplot"),
                           br(),
                           h5(textOutput("description"))),
                  
                  #New tab panel for Statistics
                  tabPanel(title = "Statistics",
                  htmlOutput(outputId = "avgs"),
                  verbatimTextOutput(outputId = "lmoutput"),
                  br()),
                  
                  # New tab panel for Data Table
                  tabPanel(title = "Data", 
                           br(),
                           conditionalPanel(condition = "input.show_data == true",
                           DT::dataTableOutput(outputId = "CFCdataTable"))),
                  
                  # New tab panel for Downloading
                  tabPanel("Download", 
                  br(),
                  HTML("Select filetype and variables, then hit 'Download data'."),
                  br(), br(), # line break and some visual separation
                  downloadButton("download_data", "Download data"))
  )
)
)
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  ######## Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("CFCdata", input$filetype)
      },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(CFCdata %>% select(input$selected_var), path = file) 
        }
      if(input$filetype == "tsv"){ 
        write_tsv(CFCdata %>% select(input$selected_var), path = file) 
        }
    }
  )
  ##########
  
    # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })

 # Create scatterplot
  output$scatterplot <- renderPlot({
    ggplot(data = CFCdata, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
  })
  
  ####### Statistics
  # Calculate averages
  output$avgs <- renderUI({
    avg_x <- CFCdata %>% pull(input$x) %>% mean() %>% round(2)
    avg_y <- CFCdata %>% pull(input$y) %>% mean() %>% round(2)
    HTML(
      paste("Average", input$x, "=", avg_x),
      "<br/>",
      paste("Average", input$y, "=", avg_y)
    )
  })
  
  # Create regression output
  output$lmoutput <- renderPrint({
    x <- CFCdata %>% pull(input$x)
    y <- CFCdata %>% pull(input$y)
    print(summary(lm(y ~ x, data = CFCdata)), digits = 3, signif.stars = FALSE)
  })
  #########
  
    # Print data table if checked
  output$CFCdataTable <- DT::renderDataTable(
    DT::datatable(data = CFCdata, 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )

}

# Create the Shiny app object
shinyApp(ui = ui, server = server)
