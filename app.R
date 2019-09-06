library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)

grants <- read.csv("GrantData.csv", 
                   header=TRUE, sep=",",na.strings = c("", "NA"))


# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("School Grants: 2010"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("District" = "District", 
                              "City" = "City", 
                              "State" = "State", 
                              "Model" = "Model"), 
                  selected = "Model"),
      
      # Show data table ---------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show Data Table",
                    value = TRUE),
      
      
      
      # Horizontal line for visual separation -----------------------
      hr(),
      
      # Select which types of models to plot ------------------------
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select model type(s):",
                         choices = c("Closure", "Restart", "Transformation", "Turnaround"),
                         selected = "Transformation"),
      
      # Select sample size ----------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(grants), 
                   value = 50)
    ),
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      # Show boxplot ------------------------------------------------
      plotOutput("boxplot"),
      
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "grants")
    )
  )
)

# Define server function required to create the boxplot ---------
server <- function(input, output, session) {
  
  
  # Update the maximum allowed n_samp for selected type movies ------
  observe({

    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(grants())),
                       max = nrow(grants())
    )
  })
  
  # Create new df that is n_samp obs from selected type movies ------
  grants_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value before proceeding
    sample_n(grants(), input$n_samp)
  })
  
  
  # Create boxplot object the plotOutput function is expecting --

  output$boxplot <- renderPlot({
    boxplot(data = grants_sample(),
            pch = 19)
    
  })

  # Print data table if checked -------------------------------------
  output$grantstable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = grants_sample()[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
    
  )
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)