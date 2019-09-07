library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(shinythemes)

grants <- read.csv("GrantData.csv", 
                   header=TRUE, sep=",",na.strings = c("", "NA"))


# Define UI for application  -------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # Application title --------------------------------------------------------
  titlePanel("2010 School Grants Across States"),
  
  # Sidebar layout with a input and output definitions -----------------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ---------------------------------------
    sidebarPanel(
      
      # Input: Choose state (begin alphabetically) ---------------------------
      selectInput(inputId = "State",
                  label = "Choose State:",
                  choices = as.list(levels(grants$State))),
      
      # Add space for visual separation --------------------------------------
      br(),
      br(),
      
      # Input: Create a range for awarded money ------------------------------
      sliderInput(inputId = "Range", 
                  label = "Award Range ($) for Data Table:",
                  min = 1, max = 1000,
                  value = c(200,800)),
      
      # Input: Show data table -----------------------------------------------
      checkboxInput(inputId = "show_data",
                    label = "Show Data Table?",
                    value = TRUE),
      
      # Horizontal line for visual separation --------------------------------
      hr(),
      br(),
      br(),
      
      # Select sample size ---------------------------------------------------
      numericInput(inputId = "n_samp", 
                   label = "Sample size:", 
                   min = 1, max = nrow(grants), 
                   value = 50),
      
      # Add space for visual separation --------------------------------------
      br(),
      br(),
      
      ## Download Button------------------------------------------------------
      downloadButton("download_data", "Download All Data")
    ),
    
    
    # Output: ----------------------------------------------------------------
    mainPanel(
      
      # Show plots -----------------------------------------------------------
      plotOutput("boxplot"),
      
      plotOutput("barchart"),
      
      plotOutput("piechart"),
      
      # Show data table ------------------------------------------------------
      DT::dataTableOutput(outputId = "grantstable")
    )
  )
)

# Define server function required to create the boxplot ----------------------
server <- function(input, output, session) {

  

  
  state_subset <- reactive({
    req(input$State) # ensure availablity of value
    filter(grants, State %in% input$State)
  })
   
  
  # Update the maximum allowed n_samp for selected type movies ---------------
  observe({

    updateNumericInput(session, 
                       inputId = "n_samp",
                       value = min(50, nrow(state_subset())),
                       max = nrow(state_subset())
    )
  })

  # Create new df that is n_samp obs from selected state ---------------------

  grants_sample <- reactive({ 
    req(input$n_samp) # ensure availablity of value
    sample_n(state_subset(), input$n_samp)
  })

  # Create plot objects the plotOutput functions are expecting ---------------

  output$boxplot <- renderPlot({
    ggplot(grants_sample(), aes(x=Model, y=Award, fill=Model)) + 
      geom_boxplot() + 
      labs(title = "Grant Amount Awarded by Model", 
           y = "($) Awarded") +
      scale_fill_brewer(palette="Blues") + 
      theme_classic()
  })
  
  output$barchart <- renderPlot({
    ggplot(grants_sample(), aes(x=City, y=Award, fill=Model)) + 
      labs(title = "Grant Amount Awarded by City",
           y = "($) Awarded") +
      geom_bar(stat="identity") +
      scale_fill_brewer(palette="Blues") + 
      theme_classic()
  })
  
  output$piechart <- renderPlot({
    ggplot(grants_sample(), aes(x=State, y=Award, fill=District)) +
      labs(title="Grant Amount Awarded by District within State", 
           x = "", y = "") +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette="YiGnBu") + 
      theme_classic()
  })


  # Print data table if checked ----------------------------------------------
  output$grantstable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = filter(grants_sample(), 
                                  Award %in% input$Range[1]:input$Range[2]), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  # Download Data ------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(data_to_download, con)
    }
  )
}




# Run the application --------------------------------------------------------
shinyApp(ui = ui, server = server)