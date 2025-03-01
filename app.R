
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(reshape2)

# Load your dataset (replace with actual dataset file path)
athlete_data <- read.csv("/Users/duncankrige/Documents/LPC(BASEBALL)/ForcePlate1.csv")  # Update with correct CSV file path

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Athlete Force Plate Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select player names
      selectInput("player", "Select Athlete:", choices = NULL, selected = NULL),
      
      # Date range input for filtering
      dateRangeInput("dateRange", "Select Date Range:",
                     start = min(as.Date(athlete_data$Date, format = "%m/%d/%Y")),
                     end = max(as.Date(athlete_data$Date, format = "%m/%d/%Y"))),
      
      # Dropdown to select metric for plotting
      selectInput("metric", "Select Metric:",
                  choices = list("CMJ Height" = "CMJ.Height",
                                 "ARBF" = "ARBF",
                                 "BNI" = "BNI",
                                 "ARPF" = "ARPF",
                                 "PNI" = "PNI",
                                 "Jump Momentum" = "Jump.Momentum",
                                 "Peak Relative Propulsive Power" = "Peak.Rel.Prop.Power",
                                 "mRSI" = "mRSI",
                                 "Bodyweight" = "Bodyweight",
                                 "L/R Brake Index" = "LR.Brake.Index",
                                 "L/R Propulsion Index" = "LR.Prop.Index")),
      
      # Dropdown to select scatterplot metrics
      selectInput("xMetric", "X-axis Metric for Scatterplot:",
                  choices = list("CMJ Height" = "CMJ.Height", "ARPF" = "ARPF", "PNI" = "PNI", "Bodyweight" = "Bodyweight")),
      selectInput("yMetric", "Y-axis Metric for Scatterplot:",
                  choices = list("Jump Momentum" = "Jump.Momentum", "CMJ Height" = "CMJ.Height", "ARPF" = "ARPF", "PNI" = "PNI"))
    ),
    
    mainPanel(
      h4("Metric Definition"),
      textOutput("metricDefinition"),  # Display metric definition
      
      h4("Summary Statistics"),
      verbatimTextOutput("summaryStats"),  # Display summary statistics for selected athlete
      
      h4("Metric Visualization"),
      plotOutput("metricPlot"),  # Display selected metric visualization
      
      h4("Scatterplot of Selected Metrics"),
      plotOutput("scatterPlot"),  # Scatterplot for metric relationships
      
      h4("Athlete Data"),
      tableOutput("athleteTable")  # Display filtered athlete data
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Populate player names in the dropdown (selectInput)
  observe({
    updateSelectInput(session, "player", choices = unique(athlete_data$Athlete.Name))
  })
  
  # Reactive expression to filter the data based on input
  filteredData <- reactive({
    req(input$player, input$dateRange)
    
    athlete_data %>%
      filter(Athlete.Name == input$player,
             as.Date(Date, format = "%m/%d/%Y") >= input$dateRange[1],
             as.Date(Date, format = "%m/%d/%Y") <= input$dateRange[2])
  })
  
  # Display filtered athlete data in a table
  output$athleteTable <- renderTable({
    filteredData()
  })
  
  # Summary statistics for key metrics (CMJ Height, ARPF, PNI)
  output$summaryStats <- renderText({
    req(filteredData())
    summary_stats <- filteredData() %>%
      summarise(
        CMJ_Height_Mean = mean(CMJ.Height, na.rm = TRUE),
        CMJ_Height_SD = sd(CMJ.Height, na.rm = TRUE),
        CMJ_Height_Range = paste(min(CMJ.Height, na.rm = TRUE), "-", max(CMJ.Height, na.rm = TRUE)),
        ARPF_Mean = mean(ARPF, na.rm = TRUE),
        ARPF_SD = sd(ARPF, na.rm = TRUE),
        ARPF_Range = paste(min(ARPF, na.rm = TRUE), "-", max(ARPF, na.rm = TRUE)),
        PNI_Mean = mean(PNI, na.rm = TRUE),
        PNI_SD = sd(PNI, na.rm = TRUE),
        PNI_Range = paste(min(PNI, na.rm = TRUE), "-", max(PNI, na.rm = TRUE))
      )
    
    paste("CMJ Height (Mean):", round(summary_stats$CMJ_Height_Mean, 2),
          "\nCMJ Height (SD):", round(summary_stats$CMJ_Height_SD, 2),
          "\nCMJ Height (Range):", summary_stats$CMJ_Height_Range,
          "\nARPF (Mean):", round(summary_stats$ARPF_Mean, 2),
          "\nARPF (SD):", round(summary_stats$ARPF_SD, 2),
          "\nARPF (Range):", summary_stats$ARPF_Range,
          "\nPNI (Mean):", round(summary_stats$PNI_Mean, 2),
          "\nPNI (SD):", round(summary_stats$PNI_SD, 2),
          "\nPNI (Range):", summary_stats$PNI_Range)
  })
  
  # Plot the selected metric
  output$metricPlot <- renderPlot({
    req(filteredData())
    
    # Get the selected metric from input
    metric_col <- input$metric
    
    ggplot(filteredData(), aes_string(x = "Date", y = metric_col)) +
      geom_line() + 
      geom_point() +
      labs(title = paste("Metric:", metric_col, "for", input$player),
           x = "Date", y = metric_col) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Scatterplot of selected metrics
  output$scatterPlot <- renderPlot({
    req(filteredData())
    
    # Get the selected x and y metrics from input
    x_metric <- input$xMetric
    y_metric <- input$yMetric
    
    ggplot(filteredData(), aes_string(x = x_metric, y = y_metric)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear trendline
      labs(title = paste("Scatterplot of", x_metric, "vs", y_metric),
           x = x_metric, y = y_metric) +
      theme_minimal()
  })
  

  # Metric definitions
  metricDefinitions <- list(
    "CMJ.Height" = "• Definition: This refers to the height (in centimeters) an athlete achieves during a Countermovement Jump (CMJ), a common test to assess lower-body power.\n• Why it’s important: CMJ height is a direct measure of an athlete’s explosive strength and jump performance. A higher jump indicates better lower body power output.",
    "ARBF" = "• Definition: ARBF stands for Average Reactive Braking Force. It measures the braking force applied by the athlete during landing.\n• Why it’s important: It indicates how well an athlete can decelerate after a jump, which is crucial for injury prevention and performance efficiency.",
    "BNI" = "• Definition: BNI (Braking Net Impulse) refers to the total braking force exerted during landing.\n• Why it’s important: BNI shows how much force is used to decelerate, which helps in understanding the athlete's braking control.",
    "ARPF" = "• Definition: ARPF stands for Average Relative Propulsive Force. It measures the amount of force used by the athlete during the propulsive phase of the jump.\n• Why it’s important: Higher ARPF indicates better explosiveness and power generation in the jump.",
    "PNI" = "• Definition: PNI (Propulsive Net Impulse) measures the net force used during the propulsive phase of the jump.\n• Why it’s important: It gives insights into how much force is generated by the athlete to push off the ground.",
    "Jump.Momentum" = "• Definition: This metric refers to the overall momentum generated during the jump, combining mass and velocity.\n• Why it’s important: Momentum indicates the overall efficiency of force production during a jump, helping coaches evaluate an athlete’s explosive capabilities.",
    "Peak.Rel.Prop.Power" = "• Definition: Peak Relative Propulsive Power refers to the maximum power output during the jump, adjusted for the athlete's body weight.\n• Why it’s important: It’s a key indicator of explosive power, adjusted for the athlete’s size, which helps compare athletes of different body weights.",
    "mRSI" = "• Definition: mRSI stands for Modified Reactive Strength Index. It’s a ratio of jump height to ground contact time during a jump.\n• Why it’s important: Higher mRSI indicates better reactive strength, a crucial component of athletic performance, especially in sports requiring repeated jumps.",
    "Bodyweight" = "• Definition: The athlete’s body weight, measured in kilograms.\n• Why it’s important: Body weight is crucial for normalizing performance metrics and comparing force generation relative to mass.",
    "LR.Brake.Index" = "• Definition: L/R Brake Index measures the balance between the left and right legs during the braking phase of the jump.\n• Why it’s important: Imbalances can highlight potential injury risks or weaknesses in one leg compared to the other.",
    "LR.Prop.Index" = "• Definition: L/R Propulsion Index measures the balance between the left and right legs during the propulsion phase of the jump.\n• Why it’s important: Similar to the brake index, this highlights any imbalances in force production between the legs."
  )
  
  # Display metric definition based on the selected metric
  output$metricDefinition <- renderText({
    req(input$metric)
    metricDefinitions[[input$metric]]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
