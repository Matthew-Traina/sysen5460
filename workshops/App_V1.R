#' @name App_V1.R
#' @author Kayleigh Calder, Olivia Thomas, Christian Stack, Matthew Traina
#' @title Japanese Solar Adoption Rates Dashboard V1
#' @description 
#' This Shiny app visualizes Japanese solar adoption rates based on user inputs between disaster occurrence, population, and date. 

# Global function to load libraries and data
global <- function() {
  library(dplyr)   # Data wrangling
  library(readr)   # Reading data
  library(ggplot2) # Data visualization
  library(shiny)   # Main Shiny package
  library(bslib)   # Easier HTML construction
}

# Import Data ########################################

# Import Japanese solar power database as an object
jp <- read_csv("/cloud/project/data/jp_solar.csv") %>%
  select(date, year, solar_rate, disaster, pop) %>%
  arrange(date, year) %>%
  group_by(date)

# Investigate the solar rate 
jp$solar_rate %>% summary()

# Check if the sum of solar rates exceed 10 per city
test <- jp %>%
  filter(solar_rate >= 10)

# Check the outlier - if all dates are 2017-09-25
test %>% print(n=124)

# Remove/filter out date outlier
jp <- jp %>%
  filter(date != "2017-09-25")

# Summarize the population data
jp$pop %>% summary()

# Extract summary_stats into a data frame
summary_stats <- summary(jp$pop)

# Extract the 1st quartile and 3rd quartile
first_quartile <- summary_stats["1st Qu."]
third_quartile <- summary_stats["3rd Qu."]

# Add new column for categorical population size
# Add new column for categorical population size
jp <- jp %>%
  mutate(population_size = case_when(
    pop <= first_quartile ~ "Low Population (0-5829)", # Create a low category
    pop > first_quartile & pop < third_quartile ~ "Medium Population (5830-23881)", # Create a medium category 
    pop >= third_quartile ~ "High Population (23882-335444)" # Create a high category
  ))

# Add a new column for categorical disaster label
jp <- jp %>%
  mutate(disaster_category = case_when(
    disaster == 0 ~ "No Disaster", # Create a No Disaster category
    disaster == 1 ~ "Disaster" # Create a Disaster category
  ))

# UI #################################################################

ui <- function(){   
  # Create vectors for population and disaster selections
  choices_population <- setNames(object = jp$population_size, nm = jp$population_size)
  choices_disaster <- setNames(object = jp$disaster_category, nm = jp$disaster_category)
  
  # Create a title card
  c1 <- card(
    card_header(class = "bg-primary",
                card_title("Do Disasters Affect Japanese Rooftop Solar Adoption Rates?"))
  )
  
  # Create a selector card
  c2 <- bslib::card(
    card_header(card_title("FILTER DATA")),
    card_body(
      # Create checkboxes for disaster user input
      checkboxGroupInput(inputId = "disaster", label = "DISASTER IMPACT", 
                         choices = list("Municipalities Affected by a Disaster" = 1, "Municipalities Not Affected by a Disaster" = 0),
                         selected = c(1, 0)),
      # Create drop down menu for populaton user inputs
      selectInput(inputId = "population", label = "MUNICIPALITY POPULATION", choices = choices_population, selected = c(1)),
      # Create slider for date user inputs
      sliderInput(inputId = "date_range", label = "DATE RANGE", 
                  min = min(jp$date), max = max(jp$date), 
                  value = c(min(jp$date), max(jp$date)),
                  timeFormat = "%Y-%m-%d"),
      
    )
  )
  
  # Create a plot card
  c3 <- bslib::layout_column_wrap(
    card(plotlyOutput(outputId = "plot_one_time_series")), # Create time series plot
    card(plotlyOutput(outputId = "plot_one_box_plot")), # Create box plot
    width = 12
  )
  
  # Create a text card
  c4a <- bslib::card(
    bslib::card_body(textOutput("text_highlight_a"))
  )
  
  c4b <- bslib::card(
    bslib::card_body(textOutput("text_highlight_b"))
  )
  
  # VALUE BOXES CARD ##########################
  
  box0 = uiOutput("fullTitle")
  
  box1 = bslib::value_box(
    title = "The Average Solar Adoption Rate of Japan was ", value = textOutput("OutputText_mean"), "per 1000 Residents per Month",
    class = "bg-primary text-light")
  
  box2 = bslib::value_box(
    title = "Disaster Affected Areas Accounted for", textOutput("OutputText_disasterPercentage"), "of Total Rooftop Solar Installs",
    class = "bg-primary text-light"
  )
  
  
  
  c5 = card(
    # Bundle the value boxes together
    box0,
    card_body(
      layout_column_wrap(box1,box2, width = 1/2)      
    )
  )
  
  # Define the page layout
  bslib::page(
    title = "Solar Options", 
    theme = bslib::bs_theme(preset = "cerulean"),
    c1, 
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(c2), 
      c5,
      c3,
      layout_column_wrap(c4a,c4b, width = 1/2)
    )
  )
}

# Server #################################################################

server <- function(input, output, session) {
  
  # Create a reactive expression to filter charts based on user inputs
  reactive_data <- reactive({
    
    # Create a filter for disaster
    disaster_filter <- if(is.null(input$disaster) || length(input$disaster) == 0) c(0, 1) else input$disaster
    # Create a filter for population 
    population_filter <- if(is.null(input$population) || input$population == "") unique(jp$population_size) else input$population
    
    # Filter the dataset based on user inputs 
    filtered_data <- jp %>%
      filter(population_size %in% population_filter, # Filter population based on user input
             date >= input$date_range[1], # Filter starting date based on slider input
             date <= input$date_range[2], # Filter ending data based on slider input
             disaster %in% disaster_filter) # Filter disaster based on user input
    
    # Check if both checkboxes are not selected
    if (length(input$disaster) == 0) {
      # Group by date only
      filtered_data %>%
        group_by(date) %>%
        summarise(avg_solar_rate = mean(solar_rate, na.rm = TRUE), .groups = "drop")
    } else {
      # Group by date and disaster category
      filtered_data %>%
        group_by(date, disaster_category) %>%
        summarise(avg_solar_rate = mean(solar_rate, na.rm = TRUE), .groups = "drop")
    }
  })
  
  # Reactive data filtered only by date for mean calculation
  reactive_data_date_only <- reactive({
    jp %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      summarise(avg_solar_rate = mean(solar_rate, na.rm = TRUE))
  })
  
  # Reactive expression for the summary statistics of the date-filtered data
  reactive_summary_solar <- reactive({
    summary(reactive_data_date_only()$avg_solar_rate)
  })
  
  # Reactive expression for the overall mean based on date-filtered data
  reactive_solar_mean <- reactive({
    mean(reactive_data_date_only()$avg_solar_rate, na.rm = TRUE)
  })
  
  
  reactive_disaster_percentage <- reactive({
    # Filter the data for the selected date range
    filtered_data <- jp %>%
      filter(date >= as.Date(input$date_range[1]) &
               date <= as.Date(input$date_range[2]))
    
    # Calculate the total solar installation rate
    total_solar_rate <- sum(filtered_data$solar_rate, na.rm = TRUE)
    
    # Calculate the solar installation rate for disaster-struck areas
    disaster_solar_rate <- sum(filtered_data$solar_rate[filtered_data$disaster == 1], na.rm = TRUE)
    
    # Calculate the proportion of solar rate in disaster areas to the total
    if (total_solar_rate > 0) {
      proportion <- (disaster_solar_rate / total_solar_rate) * 100
    } else {
      proportion <- 0  # Avoid division by zero if no data
    }
    
    return(proportion)
  })
  
  # Update the output to display the proportion
  output$OutputText_disasterPercentage <- renderText({
    paste(format(reactive_disaster_percentage(), nsmall = 2), "%")
  })
  
  output$fullTitle <- renderUI({
    req(input$date_range)  # Ensure the date range input is available
    div(
      style = "font-weight: bold; text-align: center;",  # Add bold and center styling
      paste("For Date Range: ",
            format(as.Date(input$date_range[1]), "%Y-%m-%d"), 
            " to ", 
            format(as.Date(input$date_range[2]), "%Y-%m-%d"))
    )
  })
  
  
  output$OutputText_mean = renderText({
    reactive_solar_mean()
  })
  
  # Create the time series plot using Plotly
  output$plot_one_time_series <- renderPlotly({
    
    req(reactive_data())  # Ensure reactive_data is available
    
    gg <- ggplot(reactive_data(), aes(x = date, y = avg_solar_rate)) +
      geom_line(aes(color = if (length(input$disaster) == 0) NULL else factor(disaster_category),  color = "Overall")) +
      geom_hline(aes(yintercept = reactive_solar_mean(), color = "Average"), linetype = "dashed", show.legend = TRUE) +
      labs(x = "Date", y = "Average Solar Adoption Rate \n(Normalized for 10000 Residents)", color = "Legend",
           title = "Rooftop Solar Unit Adoption Rates over Time") +
      scale_color_manual(values = c("No Disaster" = "steelblue", "Disaster" = "orange", "Average" = "grey")) +
      theme_minimal() +
      theme(legend.position = "top", plot.title = element_text(vjust = -5),
            axis.title.x = element_text(vjust = -0.5))  # Adjust vertical position of the x-axis title
    
    # Convert ggplot object to Plotly
    ggplotly(gg)
  })
  
  # Create the box plot
  output$plot_one_box_plot <- renderPlotly({
    
    req(reactive_data())  # Ensure reactive_data is available
    
    gg2 <- ggplot(reactive_data(), mapping = aes(x = if(length(input$disaster) == 0) "Overall" else disaster_category, y = avg_solar_rate, color = disaster_category)) +
      geom_boxplot(aes(color = if (length(input$disaster) == 0) NULL else factor(disaster_category))) +
      labs(x = "Disaster Occurrence", 
           y = "Average Solar Adoption Rate \n(Normalized for 10000 Residents)",
           title = "Rooftop Solar Units based on Disaster Occurrence") +
      scale_color_manual(values = c("No Disaster" = "steelblue", "Disaster" = "orange")) + 
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(gg2, tooltip = c("Disaster Occurrence", "avg_solar_rate"))
  })
  
  # Render text output for the time series chart
  output$text_highlight_a <- renderText({
    req(reactive_data())
    
    # If checkboxes are not selected, output custom text
    if (is.null(input$disaster) || length(input$disaster) == 0) {
      paste(
        "The Solar Rate Over Time Plot for", input$population, "Population Municipalities between", 
        input$date_range[1], "and", input$date_range[2], "is shown above.\nThis does not distinguish between disaster and non-disaster areas."
      )
      
      # If checkboxes are selected, output this text
    } else {
      paste(
        "The Solar Rate Over Time Plot for", input$population, "Population Municipalities between", 
        input$date_range[1], "and", input$date_range[2], "is shown above. Japan's Average Solar Rate during the selected time period is shown in grey dashed line."
      )
    }
  })
  
  
  # Render text output for the box plot
  output$text_highlight_b <- renderText({
    req(reactive_data())
    
    # If checkboxes are not selected, output custom text
    if (is.null(input$disaster) || length(input$disaster) == 0) {
      paste(
        "As shown above, the Maximum Solar Rate is", round(max(reactive_data()$avg_solar_rate), 3),
        "and the Minimum Solar Rate is", round(min(reactive_data()$avg_solar_rate), 3),
        "for", input$population, "Population Municipalities between",
        format(input$date_range[1], "%Y-%m-%d"), "and", format(input$date_range[2], "%Y-%m-%d"), ". \nThis does not distinguish between disaster and non-disaster areas."
      )
      
      # If checkboxes are selected, output this text
    } else {
      paste(
        "As shown above, the Maximum Solar Rate is", round(max(reactive_data()$avg_solar_rate), 3),
        "and the Minimum Solar Rate is", round(min(reactive_data()$avg_solar_rate), 3),
        "for", input$population, "Population Municipalities between",
        format(input$date_range[1], "%Y-%m-%d"), "and", format(input$date_range[2], "%Y-%m-%d"), "."
      )
    }
  })
}

# Run the Shiny app ####################################################

shiny::shinyApp(ui = ui, server = server, onStart = global)