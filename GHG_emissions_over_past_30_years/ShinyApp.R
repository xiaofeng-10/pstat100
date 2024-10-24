library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

emissions_data <- read.csv("data/historical_emissions.csv", 
                           na.strings = "N/A")
emissions_data_long <- emissions_data %>% 
  select(-Data.source, - Sector, -Gas, -Unit) %>% 
  pivot_longer(cols = -Country, names_to = "Year", values_to = "Emissions") %>%
  mutate(Year = as.numeric(substring(Year, 2)))

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "united"),
  titlePanel("Historical GHGs Emissions by Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Choose a country:", 
                  choices = unique(emissions_data_long$Country)),
      p(style = 'color: grey;', 'Here you can select any country to see its Gas Emissions change over the years.'),
      hr(),
      sliderInput("year", "Select Year:", 
                  min = min(emissions_data_long$Year), 
                  max = max(emissions_data_long$Year), 
                  value = max(emissions_data_long$Year)),
      p(style = "color: grey;","This slider controls the year of gas emissions data for the countries you have selected to compare."),
      selectInput("compareCountries", "Select countries to compare:", 
                  choices = unique(emissions_data_long$Country), multiple = TRUE),
      p(style = 'color: grey;', 'Here you can select one or several countries you are interested in to compare their Gas Emissions in a specific year.'),
    ),
    
    mainPanel(
      plotOutput("trendPlot"),
      plotOutput("comparisonPlot")
    )
  )
)

server <- function(input, output, session) {
  output$trendPlot <- renderPlot({
    data_to_plot <- filter(emissions_data_long, Country == input$country)
    ggplot(data_to_plot, aes(x = Year, y = Emissions)) +
      geom_line() +
      labs(title = paste("Emissions Over Time for", input$country), 
           x = "Year", 
           y = "Emissions (MtCO2e)") +
      theme_minimal()+
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 10, face = "bold"),
            axis.text.y = element_text(size = 10, face = "bold"),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'))
  })
  
  output$comparisonPlot <- renderPlot({
    data_to_plot <- filter(emissions_data_long, 
                           Year == input$year & Country %in% input$compareCountries)
    ggplot(data_to_plot, aes(x = reorder(Country, Emissions), 
                             y = Emissions, 
                             fill = Country)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Emissions Comparison in", input$year), 
           x = "Country", 
           y = "Emissions (MtCO2e)") +
      theme_minimal()+
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "bold"),
            axis.text.y = element_text(size = 10, face = "bold"),
            plot.title = element_text(size = 20, face = "bold"),
            axis.title.x = element_text(size = 15, face = 'bold'),
            axis.title.y = element_text(size = 15, face = 'bold'))
  })
}

shinyApp(ui = ui, server = server)
