library(shiny)
library(tidyverse)
library(bslib)
library(readr)

mnist <- read.csv("data/mnist.csv")

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "sketchy"),
  title = "PSTAT 100",
  sidebar = sidebar(
    h3("Perform dimension reduction on an MNIST image, reconstitute the dimension-reduced image"),
    br(),
    sliderInput(inputId = "num_dimensions",
                label = "Number of dimensions:",
                min = 1, max = 28, value = 2),
    p(style = "color: grey;","This slider controls the number of dimensions we first project the image into. Fewer dimensions result in 'grainier' images."),
    numericInput(inputId = "which_row",
                 label = "Which Row?",
                 min = 1, max = 1000, value = 1),
    p(style = "color: grey;","This input controls which row (of the 1000 rows included) to use when generating the image."),
    actionButton(inputId = "goButton", label = "Go")
    ),
  layout_columns(
    card(
      card_header("Reconstituted Image"),
      plotOutput("distPlot")
    ),
    card(
      card_header("True Classification"),
      textOutput("verb")
    )
  )
)

server <- function(input, output) {
  image_gen_ggplot <- function(vect) {
    as.im <- matrix(vect, nrow = 28, byrow = T)
    as.im <- scale(as.im, scale = F)
    
    as.im[nrow(as.im):1, ] %>%
      as.data.frame() %>%
      rowid_to_column(var = 'y') %>%
      pivot_longer(
        -y,
        names_to = 'x',
        values_to = "brightness"
      ) %>%
      mutate(x = parse_number(x)) %>%
      ggplot(aes(x = x, y = y, fill = brightness)) +
      geom_raster() +
      theme_void() +
      scale_fill_gradient2(low = "white", high = "black", guide = "none") +
      theme(
        panel.border = element_rect(linewidth = 1, fill = NA)
      )
  }
  
  output$verb <- renderText({
    paste0("The number displayed is ", mnist[input$which_row, 1])
  })
  
  output$distPlot <- renderPlot({
    req(input$goButton)
    row <- mnist[input$which_row, ]
    vect <- as.numeric(row[-1]) / 255
    X <- matrix(vect, nrow = 28, byrow = T)
    pca <- prcomp(X, center = T, scale. = F)
    n_dims <- min(input$num_dimensions, ncol(pca$x))
    reduced <- pca$x[, 1:n_dims] %*% t(pca$rotation[, 1:n_dims])
    reconstructed <- as.vector(t(reduced))
    image_gen_ggplot(reconstructed)
  })
}  
  

# Run the application
shinyApp(ui = ui, server = server)

