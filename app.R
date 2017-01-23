#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# load movie data
allMovies = read.csv('OMDB_WIDEDISTR_MOVIES_1972_2016.csv')
#recMovies = read.csv('OMDB_ALL_MOVIES_2016.csv')

#available movie genres in dataset
movieGenres <- unique(strsplit(paste(allMovies$Genre,collapse = ", "), ', ')[[1]])[1:21]

# Define UI for application, controls on bottom, scatterplot on top
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparisons of Movie Reviews"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('genre', 'Genre', movieGenres),
      selectInput('x', 'Horizontal Axis', c("Tomatometer", "imdb-Rating", "Metascore")),
      selectInput('x', 'Vertical Axis', c("Tomatometer", "imdb-Rating", "Metascore")),
      sliderInput("year", "Release Year:", min = 1970, max = 2017, c(1970, 2017), step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("moviePlot")
    ),
    position = "right",
    fluid = TRUE
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$moviePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    movies <- allMovies
    #todo: do pre processing of data somewhere else
    #get only rated movies
    movies$imdbRating = as.numeric(as.character(movies$imdbRating))
    movies$tomatoMeter = as.numeric(as.character(movies$tomatoMeter))
    movies <- movies[!is.na(movies$imdbRating) && !is.na(movies$tomatoMeter)]
    #get movies of specific genre
    movies <- movies[grep(input$genre,movies$Genre),]
    
    # draw the histogram with the specified number of bins
    plot(movies$imdbRating, movies$tomatoMeter, main="Scatterplot Example", 
         xlab="imdb rating", ylab="tomatometer ", pch=19)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

