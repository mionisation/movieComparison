#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

getCleanedMovieSet <- function(movieSet) {
  #convert to int, handle NAs
  movieSet
}
# load movie data
moviesAll = getCleanedMovieSet(read.csv('data/OMDB_WIDEDISTR_MOVIES_1972_2016.csv', stringsAsFactors = F))
#allMovies$Year <- as.numeric(as.character(allMovies$Year))



#Prepare data structures for layouting
dataSetSelection = c("OMDB_WIDEDISTR_MOVIES_1972_2016.csv","OMDB_ALL_MOVIES_2016.csv")
names(dataSetSelection) = c("Most popular movies from 1972 to 2016", "All movie releases 2016")

#recMovies = read.csv('OMDB_ALL_MOVIES_2016.csv')

#available movie genres in dataset
movieGenres <- unique(strsplit(paste(moviesAll$Genre,collapse = ", "), ', ')[[1]])[1:21]

# Define UI for application, controls on bottom, scatterplot on top
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparisons of Movie Reviews"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('dataSet', 'Select set of movies', dataSetSelection),
      selectInput('genre', 'Genre', movieGenres, multiple = TRUE, selected = "Action"),
      selectInput('x', 'Horizontal Axis', c("tomatoMeter","tomatoRating", "imdbRating", "metaScore")),
      selectInput('y', 'Vertical Axis', c("tomatoMeter","tomatoRating", "imdbRating", "metaScore"), selected = "imdbRating"),
      sliderInput("year", "Release Year:", min = 1970, max = 2016, c(1970, 2016), step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("moviePlot", height = 600)
    ),
    position = "right",
    fluid = TRUE
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #Filter movies depending on inputs
  movies <- reactive({
    fromYear <- input$year[1]
    toYear <- input$year[2]
    xAxis <- input$x
    yAxis <- input$y
    dataSet <- input$dataSet
    #array or one value?
    genre <- input$genre
    
    if(dataSet == "OMDB_WIDEDISTR_MOVIES_1972_2016.csv") {
      movieSet = moviesAll
    } else {
      movieSet = movies2016
    }

    movieSet <- movieSet %>% filter(
      Year >= fromYear,
      Year <= toYear,
      tomatoReviews > 1000000000000000,
      imdbVotes >= 1000000000000000000
    )
    
    movieSet$xAxis = setAxis(movieSet, xAxis)
    movieSet$yAxis = setAxis(movieSet, yAxis)
    
    movieSet <- as.data.frame(movieSet)
  })
  
  setAxis <- function(set, axisName) {
    switch(axisName, tomatoMeter = set$tomatoMeter, tomatoRating = set$tomatoRating, imdbRating = set$imdbRating, metaScore = set$Metascore )
  }
  
  output$moviePlot <- renderPlot({
    
    #todo: do pre processing of data somewhere else ; use filter like in https://github.com/rstudio/shiny-examples/blob/master/051-movie-explorer/server.R
    #get only rated movies
    #movies$imdbRating = as.numeric(as.character(movies$imdbRating))
    #movies$tomatoMeter = as.numeric(as.character(movies$tomatoMeter))
    #movies <- movies[!is.na(movies$imdbRating) && !is.na(movies$tomatoMeter)]
    #get movies of specific genre
    #movies <- movies[grep(input$genre,movies$Genre),]
    #s movies <- movies[as.numeric(levels(movies$Year))[movies$Year] >= input$year[1] && as.numeric(levels(movies$Year))[movies$Year] <= input$year[2]]
    #movies <- movies[movies$Year >= input$year[1] && movies$Year <= input$year[2]]
    # draw the histogram with the specified number of bins
    m = movies()
    plot(m$xAxis, m$yAxis, main="Scatterplot Example", 
         xlab=names(m$xAxis), ylab=names(m$yAxis), pch=19, col="#00000033")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

