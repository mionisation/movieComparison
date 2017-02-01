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
library(ggvis)
library(foreach)

getCleanedMovieSet <- function(movieSet) {
  #convert numeric columns to num
  
  ##counts of votes
  movieSet$imdbVotes <- as.numeric(as.character(gsub(",","",movieSet$imdbVotes)))
  movieSet$tomatoReviews <- as.numeric(as.character(gsub(",","",movieSet$tomatoReviews)))
  
  ##ratings
  movieSet$imdbRating <- as.numeric(as.character(movieSet$imdbRating))
  movieSet$tomatoRating <- as.numeric(as.character(movieSet$tomatoRating))
  movieSet$Metascore <- as.numeric(as.character(movieSet$Metascore))
  movieSet$tomatoMeter <- as.numeric(as.character(movieSet$tomatoMeter))
  movieSet$tomatoFresh <- as.numeric(as.character(movieSet$tomatoFresh))
  movieSet$tomatoRotten <- as.numeric(as.character(movieSet$tomatoRotten))
  
  ##other
  movieSet$Year <- as.numeric(as.character(movieSet$Year))
  
  movieSet
}
# load movie data
moviesAll = getCleanedMovieSet(read.csv('data/OMDB_WIDEDISTR_MOVIES_1972_2016.csv', stringsAsFactors = F))
movies2016 = getCleanedMovieSet(read.csv('data/OMDB_ALL_MOVIES_2016.csv', stringsAsFactors = F))

#Prepare data structures for layouting
dataSetSelection = c("OMDB_WIDEDISTR_MOVIES_1972_2016.csv","OMDB_ALL_MOVIES_2016.csv")
names(dataSetSelection) = c("Most popular movies from 1972 to 2016", "All movie releases 2016")
ratingMeasureSelection = c("tomatoMeter", "imdbRating", "metaScore")
names(ratingMeasureSelection) = c("Tomatometer", "imdb Rating", "Metascore")
#recMovies = read.csv('OMDB_ALL_MOVIES_2016.csv')

#available movie genres in dataset
movieGenres <- unique(strsplit(paste(moviesAll$Genre,collapse = ", "), ', ')[[1]])[1:21]

# Define UI for application, controls on bottom, scatterplot on top
ui <- fluidPage(
  
  # Application title
  titlePanel("Comparisons of Movie Reviews", windowTitle = "ReviewCompare"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column( width = 2,
      selectInput('dataSet', 'Select set of movies', dataSetSelection),
      selectInput('genre', 'Genre', movieGenres, multiple = TRUE, selected = "Action"),
      selectInput('x', 'Horizontal Axis', ratingMeasureSelection),
      selectInput('y', 'Vertical Axis', ratingMeasureSelection, selected = "imdbRating"),
      sliderInput("year", "Release Year:", min = 1970, max = 2016, c(1970, 2016), step = 1)
    ),
    column( width = 2,
            helpText("Average of the selection:"),
            #todo: maybe include histograms here,
            #normalize ratings to compare better (include correlation)
            #
            textOutput("avgMoviesetX"),
            textOutput("avgMoviesetY"),
            helpText("Most controversial in this selection:")
    ),
    
    # Show a plot of the generated distribution
    column( width = 6,
      ggvisOutput("moviePlot")
    ),

    position = "right",
    fluid = TRUE
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  ratingMeasureSelection = c("tomatoMeter", "imdbRating", "metaScore")
  names(ratingMeasureSelection) = c("Tomatometer", "imdb Rating", "Metascore")
  #Filter movies depending on inputs
  movies <- reactive({
    #read input from user controls
    fromYear <- input$year[1]
    toYear <- input$year[2]
    xAxisName <- input$x
    yAxisName <- input$y
    dataSet <- input$dataSet
    genre <- input$genre
    
    ## which dataset is selected
    if(dataSet == "OMDB_WIDEDISTR_MOVIES_1972_2016.csv") {
      movieSet = moviesAll
    } else {
      movieSet = movies2016
    }

    ##filter year from user controls and min. review numbers
    movieSet <- movieSet %>% filter(
      Year >= fromYear,
      Year <= toYear,
      tomatoReviews > 5, # don't include movies with too few reviews
      imdbVotes >= 500
    )
    

    ##filter out NAs of selected axis
    movieSet$xAxis = setAxis(movieSet, xAxisName)
    movieSet$yAxis = setAxis(movieSet, yAxisName)
    movieSet <- as.data.frame(movieSet)
    movieSet <- movieSet[!is.na(movieSet$xAxis) & !is.na(movieSet$yAxis),]
    ## filter the movies according to specified genre
    if(!is.null(genre)){
      foreach(n=unlist(strsplit(genre, '\t', fixed = TRUE))) %do% {
        movieSet <- movieSet[grep(n,movieSet$Genre),]
      }
    }
    movieSet
  })
  
  setAxis <- function(set, axisName) {
    switch(axisName, tomatoMeter = set$tomatoMeter, tomatoRating = set$tomatoRating, imdbRating = set$imdbRating, metaScore = set$Metascore )
  }
  
  output$avgMoviesetX <- renderText(expr = paste(xAxisName(), round(mean(movies()$xAxis), digits = 2)))
  
  output$avgMoviesetY <- renderText(expr = paste(yAxisName(), round(mean(movies()$yAxis), digits = 2)))
 
  xAxisName <- reactive({
    names(ratingMeasureSelection[ratingMeasureSelection == input$x])
  })
  
  yAxisName <- reactive({
    names(ratingMeasureSelection[ratingMeasureSelection == input$y])
  })
 
  moviePlot <- reactive({
    m = movies()
    # todo:
    # make two panels with the same controls, display results in the same figure ?
    # split up server and ui parts
    m %>% ggvis(~xAxis, ~yAxis, fill = ~Year) %>% 
      layer_points(size := 50, size.hover := 200, fillOpacity := 0.2, stroke := 1, fillOpacity.hover := 0.5, key := ~Title) %>%
      layer_smooths()    %>%    
      add_axis("x", title = xAxisName()) %>%
      add_axis("y", title = yAxisName()) %>%
      add_tooltip(on = "hover", html = function(data){(paste0("<b>", data$Title, "</b><br>", data$Year, "<br>"))}) %>%
      set_options(height = 600, width = 800 )
  })
  bind_shiny(moviePlot, "moviePlot")
}

# Run the application 
shinyApp(ui = ui, server = server)

