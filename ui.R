library(ggvis)
library(shiny)

#available movie genres in dataset
movieGenres <- unique(strsplit(paste(moviesAll$Genre,collapse = ", "), ', ')[[1]])[1:21]

# Define UI for application, controls on bottom, scatterplot on top
fluidPage(
  
  # Application title
  titlePanel("Comparisons of Movie Reviews", windowTitle = "ReviewCompare"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column( width = 3,
            selectInput('dataSet', 'Select set of movies', dataSetSelection),
            selectInput('genre', 'Genre', movieGenres, multiple = TRUE, selected = "Action"),
            selectInput('x', 'Horizontal Axis', ratingMeasureSelection),
            selectInput('y', 'Vertical Axis', ratingMeasureSelection, selected = "imdbRating"),
            sliderInput("year", "Release Year:", min = 1970, max = 2016, c(1970, 2016), step = 1),
            
            helpText(""),
            helpText(""),
            helpText(""),
            helpText("Average of the selection:"),
            textOutput("avgMoviesetX"),
            textOutput("avgMoviesetY")
    ),
    #column( width = 1
            #todo: maybe include histograms here,
            #normalize ratings to compare better (include correlation)
            #todo: include z score difference for controvversial titles
            #helpText("Most controversial in this selection:")
    #),
    
    # Show a plot of the generated distribution
    column( width = 6,
            ggvisOutput("moviePlot")
    ),
    position = "right",
    fluid = TRUE
  )
)