library(shiny)
library(ggvis)
library(dplyr)
library(foreach)

# Define server logic required to draw a histogram
function(input, output) {
  #Filter movies depending on inputs
  movies <- reactive({
    #read input from user controls
    fromYear <- input$year[1]
    toYear <- input$year[2]
    xAxisName <- input$x
    yAxisName <- input$y
    dataSet <- input$dataSet
    genre <- input$genre
    #restricting reviewCount
    tomatoMinCount = 5
    imdbMinCount = 100
    
    ## which dataset is selected
    if(dataSet == "OMDB_WIDEDISTR_MOVIES_1972_2016.csv") {
      movieSet = moviesAll
      tomatoMinCount = 10
      imdbMinCount = 500
    } else {
      movieSet = movies2016
    }
    
    ##filter year from user controls and min. review numbers
    movieSet <- movieSet %>% filter(
      Year >= fromYear,
      Year <= toYear,
      tomatoReviews > tomatoMinCount, # don't include movies with too few reviews
      imdbVotes >= imdbMinCount
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
      layer_points(size := 50, size.hover := 200, fillOpacity := 0.4, stroke := 1, strokeOpacity := 0.3, fillOpacity.hover := 0.5, key := ~Title) %>%
      layer_smooths()    %>%    
      add_axis("x", title = xAxisName()) %>%
      add_axis("y", title = yAxisName()) %>%
      add_tooltip(on = "hover", html = function(data){(paste0("<b>", data$Title, "</b><br>", data$Year, "<br>"))}) %>%
      set_options(height = 600, width = 800 )
  })
  bind_shiny(moviePlot, "moviePlot")
}