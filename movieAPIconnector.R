require(RJSONIO)

getOMDBMovieData <- function(movieSrcName) {
  movie <- read.csv(file=movieSrcName, header = TRUE, sep = ',', quote = "\"")
  #create omdb dataframe
  dat <- getMovieFromOMDB(1);
  omdbDF <- as.data.frame(setNames(replicate(35,numeric(0), simplify = F), names(dat)))
  for(i in 1:10) { #nrow(movie)
    omdbDF[i,] <- getMovieFromOMDB(i);
  }
  return(omdbDF)
}

getMovieFromOMDB <- function(i) {
  callURL <- getUrl(movie[i,2]);
  dat <-fromJSON(callURL);
  return(dat);
}

getUrl <- function(id) {
  root <- "http://www.omdbapi.com/?"
  u <- paste0(root,"i=", id, "&tomatoes=true")
  return(URLencode(u))
}


widDistrDF = getOMDBMovieData('WIDEDISTR_MOVIES_1972_2016.csv')
getOMDBMovieData('ALL_MOVIES_2016.csv')