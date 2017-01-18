require(RJSONIO)

getOMDBMovieData <- function(movieSrcName) {
  movies <- read.csv(file=movieSrcName, header = TRUE, sep = ',', quote = "\"")
  #create omdb dataframe
  dat <- getMovieFromOMDB(movies[1,2]);
  omdbDF <- as.data.frame(setNames(replicate(35,numeric(0), simplify = F), names(dat)))
  for(i in 1:nrow(movies)) { #nrow(movies)
    omdbDF[i,] <- getMovieFromOMDB(movies[i,2]);
    if(i %% 50 == 0) {
      print(paste("Download progress by ", round(i*100/nrow(movies), digits = 2), "%")  )
    }
    
  }
  return(omdbDF)
}

# make API call with movie from row i
getMovieFromOMDB <- function(movieID) {
  callURL <- getUrl(movieID);
  dat <-fromJSON(callURL);
  return(dat);
}

# build API URL with movie id
getUrl <- function(id) {
  root <- "http://www.omdbapi.com/?"
  u <- paste0(root,"i=", id, "&tomatoes=true")
  return(URLencode(u))
}


widDistrDF = getOMDBMovieData('WIDEDISTR_MOVIES_1972_2016.csv');
write.csv(widDistrDF, "OMDB_WIDEDISTR_MOVIES_1972_2016.csv");
allMovDF = getOMDBMovieData('ALL_MOVIES_2016.csv');
write.csv(allMovDF, "OMDB_ALL_MOVIES_2016.csv")

