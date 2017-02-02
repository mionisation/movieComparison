
#Prepare data structures for layouting
dataSetSelection <- c("OMDB_WIDEDISTR_MOVIES_1972_2016.csv","OMDB_ALL_MOVIES_2016.csv")
names(dataSetSelection) = c("Most popular movies 1972-2016", "All movie releases 2016")
ratingMeasureSelection = c("tomatoMeter", "imdbRating", "metaScore")
names(ratingMeasureSelection) = c("Tomatometer", "imdb Rating", "Metascore")

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