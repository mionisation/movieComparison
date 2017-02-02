# Comparing reviews of movies
Data wrangling and vis project for trying out the R Shiny framework. Uses the ggvis, dplyr, foreach libraries in R.

`ui.R` - Layouting information

`server.R` - data manipulation, plot creation, handle input and output

`global.R` - global information for both ui and server parts

The data was obtained via imdb and the omdb API, check out the script data/movieAPIconnector.R
	
	All movie releases wide distr: http://www.imdb.com/list/ls057823854/
	2016 movie releases: http://www.imdb.com/list/ls076622175/
	(go to page, ensure you're logged in and click on the bottom (right under 'go to next page'): "Export this list" )

Check the final product out here:
https://mionisation.shinyapps.io/comparing_movie_reviews/
