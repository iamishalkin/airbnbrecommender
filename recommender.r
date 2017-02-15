#https://muffynomster.wordpress.com/2015/06/07/building-a-movie-recommendation-engine-with-r/
library(recommenderlab)

#1 try with spb data
#create item matrix at first without preprocessing
library(readr)
spbaprt <- read_csv("Saint-Petersburg--Russia.csv")
