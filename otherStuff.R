
library(tidyverse)

a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
b <- read_lines(a)
titles <- read_lines(aa) %>% head(27)
close(a)

aa
bb <- head(b,1000)

bb <- bb %>% paste(collapse = "") %>% str_split("<EOS>")


## Test
bb[[1]][1] %>% emoDataframeMaker

bb[[1]] %>% head %>% map(emoDataframeMaker, addColor = TRUE)

listOfEmos <- bb[[1]] %>% map(emoDataframeMaker, addColor = TRUE, nrc = TRUE)

listOfEmos[[3]] -> emoDF


## Tests
listOfEmos[[2]] %>% slopeFinder
slopes <- listOfEmos %>%  map_dfr(slopeFinder)


nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
