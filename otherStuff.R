
library(tidyverse)

# a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
# aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
# b <- read_lines(a)
# titles <- read_lines(aa) %>% head(27)
# close(a)
#
# aa
# bb <- head(b,1000)
#
# bb <- bb %>% paste(collapse = "") %>% str_split("<EOS>")

dataset <- read_csv(file("http://bit.ly/2uhqjJE?.csv"))
bb <- dataset$texts
titles <- dataset %>% unite(name, FirstName, President) %>% pull

## Test
bb[1] %>% emoDataframeMaker

bb %>% head %>% map(emoDataframeMaker, addColor = TRUE)

listOfEmos <- bb %>% map(emoDataframeMaker, addColor = TRUE, nrc = TRUE)
listOfEmos

## Tests
listOfEmos[[2]] %>% slopeFinder
slopes <- listOfEmos %>%  map_dfr(slopeFinder)
slopes

emoMultiPlotter(listOfEmos = listOfEmos, color = T)
emoMultiPlotter(listOfEmos = listOfEmos, color = F)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles, showTrends = slopes)


nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
