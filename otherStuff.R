
library(tidyverse)

a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
b <- read_lines(a)
titles <- read_lines(aa) %>% head(52)
close(a)

aa
bb <- head(b,2000)

bb <- bb %>% paste(collapse = "") %>% str_split("<EOS>")
bb <- bb[[1]]

# dataset <- read_csv(file("http://bit.ly/2uhqjJE?.csv"))
# bb <- dataset$texts
# titles <- dataset %>% unite(name, FirstName, President) %>% pull

## Test
listOfEmos <- bb %>% map(emoDataframeMaker, addColor = TRUE, nrc = TRUE)
listOfEmos

emoDF <- listOfEmos[[8]]


## Tests
emoDF %>% slopeFinder
slopes <- listOfEmos %>%  map_dfr(slopeFinder)
slopes

emoDF <- listOfEmos[[6]]
emoDF %>% emoPlotter(showTrends = emoDF %>% slopeFinder, color = TRUE, title = "Test")


emoMultiPlotter(listOfEmos = listOfEmos, color = T)
emoMultiPlotter(listOfEmos = listOfEmos, color = F)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
emoMultiPlotter(listOfEmos = listOfEmos, showTrends = slopes, titles = titles,  color = T )


nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
