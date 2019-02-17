
library(tidyverse)

a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
stories_all <- read_lines(a)

aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
titles_all <- read_lines(aa)

close(a);close(aa)

xx <- 1000
stories <- head(stories_all,xx) %>% paste(collapse = "") %>% str_split("<EOS>")
stories <- stories[[1]]
titles <- titles %>% head(length(stories))

# dataset <- read_csv(file("http://bit.ly/2uhqjJE?.csv"))
# stories <- dataset$texts
# titles <- dataset %>% unite(name, FirstName, President) %>% pull

## Get Emo valence
listOfEmos <- stories %>% map(emoDataframeMaker, addColor = TRUE)
listOfEmos %>% head(2)

emoDF <- listOfEmos[[8]]


## Slope
emoDF %>% slopeFinder
slopes <- listOfEmos %>%  map_dfr(slopeFinder)
slopes

## Plot 1
emoDF %>% emoPlotter(showTrends = emoDF %>% slopeFinder, color = TRUE, title = "Test")

## Plot all
emoMultiPlotter(listOfEmos = listOfEmos, color = T)
emoMultiPlotter(listOfEmos = listOfEmos, color = F)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
emoMultiPlotter(listOfEmos = listOfEmos, showTrends = slopes, titles = titles,  color = F)


nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
