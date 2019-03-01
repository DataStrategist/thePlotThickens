
library(tidyverse)
library(thePlotThickens)

a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
stories_all <- read_lines(a)

aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
titles_all <- read_lines(aa)

close(a);close(aa)

## Find end of 28th story

HowMany <- 1000
xx <- stories_all %>% grep(x = ., "<EOS") %>% head(HowMany) %>% tail(1)


stories <- head(stories_all,xx) %>% paste(collapse = "") %>% str_split("<EOS>")
stories <- stories[[1]]
titles <- titles_all %>% head(length(stories))

# dataset <- read_csv(file("http://bit.ly/2uhqjJE?.csv"))
# stories <- dataset$texts
# titles <- dataset %>% unite(name, FirstName, President) %>% pull

## OK get emotional valences for ALL thingies... but test purrr vs furrr
t <- Sys.time()
stories %>% map(emoDataframeMaker, addColor = TRUE)
Sys.time() - t
## ^ 36.4 secs for 1000 entries... let's try furrr

library(furrr)
plan(multiprocess)

t <- Sys.time()
listOfEmos <- stories %>% future_map(emoDataframeMaker, addColor = TRUE)
Sys.time() - t
## ^ 13.8 seconds. Much better. So for 100 000 stories it should take 1380 seconds or 23 minutes


## DO IT!
t <- Sys.time()
listOfEmos <- stories_all %>% future_map(emoDataframeMaker, addColor = TRUE, .progress = TRUE)
Sys.time() - t


listOfEmos <- stories %>% map(emoDataframeMaker, addColor = TRUE)

emoDF <- listOfEmos[[8]]


## Slope
emoDF %>% slopeFinder
slopes <- listOfEmos %>%  map_dfr(slopeFinder)
slopes

## Plot 1
emoDF %>% emoPlotter(showTrends = emoDF %>% slopeFinder, color = TRUE, title = "Test")

## Plot all
emoMultiPlotter(listOfEmos = listOfEmos, showTrends = slopes, titles = titles,  color = F)


nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
