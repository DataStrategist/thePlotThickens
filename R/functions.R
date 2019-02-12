library(tidyverse)

a <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/plots")
aa <- file("C:/Users/Amit/Dropbox/Data/movie data/plots/titles")
b <- read_lines(a)
titles <- read_lines(aa) %>% head(27)
close(a)

aa
bb <- head(b,1000)

bb <- bb %>% paste(collapse = "") %>% str_split("<EOS>")

emoDataframeMaker <- function(text, sentimentType = "syuzhet", addColor = FALSE){
  ## Error catchers
  if (class(text) != "character") stop("Need a character vector")
  if (length(text) != 1) stop("I only take one text at a time. To feed me many, purrr:map me")
  a <- data_frame(text = syuzhet::get_sentences(text)) %>%
    mutate(sentiment = syuzhet::get_sentiment(text, sentimentType)) %>%
    mutate(cumSentiment = cumsum(sentiment))
  if (addColor) a <- a %>% mutate(color = case_when(cumSentiment >= 0 ~ "green", TRUE ~ "red"))
  a
}

bb[[1]][1] %>% emoDataframeMaker -> aa

emoDF <- bb[[1]] %>% map(emoDataframeMaker, addColor = TRUE)

## Slope finder ----
slopeFinder <- function(data){
  ## linear
  dataToModel <- data %>% pull(cumSentiment) %>% data_frame(y = .) %>% mutate(x = 1:nrow(.))

  modelLinear <- dataToModel %>% lm(y ~ x, data = .)

  slope <- modelLinear %>% broom::tidy() %>% pull("estimate")
  rs <- modelLinear %>%  broom::glance() %>% pull("r.squared")

  results <- data_frame(slopeLM = slope[2], interceptLM = slope[1], rsLM = rs)

  ## boy meets girl (sin wave)
  ## Thanks https://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
  y <- dataToModel$y
  t <- dataToModel$x
  res <- safely(nls)(y ~ A*sin(omega*t+phi)+C, data = data.frame(t,y),
                     start = list(A = 1, omega = 1, phi = 1, C = 1))
  res <- res$result
  co <- coef(res)

  fit <- function(x, a, b, c, d) {a*sin(b*x + c) + d}
  ssp <- spectrum(y, plot = FALSE)
  per <- 1/ssp$freq[ssp$spec == max(ssp$spec)]
  reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
  results$rsSIN <- broom::glance(reslm) %>% pull(r.squared)
  # rg <- diff(range(y)) # to plot
  # plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
  # lines(fitted(reslm)~t,col=4,lty=2)


  ## getting class, first get max and then identify where it is
  results <- results %>% mutate(mak=pmax(!!!rlang::syms(
    grep(pattern = "rs", names(results), value = TRUE))))

  results <- results %>% mutate(category = case_when(
    rsLM  == mak & slopeLM > 0 ~ '1.rags->riches',
    rsLM  == mak & slopeLM < 0 ~ '2.riches->rags',
    rsSIN == mak               ~ '3.boy meets girl'
  )) %>% separate(category, c("catNum", "category"), sep = "\\.") %>%
    mutate(catNum = as.numeric(catNum))

  ## output
  results
}

## Tests
emoDF[[2]] %>% slopeFinder
slopes <- emoDF %>%  map_dfr(slopeFinder)

## plot ----
emoMultiPlotter <- function(listOfEmos, titles = NULL, color = FALSE, showTrends = NULL){
  values <- listOfEmos %>% map("cumSentiment")
  if (color) colorpoints <- listOfEmos %>% map("color")

  par(mfrow = c(4,ceiling(length(values)/4)), mar=c(2.1,2.1,2.1,2.1))

  for (i in seq_along(values)) {
    if (color) {
      plot(type = "b", values[[i]], col = colorpoints[[i]])
    } else {
      plot(values[[i]], type = "l")
    }
    ## Add titles or just numbers
    if (is.null(titles)){
      text(x=length(values[[i]])/2,y = max(values[[i]]),labels = i)
    } else {
      text(x=length(values[[i]])/2,y = max(values[[i]]),labels = titles[i])
    }
    # browser()
    ## Add trends or not
    if (!is.null(showTrends)) {
      if (showTrends$catNum[i] <= 2) abline(lsfit(x = seq_along(values[[i]]), y = values[[i]]),
                                            col = "purple")
    }
  }
  par(mfrow = c(1,1))
}

## Tests
emoMultiPlotter(listOfEmos = emoDF, color = T)
emoMultiPlotter(listOfEmos = emoDF, color = F)
emoMultiPlotter(listOfEmos = emoDF, color = T, titles = titles)
emoMultiPlotter(listOfEmos = emoDF, color = T, titles = titles, showTrends = slopes)

