
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param text PARAM_DESCRIPTION
#' @param sentimentType PARAM_DESCRIPTION, Default: 'syuzhet'
#' @param addColor PARAM_DESCRIPTION, Default: FALSE
#' @param nrc PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[syuzhet]{get_sentences}},\code{\link[syuzhet]{get_sentiment}},\code{\link[syuzhet]{get_nrc_sentiment}}
#' @rdname emoDataframeMaker
#' @export
#' @importFrom syuzhet get_sentences get_sentiment get_nrc_sentiment
#' @importFrom dplyr %>% select mutate
emoDataframeMaker <- function(text, sentimentType = "syuzhet", addColor = FALSE, nrc = FALSE){
  ## Error catchers
  if (class(text) != "character") stop("Need a character vector")
  if (length(text) != 1) stop("I only take one text at a time. To feed me many, purrr:map me")
  a <- data_frame(text = syuzhet::get_sentences(text)) %>%
    mutate(sentiment = syuzhet::get_sentiment(text, sentimentType)) %>%
    mutate(cumSentiment = cumsum(sentiment))
  if (addColor) a <- a %>% mutate(color = case_when(sentiment >= 0 ~ "green", TRUE ~ "red"))

  if (nrc) a <- bind_cols(a, syuzhet::get_nrc_sentiment(a$text)) %>%
    mutate(cumAnger = cumsum(anger),
           cumAnticipation = cumsum(anticipation),
           cumDisgust = cumsum(disgust),
           cumFear = cumsum(fear),
           cumJoy = cumsum(joy),
           cumSadness = cumsum(sadness),
           cumSurprise = cumsum(surprise),
           cumTrust = cumsum(trust)
           # ,
           # cumNegative = cumsum(negative),
           # cumPositive = cumsum(positive)
           )
  a
}

## Slope finder ----
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param emoDF PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[broom]{tidy}},\code{\link[broom]{glance}}
#'  \code{\link[rlang]{syms}}
#' @rdname slopeFinder
#' @export
#' @importFrom broom tidy glance
#' @importFrom rlang syms
slopeFinder <- function(emoDF){
  ## linear
  dataToModel <- emoDF %>% pull(cumSentiment) %>% data_frame(y = .) %>% mutate(x = 1:nrow(.))

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


## plot ----
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param listOfEmos PARAM_DESCRIPTION
#' @param titles PARAM_DESCRIPTION, Default: NULL
#' @param color PARAM_DESCRIPTION, Default: FALSE
#' @param showTrends PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname emoMultiPlotter
#' @export
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


## nrc plotter for fun ----
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param listOfEmos PARAM_DESCRIPTION
#' @param titles PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nrcMultiPlotter
#' @export
nrcMultiPlotter <- function(listOfEmos, titles = NULL){
  values <- listOfEmos %>% map(select,contains("cum"))

  par(mfrow = c(4, ceiling(length(values)/4)), mar = c(2.1,2.1,2.1,2.1))

  for (i in seq_along(values)) {
    plot(type = "l", values[[i]]$cumAnger, col = "red", lwd = 2, ylim = c(0,max(values[[i]])))
    lines(type = "l", values[[i]]$cumAnticipation, lwd = 2, col = "grey")
    lines(type = "l", values[[i]]$cumDisgust, lwd = 2, col = "#cccc33")
    lines(type = "l", values[[i]]$cumFear, lwd = 2, col = "yellow") #FFDB58
    lines(type = "l", values[[i]]$cumJoy, lwd = 2, col = "purple")
    lines(type = "l", values[[i]]$cumSadness, lwd = 2, col = "black")
    lines(type = "l", values[[i]]$cumSurprise, lwd = 2, col = "orange")
    lines(type = "l", values[[i]]$cumTrust, lwd = 2, col = "blue")


    ## Add titles or just numbers
    if (is.null(titles)){
      text(x=nrow(values[[i]])/2,y = max(values[[i]]),labels = i)
    } else {
      text(x=nrow(values[[i]])/2,y = max(values[[i]]),labels = titles[i])
    }
  }
  par(mfrow = c(1,1))
}



