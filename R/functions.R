
#' @title EMO dataframe maker
#' @description Given input text outputs a dataframe of sentiment scores.
#' @param text Input text, typically a long-form document
#' @param sentimentType Type of sentiment analysis - an argument to syuzhet::get_sentiment, Default: 'syuzhet'
#' @param addColor Add a column with a colour code to indicate sentiment, Default: FALSE
#' @param nrc Add 8 columns with intensity scores for each of the emotions in the NRC emotion lexicon, Default: FALSE
#' @return A dataframe where each row is a sentence with columns for the original text, sentiment score of the sentinence and cumulative sentiment score of the volume.
#' @details Calculates sentence-by-sentence and cumulative sentiment scores for a given document.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[syuzhet]{get_sentences}},\code{\link[syuzhet]{get_sentiment}},\code{\link[syuzhet]{get_nrc_sentiment}}
#' @rdname emoDataframeMaker
#' @export
#' @importFrom syuzhet get_sentences get_sentiment get_nrc_sentiment
#' @importFrom dplyr %>% select mutate
emoDataframeMaker <- function(text, sentimentType = "syuzhet", addColor = FALSE, nrc = FALSE) {
  ## Error catchers
  if (class(text) != "character") stop("Need a character vector")
  if (length(text) != 1) stop("I only take one text at a time. To feed me many, purrr:map me")
  a <- data_frame(text = syuzhet::get_sentences(text)) %>%
    mutate(sentiment = syuzhet::get_sentiment(text, sentimentType)) %>%
    mutate(cumSentiment = cumsum(sentiment))
  if (addColor) a <- a %>% mutate(color = case_when(sentiment >= 0 ~ "green", TRUE ~ "red"))

  if (nrc) {
    a <- bind_cols(a, syuzhet::get_nrc_sentiment(a$text)) %>%
      mutate(
        cumAnger = cumsum(anger),
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
  }
  a
}

## Slope finder ----
#' @title Slope finder
#' @description Reports the fit of alternative emotional arcs to a document and the best fitting canonical story type.
#' @param emoDF A document in the form of an EMO dataframe
#' @return Outputs a tibble of one row reporting the best fitting story type.
#' @details The output tibble contains estimates of the linear slope and estimate, R2 for the linear and sinusoidal fit, the highest R2 achieved and the most appropriate category/story type
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[broom]{tidy}},\code{\link[broom]{glance}}
#'  \code{\link[rlang]{syms}}
#' @rdname slopeFinder
#' @export
#' @importFrom broom tidy glance
#' @importFrom rlang syms
slopeFinder <- function(emoDF) {
  ## linear ----------------------------------
  dataToModel <- emoDF %>% pull(cumSentiment) %>% data_frame(y = .) %>%
    mutate(x = 2*pi/nrow(emoDF) * 1:nrow(emoDF))

  modelLinear <- dataToModel %>% lm(y ~ x, data = .)

  slope <- modelLinear %>% broom::tidy() %>% pull("estimate")
  rs <- modelLinear %>% broom::glance() %>% pull("sigma")

  results <- data_frame(slopeLM = slope[2], interceptLM = slope[1], sigma_LM = rs)

  ## boy meets girl (sin wave) -----------------------
  ## Thanks https://stats.stackexchange.com/questions/60994/fit-a-sinusoidal-term-to-data
  y <- dataToModel$y
  t <- dataToModel$x

  ## Assume start is halfway thru the curve for intercept and A goes up the rest of the way
  res <- safely(nls)(y ~ A * sin(t) + C, data = dataToModel,
    start = list(A = max(y) - mean(y), C = mean(y)))
  res <- res$result
  co <- coef(res)

  results$sigma_SIN <- broom::glance(res) %>% pull(sigma)
  results <- bind_cols(results, co %>% data.frame %>% t %>% as.data.frame)



  ## EVALUATE BEST ----------------------------
  results <- results %>% mutate(mak = pmin(!!!rlang::syms(
    grep(pattern = "sigma", names(results), value = TRUE)
  )))

  results <- results %>%
    mutate(category = case_when(
      sigma_LM == mak & slopeLM > 0 ~ "1.rags->riches",
      sigma_LM == mak & slopeLM < 0 ~ "2.riches->rags",
      sigma_SIN == mak ~ "3.boy meets girl"
    )) %>%
    separate(category, c("catNum", "category"), sep = "\\.") %>%
    mutate(catNum = as.numeric(catNum))

  ## output
  results
}


## plot ----

#' @title emoPlotter
#' @description Takes one EMO dataframe and outputs a faceted plot of the emotional arcs
#' @param emoDF One EMO dataframes
#' @param title Optional string providing title for the plot. Default: NULL
#' @param color Show colours to indicate sentiment - requires EMO dataframe with colour data, Default: FALSE
#' @param showTrends Optional input from slopeFinder, containing the best-fitting regression information, Default: NULL
#' @return Returns a plot object
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname emoPlotter
#' @export
emoPlotter <- function(emoDF, showTrends = NULL, title = NULL, color = FALSE) {
# browser()
  plot(x, y, main = title,
       xlab = "", ylab = "Emotional Valence",
       xlim = c(0, 2*pi), ylim = c(min(emoDF$cumSentiment), max(emoDF$cumSentiment)))

  palette <- RColorBrewer::brewer.pal(8, "Pastel1")
  ggbg <- function(x) {
    points(0, 0, pch = 16, cex = 1e6, col = x,
           xlim = c(0,2*pi))
    grid(col = "white", lty = 1)
  }

  ## Add trends or not
  linWt = 1; sinWt = 1

  if (!is.null(showTrends)) {
    if (showTrends$catNum == 1) { ## LINEAR
      bg <- palette[3]; linWt = 2
    } else if (showTrends$catNum == 2) { ## LINEAR2
      bg <- palette[1]; linWt = 2
    } else if (showTrends$catNum == 3) { ## SIN
      bg <- palette[2]; sinWt = 2
    }

    abline(a = showTrends$interceptLM, b = showTrends$slopeLM,
           lwd = linWt, col = "purple", panel.first = ggbg(bg))
    #lm(y ~ sin(2 * pi / per * t) + cos(2 * pi / per * t))
    # curve(showTrends$`sin(2 * pi/per * t)`*sin(2 * pi / showTrends$perSIN*x) +
    #         showTrends$`cos(2 * pi/per * t)`*cos(2*pi/showTrends$perSIN*x), add = TRUE)

    # lines(fitted(reslm)~t,col=4,lty=2) ## This wont work because I can't pass the reslm in a dataframe.

    curve(showTrends$A * sin(x) + showTrends$C, lwd = sinWt,
          col = "blue", add = TRUE) ## This is the wrong stuff
  }

  ## Main plot
  values <- emoDF$cumSentiment

  if (color) {
    colorpoints <- emoDF$color
    if (is.null(unlist(colorpoints))) {
      rm(colorpoints)
      color <- FALSE
      warning("Color requires an EMO dataframe with color data")
    }
    ## A note on x. in order to fit sin wave correctly, want x to go exactly 2pi wide regardless of input
    points(x = 2*pi/length(emoDF$cumSentiment) * 1:length(emoDF$cumSentiment), y = emoDF$cumSentiment, type = "b", col = colorpoints, lwd = 2)
  } else {
    points(x = 2*pi/length(emoDF$cumSentiment) * 1:length(emoDF$cumSentiment), y = emoDF$cumSentiment, type = "l")
  }
}

#' @title EMO multiplotter
#' @description Takes a list of EMO  dataframes and outputs a faceted plot of the emotional arcs
#' @param listOfEmos List of EMO dataframes
#' @param titles Show the document titles, Default: NULL
#' @param color Show colours to indicate sentiment - requires EMO dataframe with colour data, Default: FALSE
#' @param showTrends PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname emoMultiPlotter
#' @export
emoMultiPlotter <- function(listOfEmos, showTrends = NULL, titles = NULL, color = FALSE) {
# browser()
  par(mfrow = c(4, ceiling(length(listOfEmos) / 4)), mar = c(2.1, 2.1, 2.1, 2.1))
  pmap(list(listOfEmos,
            split(showTrends, seq(nrow(showTrends))),
            as.list(titles)), .f = emoPlotter, color = color)

  par(mfrow = c(1, 1))
}


## nrc plotter for fun ----
#' @title NRC plotter
#' @description Given a list of EMO dataframes, plots the cumulative score for each emotion in Saif Mohammed's NRC emotion lexicon
#' @param listOfEmos A list of EMO dataframes
#' @param titles Display the title of the document in the plot, Default: NULL
#' @return A faceted plot of each document and its cumulative NRC scores
#' @details Shows cumulative scores for anger, fear, anticipation, trust, surprise, sadness, joy, and disgust
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname nrcMultiPlotter
#' @export
nrcMultiPlotter <- function(listOfEmos, titles = NULL) {
  values <- listOfEmos %>% map(select, contains("cum"))

  par(mfrow = c(4, ceiling(length(values) / 4)), mar = c(2.1, 2.1, 2.1, 2.1))

  for (i in seq_along(values)) {
    plot(type = "l", values[[i]]$cumAnger, col = "red", lwd = 2, ylim = c(0, max(values[[i]])))
    lines(type = "l", values[[i]]$cumAnticipation, lwd = 2, col = "grey")
    lines(type = "l", values[[i]]$cumDisgust, lwd = 2, col = "#cccc33")
    lines(type = "l", values[[i]]$cumFear, lwd = 2, col = "yellow") # FFDB58
    lines(type = "l", values[[i]]$cumJoy, lwd = 2, col = "purple")
    lines(type = "l", values[[i]]$cumSadness, lwd = 2, col = "black")
    lines(type = "l", values[[i]]$cumSurprise, lwd = 2, col = "orange")
    lines(type = "l", values[[i]]$cumTrust, lwd = 2, col = "blue")


    ## Add titles or just numbers
    if (is.null(titles)) {
      text(x = nrow(values[[i]]) / 2, y = max(values[[i]]), labels = i)
    } else {
      text(x = nrow(values[[i]]) / 2, y = max(values[[i]]), labels = titles[i])
    }
  }
  par(mfrow = c(1, 1))
}
