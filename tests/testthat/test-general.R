context("Test-general")

library(testthat)
library(thePlotThickens)
library(tidyverse)

dataset <- read_csv(file("http://bit.ly/2uhqjJE?.csv"))
titles <- dataset %>% unite(name, FirstName, President) %>% pull()
bb <- dataset$texts

context("Testing emoDataframeMaker") ##########################
test_that("emoDataframeMaker gives df", {
  expect_equal(class(emoDataframeMaker(bb[1])), c("tbl_df", "tbl", "data.frame"))
})

test_that("emoDataframeMaker sentiment and cumSentiment work", {
  expect_equal(emoDataframeMaker(bb[1]) %>% pull() %>% sum(), 585.6)
})

test_that("emoDataframeMaker NRC thingie works", {
  expect_equal(ncol(emoDataframeMaker(bb[1], nrc = TRUE)), 21)
})

test_that("emoDataframeMaker NRC thingie gives correct values", {
  expect_equal(emoDataframeMaker(bb[1], nrc = TRUE) %>% pull() %>% sum(), 894)
})

test_that("emoDataframeMaker color works", {
  expect_equal(emoDataframeMaker(bb[1], addColor = TRUE) %>%
    pull() %>%
    table() %>%
    as.data.frame() %>%
    pull(), c(19, 4))
})

# context("Testing slopeFinder") ##########################
# listOfEmos[[2]] %>% slopeFinder
# slopes <- listOfEmos %>%  map_dfr(slopeFinder)
# slopes
#
# emoMultiPlotter(listOfEmos = listOfEmos, color = T)
# emoMultiPlotter(listOfEmos = listOfEmos, color = F)
# emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
# emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles, showTrends = slopes)
#
#
# nrcMultiPlotter(listOfEmos = listOfEmos, titles = titles)
# ######
# test_that("emotion dataframer gives out a dataframe",
#           expect_equal(object =class(bb[[1]][1] %>% emoDataframeMaker),
#                        expected = "data.frame"))
#
#
# emoMultiPlotter(listOfEmos = listOfEmos, color = T)
# emoMultiPlotter(listOfEmos = listOfEmos, color = F)
# emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
# emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles, showTrends = slopes)
