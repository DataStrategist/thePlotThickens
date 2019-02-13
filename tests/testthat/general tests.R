context("General")
library(testthat)

test_that("emotion dataframer gives out a dataframe",
          expect_equal(object =class(bb[[1]][1] %>% emoDataframeMaker),
                       expected = "data.frame"))


emoMultiPlotter(listOfEmos = listOfEmos, color = T)
emoMultiPlotter(listOfEmos = listOfEmos, color = F)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles)
emoMultiPlotter(listOfEmos = listOfEmos, color = T, titles = titles, showTrends = slopes)
