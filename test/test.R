library(testthat)
library(dplyr, warn.conflicts=F)
library(stringr)
library(glue)
library(R6)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)
library(yaml)
library(cli)


xx <- list.files("R", full.names = TRUE, recursive = TRUE) %>%
purrr::map(function(src){

    if (!file.info(src)$isdir){
        source(src)
    }

})



test_that("ランダム文字列の作成",{

    set.seed(123)
    size <- 2
    length <- 5
    actual <- rep(length, size) %>% rnd_char(usable_chars = LETTERS)
    expected <- c("OSNCJ","RVKET")

    testthat::expect_equal(expected, actual)
})

test_that("エラー率を設定した場合のコードリスト作成",{

    set.seed(123)
    size <- 2
    length <- 5
    actual <- sample_with_err(c("A","B","C"), 10, is_codelist = TRUE, usable_chars = LETTERS, err_rate = 0.2)
    expected <- c("Z","A","A","A","C","E","A","C","C","B")
    testthat::expect_equal(expected, actual)

})

test_that("ファイル生成",{

    set.seed(123)
    src <- "./test/test.json"
    actual.src <- "./test/test.csv"
    expected.src <- "./test/test.csv"

    dd <- DummyDataGen$new()
    dd$setting_from_json(src)

    rec <- 10
    dd$generate(rec = rec, dest = actual.src)

    expected <- readLines(expected.src)
    actual <- readLines(actual.src)

    testthat::expect_equal(expected, actual)

})




# test_that("test",{

#     testthat::expect_equal(expected, actual)
# })







