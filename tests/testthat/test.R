

library(testthat)
library(dplyr, warn.conflicts=F)
library(stringr)
library(stringi)
library(glue)
library(R6)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)
library(yaml)
library(cli)
library(logger)
library(tidyxl)

test.dir <- here::here("tests")

r_files <- list.files(here::here("R") , full.names = TRUE, recursive = TRUE)
purrr::walk(r_files, ~ if (!file.info(.x)$isdir) source(.x))


# --- グローバルカウンタ ---
test_counter <- 0
test_id <- ""
next_test_id <- function(label) {
  test_counter <<- test_counter + 1
  test_id <<- as.character(glue("test_{test_counter}"))
  paste0(test_id, "_", label)
}

# --- 前処理 ---
setup({
    test_root <- tempfile("test_data_")
    dir.create(test_root)
    message("テスト用ルート作成: ", test_root)
    assign("test_root", test_root, envir = .GlobalEnv)  # グローバルに明示的に格納
})

# --- 後処理 ---
teardown({
  if (dir.exists(test_root)) {
    message("テスト用フォルダ削除: ", test_root)
    unlink(test_root, recursive = TRUE, force = TRUE)
  }
})


# --- テストごとに実行する前処理
setup_test <- function() {

    subtest_dir <- glue("{test_root}/{test_id}")
    dir.create(subtest_dir)
    message("テストごとの前処理実行")
    return(subtest_dir)

}


# --- テスト ---
test_that(next_test_id("ランダム文字列の作成"),{

    setup_test()

    set.seed(123)
    size <- 2
    length <- 5
    actual <- rep(length, size) %>% rnd_char(usable_chars = LETTERS)
    expected <- c("OSNCJ","RVKET")

    testthat::expect_equal(expected, actual)

})

test_that(next_test_id("エラー率を設定した場合のコードリスト作成"),{

    subtest_dir <- setup_test()

    set.seed(123)
    size <- 2
    length <- 5
    actual <- sample_with_err(c("A","B","C"), 10, is_codelist = TRUE, usable_chars = LETTERS, err_rate = 0.2)
    expected <- c("Z","A","A","A","C","E","A","C","C","B")
    testthat::expect_equal(expected, actual)

})

test_that(next_test_id("jsonファイルの定義でtestファイル生成（固定長）"),{

    subtest_dir <- setup_test()

    setting.src <- list(
        info = list(
            datatype = "fixed",
            delim = "\t"
        ),
        items = list(
            list(id = 1, pos = 1, length = 2, code = c("C2","CP","C6")),
            list(id = 2, pos = 3, length = 1, code = c("A","J","K")),
            list(id = 3, pos = 4, length = 6, code = c("202010"))
        )
    )

    expected <- tribble(
        ~X1,    ~X2,    ~X3,
        "C6",	"K",	"202010",
        "C2",	"J",	"202010",
        "C6",	"A",	"202010",
        "C6",	"J",	"202010",
        "C2",	"K",    "202010",
        "C2",	"J",	"202010",
        "C2",	"A",	"202010",
        "C2",	"K",	"202010",
        "C6",	"K",	"202010",
        "CP",	"A",    "202010",
    ) %>%
    unite(X1, everything(), sep = "")


    set.seed(123)
    setting <- glue("{subtest_dir}/setting.json")
    actual <- glue("{subtest_dir}/actual.csv")

    # 設定を JSON に書き込む
    jsonlite::write_json(setting.src, setting, auto_unbox = TRUE, pretty = TRUE)

    dd <- DummyDataGen$new()
    dd$setting_from_json(setting)

    rec <- nrow(expected)
    dd$generate(rec = rec, dest = actual)

    actual <- read_csv(actual, col_names = FALSE)

    expected <- expected %>% pull(X1)
    actual <- actual %>% pull(X1)

    testthat::expect_equal(expected,actual)

})


test_that(next_test_id("JSONファイルの定義でtestファイル生成（CSV）"),{

    subtest_dir <- setup_test()

    setting.src <- list(
        info = list(
            datatype = "variable",
            delim = ","
        ),
        items = list(
            list(id = 1, pos = 1, length = 2, code = c("C2","CP","C6")),
            list(id = 2, pos = 3, length = 1, code = c("A","J","K")),
            list(id = 3, pos = 4, length = 6, code = c("202010"))
        )
    )

    expected <- tribble(
        ~X1,    ~X2,    ~X3,
        "C6",	"J",	"202010",
        "C6",	"J",	"202010",
        "C6",	"A",	"202010",
        "CP",	"J",	"202010",
        "C6",	"K",    "202010",
        "CP",	"A",	"202010",
        "CP",	"K",	"202010",
        "CP",	"K",	"202010",
        "C6",	"A",	"202010",
        "C2",	"A",    "202010",
    ) %>%
    unite(X1, everything(), sep = ",")


    set.seed(123)
    setting <- glue("{subtest_dir}/setting.json")
    actual <- glue("{subtest_dir}/actual.csv")

    # 設定を JSON に書き込む
    jsonlite::write_json(setting.src, setting, auto_unbox = TRUE, pretty = TRUE)

    dd <- DummyDataGen$new()
    dd$setting_from_json(setting)

    rec <- nrow(expected)
    dd$generate(rec = rec, dest = actual)



    actual <- read_csv(actual, col_names = FALSE) %>%
              unite(X1, everything(), sep = ",")

    expected <- expected %>% pull(X1)
    actual <- actual %>% pull(X1)

    testthat::expect_equal(expected,actual)

})


test_that(next_test_id("EXCELファイルの定義（標準記法）の読み込み"),{

    subtest_dir <- setup_test()

    expected <- list(
        info = list(
            datatype = "variable",
            delim = ","
        ),
        items = list(
            list(id = 1, pos = 1, code = c("C2","CP","C6")),
            list(id = 2, pos = 2, code = c("A","J","K")),
            list(id = 3, pos = 3, code = c("202010"))
        )
    )


    src <- testthat::test_path("testdata/std.xlsx")

    std <- StandardCodeTable$new(src)

    items <- std$items %>%
    left_join(std$codelist %>% select(itemno, code), by = "itemno", multiple = "all") %>%
    nest(datas = -itemno) %>%
    mutate(items = purrr::pmap(
        list(itemno, datas),
        function(itemno, datas){

            return(
                list(
                    id = as.integer(itemno),
                    pos = as.integer(itemno),
                    code = datas %>% pull(code)
                )
            )


        })
    ) %>%
    select(items) %>%
    as.list %>%
    .$items

    actual <- list(
        info = list(
            datatype = std$info$datatype,
            delim = std$info$delim
        ),
        items = items
    )

    testthat::expect_equal(expected,actual)

})

test_that(next_test_id("EXCELファイルの定義（二次メタ）の読み込み"),{

    subtest_dir <- setup_test()

    expected <- list(
        info = list(
            datatype = "variable",
            delim = ","
        ),
        items = list(
            list(id = 1, pos = 1, code = c("C2","CP","C6")),
            list(id = 2, pos = 2, code = c("A","J","K")),
            list(id = 3, pos = 3, code = c("202010")),
            list(id = 4, pos = 4, code = c("202010")),
            list(id = 5, pos = 5, code = as.character(NA)),
            list(id = 6, pos = 6, code = as.character(NA))
        )
    )


    src <- testthat::test_path("testdata/micro.xlsx")

    micro <- MicroCodeTable$new(src)


    items <- micro$items %>%
    left_join(micro$codelist %>% select(id, code), by = "id", multiple = "all") %>%
    nest(datas = -pos) %>%
    mutate(items = purrr::pmap(
        list(pos, datas),
        function(pos, datas){


            return(
                list(
                    id = as.integer(pos),
                    pos = as.integer(pos),
                    code = datas %>% pull(code)
                )
            )


        })
    ) %>%
    select(items) %>%
    as.list %>%
    .$items



    actual <- list(
        info = list(
            datatype = micro$info$datatype,
            delim = micro$info$delim
        ),
        items = items
    )

    testthat::expect_equal(expected,actual)

})
