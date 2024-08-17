
library(dplyr, warn.conflicts=F)
library(stringr)
library(glue)
library(R6)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)
library(argparse)
library(yaml)
library(cli)

parser <- ArgumentParser(description='Example of argparse')

# 引数の追加
parser$add_argument("--src", type="character")
parser$add_argument("--dest", type="character")
parser$add_argument("--rec", type="integer", default = 100)
parser$add_argument("--chunk", type="integer", default = 0)
parser$add_argument("--err", type="double", default = 0)

args <- parser$parse_args()


ss <- c(
    list.files("R/common", full.names = TRUE),
    list.files("R", full.names = TRUE)
) %>%
str_subset("\\.R$") %>%
purrr::map(function(src){
    if (str_detect(basename(src),"^_", negate = TRUE)){
        source(src, encoding = "UTF-8")
    }
})


src <- args$src
dest <- args$dest
rec <- args$rec
chunk <- args$chunk
err_rate <- args$err


dd <- DummyDataGen$new()


if (str_detect(get_ext(src), regex("xlsx", ignore_case = TRUE))){

    dd$setting_from_std(src)

}

if (str_detect(get_ext(src), regex("json", ignore_case = TRUE))){

    dd$setting_from_json(src)

}

dd$generate(rec = rec, dest = dest, chunk = chunk, err_rate = err_rate)

