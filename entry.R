
library(dplyr)
library(stringr)
library(glue)
library(R6)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)
library(argparse)

parser <- ArgumentParser(description='Example of argparse')

# 引数の追加
parser$add_argument("--src", type="character")
parser$add_argument("--dest", type="character")
parser$add_argument("--rec", type="integer", default = 100)
parser$add_argument("--dtype", type="character", default = "variable")
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
dtype <- args$dtype

std <- StandardCodeTable$new(src)

dd <- DummyDataGen$new(std$items, std$codelist)

dd$generate(rec = rec, dest = dest, datatype = dtype)

