
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


if (str_detect(get_ext(src), regex("xlsx", ignore_case = TRUE))){
    std <- StandardCodeTable$new(src)
    dtype <- std$info$datatype
    delim <- std$info$delim
    dd <- DummyDataGen$new(std$items, std$codelist)

}

if (str_detect(get_ext(src), regex("json", ignore_case = TRUE))){
    json <- read_yaml(src)
    dtype <- json$info$datatype
    delim <- json$info$delim

    base <- tibble(json$items) %>% unnest_wider(everything())
    items <- base %>% distinct(id, pos, length) %>% mutate(across(everything(), ~as.character(.x)))
    codelist <- base %>% select(id, code) %>% unnest(code) %>% mutate(across(everything(), ~as.character(.x)))
    dd <- DummyDataGen$new(items, codelist)
}

dd$generate(rec = rec, dest = dest, datatype = dtype, delim = delim, chunk = chunk)

