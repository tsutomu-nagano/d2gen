
library(dplyr)
library(stringr)
library(glue)
library(R6)
library(tidyr)
library(readxl)
library(openxlsx)
library(readr)

c(
    list.files("R/common", full.names = TRUE),
    list.files("R", full.names = TRUE)
) %>%
str_subset("\\.R$") %>%
purrr::map(function(src){
    if (str_detect(basename(src),"^_", negate = TRUE)){
        source(src, encoding = "UTF-8")
    }
})


src <- "test/【オンサイト用】（標準記法）令和2年国調個別データCP_2020_RCD_Kobetsu-kk_B(基本集計).xlsx"
std <- StandardCodeTable$new(src)

dd <- DummyDataGen$new(std$items, std$codelist)

dd$generate(rec = 100, dest = "hoge.csv", datatype = "variable")

