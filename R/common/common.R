
era2y_base <- tribble(
    ~era,   ~yyyy,
    "令和", 2018,
    "平成", 1988,
    "昭和", 1925,
    "大正", 1911,
    "明治", 1867,
)

era_ptn <- as.character(glue("({str_c(pull(era2y_base, era), collapse = '|')})([0-9０-９]+|元)年"))


excel2list <- function(l, src, sheet){

    df <- read_excel(src, col_types = "list", col_names = FALSE, sheet = sheet, .name_repair = "unique_quiet") %>%
            setNames(1:length(names(.))) %>%
            mutate(row = dplyr::row_number()) %>%
            pivot_longer(cols = -row, names_to = "col") %>%
            mutate(col = as.integer(col))


    purrr::reduce(
        .init = l,
        .x = names(l),
        .f = function(l, name){

            range <- l[[name]]

            if (range != ""){
                m <- str_match(range, "(?<COLS>[A-Z]+)(?<ROWS>[0-9]+)(:(?<COLE>[A-Z]+)(?<ROWE>[0-9]+))?")

                if (!is.na(m[1,1])){
                    cols <- col2int(m[[1, "COLS"]])
                    rows <- as.integer(m[[1, "ROWS"]])

                    cole <- col2int(coalesce(m[[1, "COLE"]], m[[1, "COLS"]]))
                    rowe <- as.integer(coalesce(m[[1, "ROWE"]], m[[1, "ROWS"]]))

                    value <- tidyr::expand_grid(col = cols:cole, row = rows:rowe) %>%
                    left_join(df, by = c("col", "row")) %>%
                    unnest(value) %>%
                    mutate(value = as.character(value)) %>%
                    arrange(row, col) %>%
                    nest(datas = -row) %>%
                    mutate(value = purrr::map(datas, function(datas){
                        datas %>% pull(value) %>% str_c(collapse = "")
                    })) %>%
                    unnest(value) %>%
                    pull(value) %>%
                    str_c(collapse = "\r\n") %>%
                    replace_na(., "")

                } else {

                    value <- range

                }


            } else {
                value <- ""
            }


            l[[name]] <- value

            return(l)
        }

    ) %>%
    return

}

era2y <- function(t){

    if (str_detect(t, era_ptn)){

        mat <- str_match(t, era_ptn)
        era_ <- mat[1,2]
        y <- mat[1,3]

        if (y == "元"){
            y <- "1"
        }
        y <- as.integer(stri_trans_general(y, "Fullwidth-Halfwidth"))

        yyyy <- era2y_base %>% filter(era == era_) %>% pull(yyyy)
        yyyy <- as.integer(yyyy) + y

        return(as.character(yyyy))

    } else {
        return(NA)
    }


}

file2hash <- function(src){

    as.character(openssl::md5(file(src)))

}

mutate_if_none <- function(df, name, init = NA){

    if (any(names(df) != name)){

        df[name] <- init

    }

    return(df)

}

read_csv_text <- function(path, ...){

    read_csv(path, col_types = cols(.default = "c"), ...)

}

get_stem <- function(path){

    return(str_match(basename(path), "(.+)\\..+")[1,2])

}

get_ext <- function(path){

    return(str_match(basename(path), ".+\\.(.+)")[1,2])

}



rnd_char <- function(size, usable_chars){

    size %>%
    purrr::map(function(size){
        str_c(sample(usable_chars, size = size, replace =TRUE), collapse = "")
    }) %>%
    unlist

}


sample_with_err <- function(x, size, is_codelist, usable_chars, err_rate = 0){

    if (is_codelist){

        if (err_rate == 0){
            ret <- sample(x, size, replace)

        } else {

            err_size <- as.integer(size * err_rate)
            size <- size - err_size

            length_ <- str_length(x)
            size_ <- sample(min(length_):max(length_), size = size, replace = TRUE)


            # # print(x)
            # # print(size_)

            # # print(length)
            # # print(size)
            # # print(rep(length, size))
            err_x <- rnd_char(size_, usable_chars)

            # print(x)
            # print(err_x)

            ret <- sample(c(
                    sample(x, size, replace = TRUE),
                    sample(err_x, err_size, replace = TRUE)
                    ))

        }

    } else {
        ret <- x
    }


    return(ret)

}
