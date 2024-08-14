
DummyDataGen <- R6Class("dummydatagen",
    public = list(

        items = NULL,
        codelist = NULL,

        initialize = function(items, codelist) {

            self$items <- items
            self$codelist <- codelist


        },

        generate = function(rec, dest, datatype = "variable", delim = ",", chunk = 0, err_rate = 0, random_chars = c(letters, LETTERS, as.character(0:9))){


            to_array <- function(x, size){

                range_ptn <- "^([0-9\\.]+)(--|～)([0-9\\.]+)$"

                if (str_detect(x, range_ptn)){
                    m <- str_match(x, range_ptn)

                    s <- m[1,2]
                    e <- m[1,4]

                    array_ <- runif(size, min = as.numeric(s), max = as.numeric(e))

                    if (str_detect(s, "\\.")){

                        dec <- str_length(str_match(s, "\\.([0-9]+)")[1,2])
                        ptn <- glue("[0-9]+\\.[0-9]{{1,{dec}}}")
                        
                        array_ <- as.character(array_) %>%
                                  str_extract(ptn)

                    } else {
                        array_ <- as.character(floor(array_))
                    }

                    return(array_)
                } else {
                    return(x)
                }

            }


            if (datatype == "variable"){
                base <- self$items %>% mutate(pos = id, length = 0)
            } else {
                delim <- ""
                base <- self$items %>% select(pos, length, id)
            }
            codes <- self$codelist %>%
                     select(id, code) %>%
                     mutate(code = str_replace_all(code, "△", " "))


            dummybase <- base %>%
            left_join(codes, by = "id", multiple = "all") %>%
            mutate(pos = as.integer(pos), length = as.integer(length), is_codelist = (id != "")) %>%
            mutate(code = if_else(is_codelist, code, rnd_char(length, random_chars))) %>%
            nest(codes = code)


            if (chunk == 0){

                chunk <- rec
                chunks <- c(rec)

            } else {
                remainder <- rec %% chunk
                quotient <- rec %/% chunk

                chunks <- rep(chunk, quotient)

                if (remainder != 0){
                    chunks <- c(chunks, rec - max(chunks))
                }
            }

            cli_alert_info("rec_total = {rec}, chunk = {chunk}, loops = {length(chunks)}")


            for(index in 1:length(chunks)){

                rec_ <- chunks[index]

                dummybase %>%
                mutate(codes = purrr::pmap(
                    list(codes, is_codelist, length),
                    function(codes, is_codelist, length){
                        codes %>%
                        pull(code) %>%
                        purrr::map2(.x = .,.y = rec_, .f = to_array) %>% unlist %>%
                        sample_with_err(
                            size = rec_,
                            is_codelist = is_codelist,
                            usable_chars = random_chars,
                            err_rate = err_rate) %>%
                        str_pad(width = length)
                })) %>%
                arrange(pos) %>%
                select(pos, codes) %>%
                pivot_wider(names_from = "pos", values_from = "codes") %>%
                unnest(cols = everything()) %>%
                write_delim(dest, delim = delim, col_names = FALSE, append = (index != 1))

                cli_alert_success("loop = {index}, rec = {rec_}")


            }



            # base %>%
            # left_join(codes) %>%
            # mutate(pos = as.integer(pos)) %>%
            # arrange(pos) %>%
            # select(pos, codes) %>%
            # pivot_wider()
            # print



        }
    )
)
