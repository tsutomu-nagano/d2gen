
get_ext <- function(path){

    return(str_match(basename(path), ".+\\.(.+)")[1,2])

}


read_csv_text <- function(path, ...){

    if (get_ext(path) == "csv"){
        df <- read_csv(path, col_types = cols(.default = "c"), ...)
    }

    if (get_ext(path) == "parquet"){
        df <- read_parquet(path)
    }

    return(df)

}


# statlist <- read_csv_text("data/common/statlist.csv")


StandardCodeList <- R6Class("standardcodelist",
    public = list(

        info = NULL,
        items = NULL,
        initialize = function(src) {




            cells <- xlsx_cells(src) %>%
                    filter(sheet != "個票データの項目（事項名）")

            infopos <- tribble(
                ~address,   ~name,
                "B1",       "def",
                "B2",       "name",
                "B3",       "description",
                "B4",       "note",
            )


            # cells %>%
            # filter(data_type != "character") %>%
            # print(n = "all")


            self$info <- cells %>%
            inner_join(infopos, by = "address", multiple = "all") %>%
            select(sheet, name, character)


            itempos <- tribble(
                ~col,   ~name,
                1,      "code",
                2,      "content",
                3,      "note",
            )


            self$items <- cells %>%
            filter(row >= 7, col %in% 1:3) %>%
            inner_join(itempos, by = "col", multiple = "all") %>%
            select(sheet, row, name, character) %>%
            rename(value = character) %>%
            pivot_wider(id_cols = c(sheet, row)) %>%
            arrange(sheet,row) %>%
            select(-row)



        }
    )
)


StandardCodeTable <- R6Class("standardcodetable",
    private = list(

        add_message = function(lv, msg){
            self$messages <- bind_rows(
                self$messages,
                tibble(id = uuid::UUIDgenerate(), lv = lv, msg = str_c(msg, collapse = "\n"))
            )
        }

    ),
    public = list(
        
        statcode = NULL,
        hash = NULL,
        datasetname = NULL,
        info = NULL,
        items = NULL,
        source = NULL,
        codelist = NULL,
        region = NULL,
        time_s = NULL,
        time_e = NULL,
        messages = tribble(~id, ~lv, ~msg, "", "","") %>% filter(id != ""),

		initialize = function(src, sheetname = "符号表") {

            self$info <- list(
                    statcode = "B1",
                    statname = "B2",
                    formname = "B3",
                    filename = "K2",
                    surveydate = "F1",
                    kind = "F2",
                    encode = "B4",
                    quote_char = "B6",
                    create_date = "K1",
                    correct_date = "N1"
                    ) %>%
                    excel2list(src, sheetname)


            df <- read_excel(src, skip = 7, col_type = "text")

            if (any(names(df) == "位置")){
                self$info$datatype <- "fixed"
                self$info$delim <- ""
                key <- "pos"
            } else {
                self$info$datatype <- "variable"
                self$info$delim <- ","
                key <- "itemno"
            }

            renames_base <- tribble(
                ~name.ja,   ~name,
                "位置", "pos",
                "バイト数", "length",
                "項目番号", "itemno",
                "符号", "code",
                "符号内容", "content",
            )

            renames <- renames_base %>% pull(name.ja)
            names(renames) <- renames_base %>% pull(name)

            base <- df %>%
                        setNames(str_replace_all(names(.), "\r\n","")) %>%
                        purrr::reduce2(
                            .init = .,
                            .x = renames_base %>% pull(name.ja),
                            .y = renames_base %>% pull(name),
                            .f = function(df, name.ja, name){

                                ns <- names(df)
                                
                                if (any(ns == name.ja)){
                                    ns <- str_replace(ns, glue("^{name.ja}$"), name)
                                    df <- df %>% setNames(ns)
                                }

                                return(df)
                            }
                        ) %>%
                        select(one_of(renames_base$name)) %>%
                        mutate(key = !!as.name(key)) %>%
                        mutate(code = if_else(!is.na(key),replace_na(code, "") , code)) %>%
                        filter(!is.na(content)) %>%
                        fill(-c(code, content)) %>%
                        select(-key) %>%
                        mutate(code = replace_na(code, "")) %>%
                        rename(id = itemno) %>%
                        mutate(num = dplyr::row_number())
            

            refs <- base %>% filter(code == "外部参照")

            if (nrow(refs) >= 1){
                ref_codelist <- refs %>%
                filter(code == "外部参照") %>%
                rowwise() %>%
                mutate(codelist = purrr::map(content, function(content){
                    
                    refname <- str_match(content, ".+\\.(.+)$")[1,2]

                    read_excel(src, sheet = refname, col_names = FALSE) %>%
                    setNames(c("code", "content")) %>%
                    return

                })) %>%
                select(-c(code, content)) %>%
                unnest(codelist)

                base <- bind_rows(
                    base %>% anti_join(refs %>% distinct(num), by = "num"),
                    ref_codelist
                ) %>%
                arrange(num)

            }

           
            self$items <- base %>% distinct(id)

            self$codelist <- base
            # self$items <- self$items %>%
            #             mutate_if_none(name = "kind", init = "") %>%
            #             mutate(type_weight = if_else(kind == "2", "ウエイト", as.character(NA))) %>%
            #             mutate(type_dimension = if_else(!is.na(id), "分類事項", as.character(NA))) %>%
            #             mutate(metatype = coalesce(type_weight, type_dimension, "その他")) %>%
            #             mutate(datatype = if_else(metatype == "その他", "文字列", "")) %>%
            #             select(-starts_with("type_"))


            # self$info$itemcount <- as.character(nrow(self$items))

            # if (self$info$datatype == "固定長"){
            #     self$info$recordlength <- self$items %>%
            #                                  pull(length) %>%
            #                                  str_trim %>%
            #                                  str_subset("^[0-9]+$") %>%
            #                                  as.integer %>%
            #                                  sum %>%
            #                                  as.character
            # } else {
            #     self$info$recordlength <- ""
            # }

            # self$codelists <- MicroCodeList$new(src)
            # ids <- self$items %>% filter(!is.na(id)) %>% pull(id) %>% unique
            # prog_id <- cli_progress_bar("二次メタ様式から事項の情報を読み取り中", total = length(ids), type = "tasks")
            # self$codelists <- purrr::map(.x = ids, .f = function(id){
            #                     cl <- MicroCodeList$new(src, id)
            #                     cli_progress_update(id = prog_id)
            #                     cl$items %>% return
            #                   }) %>%
            #                   bind_rows
            # cli_progress_done(id = prog_id)

        },

        export_mreg_template = function(dest_dir){


            ds <- self$info %>%
                    as_tibble %>%
                    mutate(
                        num = self$hash,
                        region = "全国"
                    ) %>%
                    unite(col = datasetname, statname, formname, sep = "_", remove = FALSE, na.rm = TRUE) %>%
                    DataSetList_Template$new(
                        statcode = self$statcode,
                        src = "R/data/template/datasetlist.xlsx",
                        dest = glue("{dest_dir}/{get_stem(self$source)}_dataset.xlsx"),
                        items = .
                        )

            ds$saveBook()


            var <- Variable_Template$new(
                            statcode = self$statcode,
                            src = "R/data/template/variable.xlsx",
                            dest = glue("{dest_dir}/{get_stem(self$source)}_variable.xlsx"),
                            items = self$items,
                            codelist = self$codelists,
                            measure = NULL,
                            num = self$hash
                            )

            var$saveBook()


        }

    )

)
