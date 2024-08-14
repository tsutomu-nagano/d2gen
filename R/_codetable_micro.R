
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


MicroCodeList <- R6Class("microcodelist",
    public = list(

        info = NULL,
        items = NULL,
        initialize = function(src) {

            log_info(glue("codelist {basename(src)} init"))


            cells <- xlsx_cells(src) %>%
                    filter(sheet != "個票データの項目（事項名）")

            infopos <- tribble(
                ~address,   ~name,
                "B1",       "def",
                "B2",       "name",
                "B3",       "description",
                "B4",       "note",
            )


            cells %>%
            filter(data_type != "character") %>%
            print(n = "all")


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


MicroCodeTable <- R6Class("microcodetable",
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
        codelists = NULL,
        region = NULL,
        time_s = NULL,
        time_e = NULL,
        messages = tribble(~id, ~lv, ~msg, "", "","") %>% filter(id != ""),

		initialize = function(src) {

            print(src)
            log_info(glue("dataset {basename(src)} init"))

            self$source <- src 
            self$hash <- file2hash(src)

            self$info <- list(
                    statcode = "B1",
                    statname = "D1",
                    formname = "F1",
                    dataname = "B2",
                    surveydate = "D2",
                    kind = "F2",
                    datatype = "B3",
                    encode = "D3",
                    quote_char = "F3",
                    memo = "B4:D5",
                    create_date = "F4",
                    correct_date = "F5"
                    ) %>%
                    excel2list(src, "個票データの項目（事項名）")

            self$statcode <- self$info$statcode
            is_statcode_offcode <- (statlist %>% filter(statcode == self$statcode) %>% nrow == 0)

            # 実施年から時間軸抽出
            self$info$time_s <- era2y(self$info$surveydate)
            self$info$time_e <- era2y(self$info$surveydate)

            # 時間軸未抽出の場合のメッセージを設定
            if (is.na(self$info$time_s)) {
                private$add_message(
                    lv = "w",
                    msg = c(
                        "実施時期から時間軸を抽出できなかったため時間軸が設定されていないためこのままでは登録できません",
                        "実施時期（D2セル）に値を設定して再作成するか、作成されたデータセットの定義に時間軸を設定してください。"
                        )
                    )
            }

            # 統計コードのオフコードのメッセー時を設定
            if (is_statcode_offcode) {
                private$add_message(
                    lv = "d",
                    msg = c(
                        "政府統計コードがオフコードのためこのままでは登録できません",
                        "政府統計コード（B1セル）を確認して再作成してください。"
                        )
                    )
            }

            renames_base <- tribble(
                ~name.ja,   ~name,
                "カテゴリー", "name1",
                "調査事項・分類事項", "name2",
                "事項名", "name",
                "位置", "pos",
                "バイト数", "length",
                "符号定義コード", "id",
                "備考", "comment",
                "種別", "kind",
                "変数名", "var",
                "対象", "target",
            )

            renames <- renames_base %>% pull(name.ja)
            names(renames) <- renames_base %>% pull(name)

            self$items <- read_excel(src, col_types = "text", skip = 7, sheet = "個票データの項目（事項名）") %>%
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
                        )
            

            self$items <- self$items %>%
                        mutate_if_none(name = "kind", init = "") %>%
                        mutate(type_weight = if_else(kind == "2", "ウエイト", as.character(NA))) %>%
                        mutate(type_dimension = if_else(!is.na(id), "分類事項", as.character(NA))) %>%
                        mutate(metatype = coalesce(type_weight, type_dimension, "その他")) %>%
                        mutate(datatype = if_else(metatype == "その他", "文字列", "")) %>%
                        select(-starts_with("type_"))


            self$info$itemcount <- as.character(nrow(self$items))

            if (self$info$datatype == "固定長"){
                self$info$recordlength <- self$items %>%
                                             pull(length) %>%
                                             str_trim %>%
                                             str_subset("^[0-9]+$") %>%
                                             as.integer %>%
                                             sum %>%
                                             as.character
            } else {
                self$info$recordlength <- ""
            }

            self$codelists <- MicroCodeList$new(src)
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
