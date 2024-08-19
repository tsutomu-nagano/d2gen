
library(glue)
library(rmarkdown)

template = "
---
title: \"d2gen dummydata report\"
output: html_document
params:
    descriptions: NULL
---

# description

```{r, echo=FALSE}
library(DT)
library(tibble)
library(tidyr)

DT::datatable(
    as_tibble(params$descriptions) %>%
    mutate(across(everything(), ~as.character(.x))) %>%
    pivot_longer(everything()) %>%
    rename(parameter = name)
)

```

# summary

```{r, echo=FALSE}

library(DT)
library(crosstalk)

library(tibble)
library(tidyr)
library(dplyr)

base <- read_delim(params$descriptions$dest, delim = params$descriptions$delim, col_names = FALSE, col_types = cols(.default = \"c\"))

items <- tibble(name = names(base))

codelist <- base %>% mutate(num = dplyr::row_number()) %>%
pivot_longer(-num) %>%
group_by(name,value) %>%
summarise(count = n(), .groups = \"drop_last\") %>%
mutate(rate = count / sum(count)) %>% 
arrange(name, desc(rate)) %>%
ungroup()


shared_df1 <- SharedData$new(items, key = ~name, group = \"reports\")
shared_df2 <- SharedData$new(codelist, key = ~name, group = \"reports\")

dt1 <- DT::datatable(shared_df1, selection = 'single')
dt2 <- DT::datatable(shared_df2) %>%
formatStyle('rate',
              background = styleColorBar(codelist$rate, 'lightblue', -90),
              backgroundSize = '95% 50%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'left')

bscols(dt1, dt2)

```

# data

```{r, echo=FALSE}

library(DT)

base <- read_delim(params$descriptions$dest, delim = params$descriptions$delim, col_names = FALSE, col_types = cols(.default = \"c\"))

DT::datatable(
base,
filter = list(position = 'top', clear = FALSE),
extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = I('colvis'))
)


```


"



create_report <- function(descriptions, dest){


    temp <- tempfile(tmpdir = ".", fileext = ".Rmd")
    write_file(template, temp)
    rmarkdown::render(
        temp,
        output_file = dest,
        params = list(descriptions = descriptions),
        output_options = list(self_contained = TRUE)
        )
    file.remove(temp)

}

