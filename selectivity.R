library(tidyverse)

selectivity <- function(data, ...){
  data <- data[tidyselect::vars_select(names(data), ...)]
  data <- data[!map_lgl(data, ~all(is.na(.)))]
  is_binary <- function(x)length(unique(na.omit(x)))==2
  as_binary <- function(x){
    if(!is_binary(x))stop("Not coercible to 0/1!")
    x <- x == unique(na.omit(x))[1]
    as.numeric(x)
  }
  data <- mutate_if(data, is_binary, as_binary)
  data <- data[!is.na(apply(data, 1, possibly(sd, NA), na.rm = TRUE)), ]
  sum <- rowSums(data, na.rm = TRUE)
  suppressWarnings(map(data, cor.test, sum)) %>%
    map_df(broom::glance) %>%
    select(conf.low, estimate, conf.high) %>%
    cbind(item = names(data), .)
}

files <- tribble(~file, ~vars,
        "Dia1_SoSe16_Auswertung.xlsx", vars(starts_with("X")),
        "Dia1_WiSe1617_Auswertung_neu.xlsx", vars(starts_with("X")),
        "Dia2_SoSe18.csv", vars(starts_with("vl_"), starts_with("se_"))
        )
files <- mutate(files,
       path = here::here("data", file),
       data = rio::import_list(path),
       selectivity = map2(.data$data, .data$vars, !!~selectivity(.x, !!!.y))
       )

walk2(files$selectivity,
      here::here("selectivity", files$file) %>% paste0(".txt"),
      ~cat(paste(pander::pander_return(.x), collapse = "\n"), file = .y))
