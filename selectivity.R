library(tidyverse)

selectivity <- function(data, ...){
  # select variables via tidyselect variables
  # see ?dplyr::select
  data <- data[tidyselect::vars_select(names(data), ...)]
  # remove NA only coumns
  data <- data[!map_lgl(data, ~all(is.na(.)))]
  # remove variables without variance
  data <- data[!is.na(apply(data, 1, possibly(sd, NA), na.rm = TRUE)), ]
  # calc the sum of all items
  sum <- rowSums(data, na.rm = TRUE)
  # use cor.test to get confidence intervals, use broom to extract results
  suppressWarnings(map(data, cor.test, sum)) %>%
    map_df(broom::glance) %>%
    select(conf.low, estimate, conf.high) %>%
    cbind(item = names(data), .)
}
# create a data.frame, with filename and column selectors
# colums selection is handled inside vars, to provide the flexibility of dplyr::select
files <- tribble(~file, ~vars,
        "Dia1_SoSe16_Auswertung.xlsx", vars(starts_with("X")),
        "Dia1_WiSe1617_Auswertung_neu.xlsx", vars(starts_with("X")),
        "Dia2_SoSe18.csv", vars(starts_with("vl_"), starts_with("se_"))
        )

# calc selectivity
files <- mutate(files,
       # retrive full path to data dir
       path = here::here("data", file),
       # import via rio, so format doesn't matter
       # (and its vectorized to import many files at once)
       data = rio::import_list(path),
       # calc selectivity
       # vars is spliced via bang bang bang, into selectivity
       # formular is unquoted to force splicing in map instead of mutate
       selectivity = map2(.data$data, .data$vars, !!~selectivity(.x, !!!.y))
       )

# write results of selectivity into text files (via cat)
# names are coressponding to the original filenames (via here)
# for visibility they are formated by to markdown tables (via pander)

walk2(files$selectivity,
      here::here("selectivity", files$file) %>% paste0(".txt"),
      ~cat(paste(pander::pander_return(.x), collapse = "\n"), file = .y))
