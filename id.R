#library(tidyverse)
gen_item_id <- function(item, start = 1, end = 5, sep = "\\s"){
  if(length(item)!=1)stop("Only one Item at the time.")
  item <- stringr::str_squish(item)
  words <- stringr::str_to_lower(stringr::word(item, start = start, end = end, sep = sep)) # first 5 words of item
  chr <- stringr::str_remove_all(words, "[^[:alnum:]]")
  id <- digest::digest(chr, algo=c("md5"), serialize = FALSE)
  return(id)
}
debug(gen_item_id)
gen_item_id("Das ist ein Test hier.")
gen_item_id("Das ist ein Test hier....")
gen_item_id("Das    ist    ein    Test    hier.")
