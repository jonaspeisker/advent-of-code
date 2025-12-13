library(microbenchmark)

#### concatenate file name ####
get_file_name <- function(day, year, example) {
  paste0(
    "../../Nextcloud/aoc_inputs/", year, "/day", day, "/input",
    ifelse(example, "_example", ""), ".txt"
  )
}
