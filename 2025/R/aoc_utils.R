#### concatenate file name ####
get_file_name <- function(day, example) {
  paste0(
    "../../Nextcloud/aoc25_inputs/day", day, "/input",
    ifelse(example, "_example", ""), ".txt"
  )
}
