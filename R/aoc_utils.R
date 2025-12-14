#### concatenate file name ####
get_file_name <- function(day, year, example) {
  paste0(
    "../../Nextcloud/aoc_inputs/", year, "/day", day, "/input",
    ifelse(example, "_example", ""), ".txt"
  )
}

#### read input ####
read_input <- function(day, year, example = FALSE) {
  get_file_name(day, year, example) |> 
    readLines()
}

#### read input and split ####
read_split <- function(day, year, example = FALSE, split = "") {
  get_file_name(day, year, example) |> 
    readLines() |> 
    strsplit(split, fixed = TRUE)
}

#### read table as matrix ####
read_table <- function(day, year, example = FALSE, sep = "") {
  get_file_name(day, year, example) |> 
    read.table(header = FALSE, sep) |> 
    as.matrix()
}


message("AoC helpers loaded ✔")
