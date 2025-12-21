#### concatenate file name ####
get_file_name <- function(day, year, example, example_part = 1) {
  paste0(
    "../../Nextcloud/aoc_inputs/", year, "/day", day, "/input",
    ifelse(example, "_example", ""), 
    ifelse(example & example_part == 2, "2", ""),
    ".txt"
  )
}

#### read input ####
read_input <- function(day, year, example = FALSE, example_part = 1) {
  get_file_name(day, year, example, example_part) |> 
    readLines(warn = FALSE)
}

#### read input and split ####
read_split <- function(day, year, example = FALSE, example_part = 1, split = "") {
  read_input(day, year, example, example_part) |> 
    strsplit(split, fixed = TRUE)
}

#### read table as matrix ####
read_table <- function(day, year, example = FALSE, example_part = 1, sep = "") {
  get_file_name(day, year, example, example_part) |> 
    read.table(header = FALSE, sep) |> 
    as.matrix()
}

message("AoC IO utils loaded ✔")
