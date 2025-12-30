#### download input ####
# rate = max requests per second

download_aoc_input <- function(year, day) {
  require(httr2)
  session <- Sys.getenv("AOC_SESSION")
  if (session == "") {
    stop("AOC_SESSION not set in environment")
  }
  
  for (y in year) {
    for (d in day) {
      destfile <- get_file_name(year = y, day = d, example = FALSE)
      
      if (file.exists(destfile)) {
        message("Already exists: ", destfile)
        next
      }
      
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      
      paste0("https://adventofcode.com/", year, "/day/", day, "/input") |>
        request() |>
        req_headers(
          Cookie = paste0("session=", session),
          `User-Agent` = "github.com/jonaspeisker/advent-of-code/, mail@jonaspeisker.de"
        ) |>
        req_throttle(capacity = 1, fill_time_s = 60) |>
        req_error(is_error = function(resp) resp_status(resp) != 200) |>
        req_perform() |>
        resp_body_raw() |>
        writeBin(destfile)
    }
  }
}

#### make templates ####
make_aoc_template <- function(year, day, overwrite = FALSE) {
  for (y in year) {
    for (d in day) {
      destfile <- paste0("R/", y, "/", y, "_day", d, ".R")
      
      if (file.exists(destfile) && !overwrite) {
        message("Already exists: ", destfile)
        next
      }
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      
      fn_name <- paste0("d", d)
      template <- sprintf(
'#### https://adventofcode.com/%d/day/%d ####

%s <- function(day = %d, year = %d, example = TRUE, part = 1) {
  verbose <- example
  input <- read_input(day, year, example)


  if (part == 1) {
  
    return()
  }
  
  if (part == 2) {
  
    return()
  }
}

# part 1
%s(example = TRUE,  part = 1)
%s(example = FALSE, part = 1)

# part 2
%s(example = TRUE,  part = 2)
%s(example = FALSE, part = 2)

# benchmark
microbenchmark::microbenchmark(
  %s(example = FALSE, part = 1), #
  %s(example = FALSE, part = 2)  #
)
', 
      y, d,          # URL
      fn_name, d, y, # function
      fn_name, fn_name,   # part 1
      fn_name, fn_name,   # part 2
      fn_name, fn_name    # benchmark
      )

      writeLines(template, destfile)
    }
  }
}

message("AoC setup utils loaded ✔")
