#### download input ####
# rate = max requests per second

download_aoc_input <- function(year, day, destfile) {
  require(httr2)
  
  if (file.exists(destfile)) {
    message("Already exists: ", destfile)
    return(invisible(destfile))
  }
  
  session <- Sys.getenv("AOC_SESSION")
  if (session == "") {
    stop("AOC_SESSION not set in environment")
  }
  
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  
  paste0("https://adventofcode.com/", year, "/day/", day, "/input") |>
    request() |>
    req_headers(
      Cookie = paste0("session=", session),
      `User-Agent` = "github.com/jonaspeisker/advent-of-code/, 1 req/s, mail@jonaspeisker.de"
    ) |>
    req_throttle(capacity = 1, fill_time_s = 60) |>
    req_error(is_error = function(resp) resp_status(resp) != 200) |>
    req_perform() |>
    resp_body_raw() |>
    writeBin(destfile)
  
  invisible(destfile)
}

#### make templates ####
make_aoc_template <- function(year, day, overwrite = FALSE) {
  destfile <- paste0("R/", year, "/", year, "_day", day, ".R")
    
  if (file.exists(destfile) && !overwrite) {
    message("Already exists: ", destfile)
    return(invisible(destfile))
  }
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  
  fn_name <- paste0("d", day)
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
  year, day,          # URL
  fn_name, day, year, # function
  fn_name, fn_name,   # part 1
  fn_name, fn_name,   # part 2
  fn_name, fn_name    # benchmark
  )

writeLines(template, destfile)
invisible(destfile)
}

message("AoC setup utils loaded ✔")
