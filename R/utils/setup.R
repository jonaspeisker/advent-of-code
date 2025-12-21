for (y in 2025) {
  for (d in 1:12) {
    download_aoc_input(
      year = y,
      day = d,
      destfile = get_file_name(year = y, day = d, example = FALSE)
    )
    make_aoc_template(year = y, day = d)
  }
}
