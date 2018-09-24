test_paths <- readr::read_csv("data-raw/test_paths.csv")
save(test_paths, file = "data/test_paths.rda", compress = "xz")
