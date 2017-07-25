library(tidyverse)

in_csv <- "data/Kokodata_20161025_20170724.csv"
out_csv <- "data/Kokodata_20161025_20170724_fixed.csv"

# Read in data
dat <- readr::read_csv(in_csv)

message("Fixing umlauts in ", in_csv, "...")

# Encoding is "unknown-8bit". Manually fix known characters.
dat <- lapply(dat, function(x) {
  x <- gsub("\x8a", "ä", x)
  x <- gsub("\x9a", "ö", x)
  x <- gsub("\x8c", "å", x)
  return(x)
})

dat <- as.data.frame(dat)

message("Done, writing fixed data into ", out_csv, "...")

# Write our data
dat <- readr::write_csv(dat, out_csv)
