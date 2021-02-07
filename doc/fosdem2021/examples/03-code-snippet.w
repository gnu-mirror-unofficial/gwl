process stats (with file)
  packages
    . "r"
    . "r-genomation"
  inputs
    string-append file "-big.gff"
  outputs
    string-append file "-small.gff"
  # R {
    library(genomation)
    gff <- gffToGRanges("{{inputs}}", split.group=TRUE)
    head(gff)
  }