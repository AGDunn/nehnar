# Generates a data set for the package, primarily for function testing
# purposes; it has some errors and omissions in it for that reason.

# Load packages. --------------------------------------------------------------
library("nehnar")
# -----------------------------------------------------------------------------

# Convert data into usable form -----------------------------------------------
example_notes <- read_book_notes("data-raw/source_example_notes")
# -----------------------------------------------------------------------------

# Write the tibble version of the data into data/. ----------------------------
usethis::use_data(example_notes, overwrite = TRUE)
# -----------------------------------------------------------------------------
