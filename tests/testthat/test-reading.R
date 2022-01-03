# you can create objects outside of the tests and you can rely on R’s
# copy-on-modify semantics to keep them unchanged between test runs. To clean
# up other actions you can use regular R functions.

# Read the example read notes into the usual format. --------------------------
# -----------------------------------------------------------------------------

# Tests for get_page_count() --------------------------------------------------
test_that("well-ordered page counts are found", {
  # one each for start of notes: Dune
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Dune", "notes"]),
    as.integer(529)
  )
  # for middle: Infinite Jest
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Infinite Jest",
                                 "notes"]),
    as.integer(1079)
  )
  # and end line: Beowulf
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Beowulf",
                                 "notes"]),
    as.integer(136)
  )
})

test_that("multiple spaces don't prevent a page count", {
  # use Knausgaard death/family, which has a few spaces after the colon.
  expect_equal(
    get_page_count(example_notes[example_notes$title == "A Death In The Family",
                                 "notes"]),
    as.integer(490)
  )
})

#test_that("misplaced page counts make numerical NAs", {
#  # pp not at start of a line ~ NA: use K-gaard boyhood island
#  # pp not at end of a line ~ NA: use K-gaard man in love
#})

test_that("missing information makes numerical NAs", {
  # no number: use Murakami birthday stories
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Birthday Stories",
                                 "notes"]),
    as.integer(NA)
  )
  # no page count text or number: owl babies
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Owl Babies",
                                 "notes"]),
    as.integer(NA)
  )
  # no notes at all: use Lovecraft collection
  expect_equal(
    get_page_count(example_notes[example_notes$author == "Lovecraft" &
                                 example_notes$title == "The Complete Fiction", 
                                 "notes"]),
    as.integer(NA)
  )
})

test_that("items with duplicated page counts make numerical NAs", {
  # duplicated page count: use Red Pill
  expect_equal(
    get_page_count(example_notes[example_notes$title == "Red Pill", "notes"]),
    as.integer(NA)
  )
})

# ZZZ add tests for error-hunting mode output, which should probably be forced
# to always produce strings to make the difference obvious.  Could be an
# omnibus test.
# -----------------------------------------------------------------------------
