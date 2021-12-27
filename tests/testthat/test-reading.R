# you can create objects outside of the tests and you can rely on R’s
# copy-on-modify semantics to keep them unchanged between test runs. To clean
# up other actions you can use regular R functions.

# just to see how tests work using the one from their template.
test_that("multiplication works", {
  # can create something inside test_that() to feed into expect_*()
  testy <- 2 * 2
  expect_equal(testy, 4)
})

# Tests for get_page_count() --------------------------------------------------
test_that("well-ordered page counts are found", {
  # one each for start: Dune
  # middle: Infinite Jest
  # and end line of notes: Beowulf
})

test_that("multiple spaces don't prevent a page count", {
  # use Knausgaard death/family
})

test_that("misplaced page counts make numerical NAs", {
  # pp not at start of a line ~ NA: use K-gaard boyhood island
  # pp not at end of a line ~ NA: use K-gaard man in love
})

test_that("missing information makes numerical NAs", {
  # no number: use Murakami birthday stories
  # no page count text or number: owl babies
  # no notes at all: use Lovecraft collection

test_that("items with duplicated page counts make numerical NAs", {
  # duplicated page count: use Red Pill
  # this one probably wants to have an informative output
})
# -----------------------------------------------------------------------------
