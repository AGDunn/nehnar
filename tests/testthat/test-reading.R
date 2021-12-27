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
  # one each for start,
  # middle
  # and end line of notes
})

test_that("misplaced page counts make numerical NAs", {
  # pp not at start of a line ~ NA
  # pp not at end of a line ~ NA
})

test_that("items with duplicated page counts make numerical NAs", {
})

test_that("badly formatted page counts make numerical NAs", {
  # capitalisation: "Page count"
  # missing the colon
  # missing the number itself
})
# -----------------------------------------------------------------------------
