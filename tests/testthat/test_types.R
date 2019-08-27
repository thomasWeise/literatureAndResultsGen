library("literatureAndResultsGen");
library("testthat");
context("types");

test_that("Test reading a try.convert.numeric.to.int", {

  a <- as.double(c(1,2,3));
  b <- as.integer(a);
  c <- try.convert.numeric.to.int(a, TRUE, TRUE);
  expect_type(c, "integer");
  expect_identical(c, b);
  c <- try.convert.numeric.to.int(a, FALSE, TRUE);
  expect_type(c, "integer");
  expect_identical(c, b);
  c <- try.convert.numeric.to.int(a, TRUE, FALSE);
  expect_type(c, "integer");
  expect_identical(c, b);
  c <- try.convert.numeric.to.int(a, FALSE, FALSE);
  expect_type(c, "integer");
  expect_identical(c, b);

  a <- as.double(c(1,2,3.4));
  b <- as.integer(a);
  expect_error(try.convert.numeric.to.int(a, TRUE, FALSE));

  c <- try.convert.numeric.to.int(a, TRUE, TRUE);
  expect_type(c, "integer");
  expect_identical(c, b);

  c <- try.convert.numeric.to.int(a, FALSE, TRUE);
  expect_type(c, "integer");
  expect_identical(c, b);

  c <- try.convert.numeric.to.int(a, FALSE, FALSE);
  expect_type(c, "double");
  expect_identical(c, a);
})
