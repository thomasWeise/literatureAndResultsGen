library("literatureAndResultsGen");
library("testthat");
context("read.bibliography");

test_that("Test reading a default bibliography", {
  bib.path <- test_path("bibliography.bib");
  expect_true(file.exists(bib.path));

  bib <- read.bibliography(bib.path);
  expect_true(is.data.frame(bib));
  expect_identical(nrow(bib), 177L);
  expect_identical(ncol(bib), 4L);
  expect_identical(colnames(bib), c("ref.id", "ref.type", "ref.as.bibtex", "ref.as.text"));
  expect_type(bib[, 1L], "character");
  expect_type(bib[, 2L], "integer");
  expect_type(bib[, 3L], "character");
  expect_type(bib[, 4L], "character");
  expect_true(is.factor(bib[, 2L]));
  expect_true(all(nchar(bib[, 1L]) > 0L));
  expect_true(all(nchar(bib[, 3L]) > 0L));
  expect_true(all(nchar(bib[, 4L]) > 0L));
  expect_false(any(is.na(bib[, 1L])));
  expect_false(any(is.na(bib[, 2L])));
  expect_false(any(is.na(bib[, 3L])));
  expect_false(any(is.na(bib[, 4L])));
  expect_identical(length(unique(unname(unlist(bib[, 1L])))), 177L);
})

