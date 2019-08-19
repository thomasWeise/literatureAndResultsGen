library("literatureAndResultsGen");
library("testthat");
context("make.r.doc.references");

test_that("Test making an r bibliographic documentaton", {
  bib.path <- test_path("bibliography.bib");
  expect_true(file.exists(bib.path));

  bib <- read.bibliography(bib.path);

  refs <- make.r.doc.references(bib$ref.id, bib);

  expect_gt(length(refs), nrow(bib));
  expect_true(all(nchar(refs) > 0L));
})

