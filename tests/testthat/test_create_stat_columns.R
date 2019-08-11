library("literatureAndResultsGen");
library("testthat");
context("create.stat.columns");

test_that("Test create.stat.columns", {
  for(is.col.integer in c(TRUE, FALSE)) {
      for(lower.bound in c(NA_integer_, 0L, 10L)) {
        for(upper.bound in c(NA_integer_, 20L, 50L)) {
          cc <- create.stat.columns(title="runtime",
                                    description="bla",
                                    is.col.integer,
                                    lower.bound,
                                    upper.bound);
          expect_length(cc, 3L);
          expect_identical(names(cc), c("columns", "conditions", "mergers"));
          for(ccc in cc) {
            expect_gt(length(cc), 0L);
          }
          expect_identical(unname(unlist(vapply(cc[[1L]], function(n) n$title, ""))),
                           c("runtime.min", "runtime.mean", "runtime.med",
                             "runtime.mode", "runtime.max", "runtime.sd"));
        }
    }
  }
})

