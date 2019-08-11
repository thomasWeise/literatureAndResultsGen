library("literatureAndResultsGen");
library("testthat");
context("create.standard.meta.columns");

test_that("Test create.standard.meta.columns", {
  for(are.objective.values.ints in c(TRUE, FALSE)) {
      for(objective.value.lower.bound in c(NA_integer_, 0L, 10L)) {
        for(objective.value.upper.bound in c(NA_integer_, 20L, 50L)) {
          cc <- create.standard.meta.columns(are.objective.values.ints,
                                             objective.value.lower.bound,
                                             objective.value.upper.bound);
          expect_length(cc, 3L);
          expect_identical(names(cc), c("columns", "conditions", "mergers"));
          for(ccc in cc) {
            expect_gt(length(cc), 0L);
          }
        }
    }
  }
})

