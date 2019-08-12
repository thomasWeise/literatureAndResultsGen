library("literatureAndResultsGen");
library("testthat");
context("generate.result.data.frame.functions");

test_that("Test generate.result.data.frame.validate.function", {
  for(is.col.integer in c(TRUE, FALSE)) {
      for(lower.bound in c(NA_integer_, 0L, 10L)) {
        for(upper.bound in c(NA_integer_, 20L, 50L)) {
          cc <- create.stat.columns(title="runtime",
                                    description="bla",
                                    is.col.integer,
                                    lower.bound,
                                    upper.bound);

          d <- generate.result.data.frame.validate.function(cc, "...tmpfunc");
          t <- parse(text=paste(d, sep="\n", collapse="\n"));
          expect_type(t, "expression");
          if(exists("...tmpfunc")) {
            rm("...tmpfunc");
          }
          eval(t);
          expect_true(exists("...tmpfunc"));
          expect_type(...tmpfunc, "closure");
          expect_true(is.function(...tmpfunc));
          rm("...tmpfunc");
        }
    }
  }
})

