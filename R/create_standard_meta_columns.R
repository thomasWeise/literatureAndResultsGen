#' @title Create the Standard Meta-Data Column Set
#' @description Create the set of standard meta-data columns
#' @param are.objective.values.ints are objective values integers?
#' @param is.time.int are time values integers?
#' @param objective.value.lower.bound a hard lower bound for objective values
#' @param objective.value.upper.bound a hard upper bound for objective values
#' @return the set of standard columns
#' @include names.R
#' @include create_stat_columns.R
#' @export create.standard.meta.columns
#' @include base_conditions.R
#' @include create_column.R
create.standard.meta.columns <- function(are.objective.values.ints=TRUE,
                                         objective.value.lower.bound=0L,
                                         objective.value.upper.bound=NA_integer_) {

  algo.id <- create.column(.col.algo.id,
                           "the ID or short name of the algorithm used to solve the problem instance",
                           "character");
  inst.id <- create.column(.col.inst.id,
                           "the ID of the problem instance solved",
                           "character");
  ref.id <- create.column(.col.ref.id,
                          "the id of the bibliography entry identifying the publication where the result was taken from",
                          "character");
  if(are.objective.values.ints) {
    lbt <- "integer";
  } else {
    lbt <- "double";
  }
  inst.opt.bound.lower <- create.column(.col.inst.opt.bound.lower,
                           "lower bound of the optimal objective value",
                           lbt);



  result <- create.columns(list(algo.id=algo.id,
                                inst.id=inst.id,
                                inst.opt.bound.lower=inst.opt.bound.lower,
                                ref.id=ref.id));
  result <- force(result);
  return(result);
}
