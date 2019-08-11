#' @title Create the Standard Column Set
#' @description Create the set of standard columns
#' @param are.objective.values.ints are objective values integers?
#' @param is.time.int are time values integers?
#' @param objective.value.lower.bound a hard lower bound for objective values
#' @param objective.value.upper.bound a hard upper bound for objective values
#' @return the set of standard columns
#' @include names.R
#' @include create_stat_columns.R
#' @export create.standard.columns
#' @include base_conditions.R
#' @include create_column.R
create.standard.columns <- function(are.objective.values.ints=TRUE,
                                    is.time.int=TRUE,
                                    objective.value.lower.bound=0L,
                                    objective.value.upper.bound=NA_integer_) {
  stopifnot(is.logical(are.objective.values.ints),
            length(are.objective.values.ints) == 1L,
            is.logical(is.time.int),
            length(is.time.int) == 1L,
            is.numeric(objective.value.lower.bound),
            length(objective.value.lower.bound) == 1L,
            is.na(objective.value.lower.bound) | is.finite(objective.value.lower.bound),
            (!are.objective.values.ints) | is.integer(objective.value.lower.bound),
            is.numeric(objective.value.upper.bound),
            length(objective.value.upper.bound) == 1L,
            is.na(objective.value.upper.bound) | is.finite(objective.value.upper.bound),
            (!are.objective.values.ints) | is.integer(objective.value.upper.bound));


  n.runs <- create.columns(columns=list(
                 create.column(.col.n.runs,
                               "the total number of repetitions / runs performed",
                               "integer")),
                 conditions=c(.numeric.conditions(.col.n.runs, TRUE),
                              .bound.conditions(.col.n.runs, TRUE, 0L, .max.runs)));

  best.f <- create.stat.columns(
                   title=.col.best.f,
                   description="the best objective value ever reached during the run",
                   is.col.integer=are.objective.values.ints,
                   lower.bound=objective.value.lower.bound,
                   upper.bound=objective.value.upper.bound);

  if(is.time.int) {
    time.bound <- .int.bound;
  } else {
    time.bound <- 1E30;
  }

  last.improvement.time <- create.stat.columns(
    title=.col.last.improvement.time,
    description="the time when the run made the last improvement, i.e., the time after which no further improvement was made, i.e., the time when the best solution was encountered during the run",
    is.col.integer=is.time.int,
    lower.bound=0L,
    upper.bound=time.bound);

  last.improvement.fes <- create.stat.columns(
    title=.col.last.improvement.fes,
    description="the function evaluation (FE) at which the run made the last improvement, i.e., the FE after which no further improvement was made, i.e., the FE when the best solution was created during the run, i.e., the exact number of solutions created until the best solution was found in the run",
    is.col.integer=TRUE,
    lower.bound=0L,
    upper.bound=.int.bound);

  total.time <- create.stat.columns(
    title=.col.total.time,
    description="the total consumed runtime of the run, i.e., the time until the run was terminated",
    is.col.integer=is.time.int,
    lower.bound=0L,
    upper.bound=time.bound);

  total.fes <- create.stat.columns(
    title=.col.total.fes,
    description="the total consumed function evaluations (FEs) of the run, i.e., the total number of candidate solutions generated during the run from the beginning to its end",
    is.col.integer=TRUE,
    lower.bound=0L,
    upper.bound=.int.bound);

  n.reach.best.f.min.runs <- create.columns(
                                  columns=list(create.column(title=.col.n.reach.best.f.min.runs,
                                                        description=paste0("the number of runs that actually discovered the best solution (", .col.best.f, ".", .col.min.name, ")"),
                                                        "integer")),
                                  conditions=c(
                                    .numeric.conditions(.col.n.reach.best.f.min.runs, TRUE),
                                    .bound.conditions(.col.n.reach.best.f.min.runs,
                                                      TRUE, 1L,
                                                      paste0("x$", .col.n.runs)),
                                    .bound.conditions(.col.n.reach.best.f.min.runs, TRUE,
                                                      NA_integer_, .max.runs)));


  reach.best.f.min.time <- create.stat.columns(
    title=.col.reach.best.f.min.time,
    description=paste0("the time needed by the runs which discovered the best solution among all runs (",
                       .col.best.f, ".", .col.min.name,
                       ") until they reached said solution"),
    is.col.integer=is.time.int,
    lower.bound=0L,
    upper.bound=time.bound,
    n.runs.col=.col.n.reach.best.f.min.runs);

  reach.best.f.min.fes <- create.stat.columns(
    title=.col.reach.best.f.min.fes,
    description=paste0("the number of function evaluations (FEs) needed by the runs which discovered the best solution among all runs (",
                       .col.best.f, ".", .col.min.name,
                       ") until they reached said solution"),
    is.col.integer=TRUE,
    lower.bound=0L,
    upper.bound=.int.bound,
    n.runs.col=.col.n.reach.best.f.min.runs);


  budget.fes <- create.columns(columns=list(
                     create.column(.col.budget.fes,
                                   "the maximum number of function evaluations a run was allowed to perform until forceful termination",
                                   "integer")),
                     conditions=c(.numeric.conditions(.col.budget.fes, TRUE),
                                  .bound.conditions(.col.budget.fes, TRUE, 1L)));

  if(is.time.int) { t = "integer"; }
  else { t = "double"; }
  budget.time <- create.columns(columns=list(
                      create.column(.col.budget.time,
                                    "the maximum time granted to a run until forceful termination",
                                    t)),
                      conditions=c(.numeric.conditions(.col.budget.time, is.time.int),
                                   .bound.conditions(.col.budget.time, is.time.int,
                                                     0L, time.bound)));


  # merge
  return(merge.columns(n.runs,
              best.f,
              last.improvement.time,
              last.improvement.fes,
              total.time,
              total.fes,
              n.reach.best.f.min.runs,
              reach.best.f.min.time,
              reach.best.f.min.fes,
              budget.fes,
              budget.time));
}
