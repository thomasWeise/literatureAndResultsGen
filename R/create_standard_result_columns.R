#' @title Create the Standard Result Column Set
#' @description Create the set of standard result (statistic) columns
#' @param are.objective.values.ints are objective values integers?
#' @param is.time.int are time values integers?
#' @param objective.value.lower.bound a hard lower bound for objective values
#' @param objective.value.upper.bound a hard upper bound for objective values
#' @return the set of standard result columns
#' @include names.R
#' @include create_stat_columns.R
#' @export create.standard.result.columns
#' @include base_conditions.R
#' @include create_column.R
#' @include force.R
create.standard.result.columns <- function(are.objective.values.ints=TRUE,
                                    is.time.int=TRUE,
                                    objective.value.lower.bound=0L,
                                    objective.value.upper.bound=NA_integer_) {

  old.options <- options(warn=2);
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

  last.improvement.time <- create.stat.columns(
    title=.col.last.improvement.time,
    description="the time when the run made the last improvement, i.e., the time after which no further improvement was made, i.e., the time when the best solution was encountered during the run",
    is.col.integer=is.time.int,
    lower.bound=0L,
    upper.bound=time.bound);
  last.improvement.time$conditons <- c(last.improvement.time$conditons,
                                       vapply(seq_len(length(last.improvement.time$columns)-1L),
                                              function(i) {
                                                paste0("(!is.na(x$",
                                                       last.improvement.time$columns[[i]]$title,
                                                       ") & (!is.na(x$",
                                                       total.time$columns[[i]]$title,
                                                       ")) & (x$",
                                                       last.improvement.time$columns[[i]]$title,
                                                       " <= x$",
                                                       total.time$columns[[i]]$title,
                                                       "))") }, ""));

  last.improvement.fes <- create.stat.columns(
    title=.col.last.improvement.fes,
    description="the function evaluation (FE) at which the run made the last improvement, i.e., the FE after which no further improvement was made, i.e., the FE when the best solution was created during the run, i.e., the exact number of solutions created until the best solution was found in the run",
    is.col.integer=TRUE,
    lower.bound=0L,
    upper.bound=.int.bound);
  last.improvement.fes$conditons <- c(last.improvement.fes$conditons,
                                       vapply(seq_len(length(last.improvement.fes$columns)-1L),
                                              function(i) {
                                                paste0("(!is.na(x$",
                                                       last.improvement.fes$columns[[i]]$title,
                                                       ") & (!is.na(x$",
                                                       total.fes$columns[[i]]$title,
                                                       ")) & (x$",
                                                       last.improvement.fes$columns[[i]]$title,
                                                       " <= x$",
                                                       total.fes$columns[[i]]$title,
                                                       "))") }, ""));

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
  reach.best.f.min.time$conditons <- c(reach.best.f.min.time$conditons,
                                      vapply(reach.best.f.min.time$columns[-length(reach.best.f.min.time$columns)],
                                             function(cc) {
                                               paste0("(!is.na(x$",
                                                      cc$title,
                                                      ") & (!is.na(x$",
                                                      total.time$columns$max$title,
                                                      ")) & (x$",
                                                      cc$title,
                                                      " <= x$",
                                                      total.time$columns$max$title,
                                                      "))") }, ""),
                                      vapply(reach.best.f.min.time$columns[-length(reach.best.f.min.time$columns)],
                                             function(cc) {
                                               paste0("(!is.na(x$",
                                                      cc$title,
                                                      ") & (!is.na(x$",
                                                      last.improvement.time$columns$max$title,
                                                      ")) & (x$",
                                                      cc$title,
                                                      " <= x$",
                                                      last.improvement.time$columns$max$title,
                                                      "))") }, ""));

  reach.best.f.min.fes <- create.stat.columns(
    title=.col.reach.best.f.min.fes,
    description=paste0("the number of function evaluations (FEs) needed by the runs which discovered the best solution among all runs (",
                       .col.best.f, ".", .col.min.name,
                       ") until they reached said solution"),
    is.col.integer=TRUE,
    lower.bound=0L,
    upper.bound=.int.bound,
    n.runs.col=.col.n.reach.best.f.min.runs);
  reach.best.f.min.fes$conditons <- c(reach.best.f.min.fes$conditons,
                                       vapply(reach.best.f.min.fes$columns[-length(reach.best.f.min.fes$columns)],
                                              function(cc) {
                                                paste0("(!is.na(x$",
                                                       cc$title,
                                                       ") & (!is.na(x$",
                                                       total.fes$columns$max$title,
                                                       ")) & (x$",
                                                       cc$title,
                                                       " <= x$",
                                                       total.fes$columns$max$title,
                                                       "))") }, ""),
                                       vapply(reach.best.f.min.fes$columns[-length(reach.best.f.min.fes$columns)],
                                              function(cc) {
                                                paste0("(!is.na(x$",
                                                       cc$title,
                                                       ") & (!is.na(x$",
                                                       last.improvement.fes$columns$max$title,
                                                       ")) & (x$",
                                                       cc$title,
                                                       " <= x$",
                                                       last.improvement.fes$columns$max$title,
                                                       "))") }, ""));


  budget.fes <- create.columns(columns=list(
                     create.column(.col.budget.fes,
                                   "the maximum number of function evaluations a run was allowed to perform until forceful termination",
                                   "integer")),
                     conditions=c(.numeric.conditions(.col.budget.fes, TRUE),
                                  .bound.conditions(.col.budget.fes, TRUE, 1L),
                                  vapply(last.improvement.fes$columns[-length(last.improvement.fes$columns)],
                                         function(cc) {
                                           paste0("all(is.na(x$", .col.budget.fes, ") | is.na(x$",
                                                  cc$title, ") | (x$", cc$title, " <= x$",
                                                  .col.budget.fes, "))") }, ""),
                                  vapply(reach.best.f.min.fes$columns[-length(reach.best.f.min.fes$columns)],
                                         function(cc) {
                                           paste0("all(is.na(x$", .col.budget.fes, ") | is.na(x$",
                                                  cc$title, ") | (x$", cc$title, " <= x$",
                                                  .col.budget.fes, "))") }, ""),
                                  vapply(total.fes$columns[-length(total.fes$columns)],
                                         function(cc) {
                                           paste0("all(is.na(x$", .col.budget.fes, ") | is.na(x$",
                                                  cc$title, ") | (x$", cc$title, " <= x$",
                                                  .col.budget.fes, "))") }, "")
                                  ),
                     mergers=unlist(c(
                       paste0("if(",
                              "all(is.na(x$", total.fes$columns$min$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$min$title, " == x$", .col.budget.fes,
                              "))) && ",
                              "all(is.na(x$", total.fes$columns$mean$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$mean$title, " == x$", .col.budget.fes,
                              "))) && ",
                              "all(is.na(x$", total.fes$columns$med$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$med$title, " == x$", .col.budget.fes,
                              "))) && ",
                              "all(is.na(x$", total.fes$columns$mode$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$mode$title, " == x$", .col.budget.fes,
                              "))) && ",
                              "all(is.na(x$", total.fes$columns$max$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$max$title, " == x$", .col.budget.fes,
                              "))) && ",
                              "all(is.na(x$", total.fes$columns$sd$title, ") | ((!is.na(x$",
                              .col.budget.fes, ")) & (x$",
                              total.fes$columns$sd$title, "<=", 0,
                              ")))",
                              ") {"),
                       unlist(lapply(list(total.fes$columns$min,
                                       total.fes$columns$mean,
                                       total.fes$columns$med,
                                       total.fes$columns$mode,
                                       total.fes$columns$max),
                                     function(cc) {
                                       t <- cc$title;
                                       c(
                                         paste0("  temp <- is.na(x$", t, ") & (!is.na(x$", .col.budget.fes, "));"),
                                         paste0("  if(any(temp)) {"),
                                         paste0("    changed <- TRUE;"),
                                         paste0("    x$", t, "[temp] <- x$", .col.budget.fes, "[temp];"),
                                         .force("    ", paste0("x$", t), "x"),
                                         paste0("  }")
                                       );
                                     })),
                       "}"
                     )));

  if(is.time.int) { t = "integer"; }
  else { t = "double"; }
  budget.time <- create.columns(columns=list(
                      create.column(.col.budget.time,
                                    "the maximum time granted to a run until forceful termination",
                                    t)),
                      conditions=c(.numeric.conditions(.col.budget.time, is.time.int),
                                   .bound.conditions(.col.budget.time, is.time.int,
                                                     0L, time.bound),
                                   vapply(last.improvement.time$columns[-length(last.improvement.time$columns)],
                                          function(cc) {
                                            paste0("all(is.na(x$", .col.budget.time, ") | is.na(x$",
                                                   cc$title, ") | (x$", cc$title, " <= x$",
                                                   .col.budget.time, "))") }, ""),
                                   vapply(reach.best.f.min.time$columns[-length(reach.best.f.min.time$columns)],
                                          function(cc) {
                                            paste0("all(is.na(x$", .col.budget.time, ") | is.na(x$",
                                                   cc$title, ") | (x$", cc$title, " <= x$",
                                                   .col.budget.time, "))") }, ""),
                                   vapply(total.time$columns[-length(total.time$columns)],
                                          function(cc) {
                                            paste0("all(is.na(x$", .col.budget.time, ") | is.na(x$",
                                                   cc$title, ") | (x$", cc$title, " <= x$",
                                                   .col.budget.time, "))") }, "")),
                      mergers=unlist(c(
                        paste0("if(",
                               "all(is.na(x$", total.time$columns$min$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$min$title, " == x$", .col.budget.time,
                               "))) && ",
                               "all(is.na(x$", total.time$columns$mean$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$mean$title, " == x$", .col.budget.time,
                               "))) && ",
                               "all(is.na(x$", total.time$columns$med$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$med$title, " == x$", .col.budget.time,
                               "))) && ",
                               "all(is.na(x$", total.time$columns$mode$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$mode$title, " == x$", .col.budget.time,
                               "))) && ",
                               "all(is.na(x$", total.time$columns$max$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$max$title, " == x$", .col.budget.time,
                               "))) && ",
                               "all(is.na(x$", total.time$columns$sd$title, ") | ((!is.na(x$",
                               .col.budget.time, ")) & (x$",
                               total.time$columns$sd$title, "<=", 0,
                               ")))",
                               ") {"),
                        unlist(lapply(list(total.time$columns$min,
                                 total.time$columns$mean,
                                 total.time$columns$med,
                                 total.time$columns$mode,
                                 total.time$columns$max),
                        function(cc) {
                          t <- cc$title;
                          c(
                            paste0("  temp <- is.na(x$", t, ") & (!is.na(x$",
                                     .col.budget.time, ")) & is.na(",
                                     .col.budget.fes, ");"),
                            paste0("  if(any(temp)) {"),
                            paste0("    changed <- TRUE;"),
                            paste0("    x$", t, "[temp] <- x$", .col.budget.time, "[temp];"),
                            .force("    ", paste0("x$", t), "x"),
                            paste0("  }")
                          );
                        })),
                        "}",

                        paste0("if(",
                               "all(is.na(x$", total.fes$columns$min$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$min$title, " == x$", .col.budget.fes,
                               "))) && ",
                               "all(is.na(x$", total.fes$columns$mean$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$mean$title, " == x$", .col.budget.fes,
                               "))) && ",
                               "all(is.na(x$", total.fes$columns$med$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$med$title, " == x$", .col.budget.fes,
                               "))) && ",
                               "all(is.na(x$", total.fes$columns$mode$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$mode$title, " == x$", .col.budget.fes,
                               "))) && ",
                               "all(is.na(x$", total.fes$columns$max$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$max$title, " == x$", .col.budget.fes,
                               "))) && ",
                               "all(is.na(x$", total.fes$columns$sd$title, ") | ((!is.na(x$",
                               .col.budget.fes, ")) & (x$",
                               total.fes$columns$sd$title, "<=", 0,
                               ")))",
                               ") {"),
                        unlist(lapply(list(total.fes$columns$min,
                                           total.fes$columns$mean,
                                           total.fes$columns$med,
                                           total.fes$columns$mode,
                                           total.fes$columns$max),
                                      function(cc) {
                                        t <- cc$title;
                                        c(
                                          paste0("  temp <- is.na(x$", t, ") & (!is.na(x$",
                                                 .col.budget.fes, ")) & is.na(",
                                                 .col.budget.time, ");"),
                                          paste0("  if(any(temp)) {"),
                                          paste0("    changed <- TRUE;"),
                                          paste0("    x$", t, "[temp] <- x$", .col.budget.fes, "[temp];"),
                                          .force("    ", paste0("x$", t), "x"),
                                          paste0("  }")
                                        );
                                      })),
                        "}"
                      )));


  # merge
  result <- (join.columns(n.runs,
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
  result <- force(result);
  options(old.options);
  return(result);
}
