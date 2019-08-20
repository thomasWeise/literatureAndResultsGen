#' @title Create Names and Conditions for Statistics Columns
#' @description We can store several types of statistics from an experiment
#'   measuring one variable. These include minimum, mean, median, mode, and
#'   maximum as well as standard deviation. There are several sanity constraints
#'   imposed on them, e.g., the minimum cannot be bigger than the maximum and if
#'   the standard deviation is 0 they must be the same, etc. Here we try to
#'   generate "column names" and conditions for a given variable.
#' @param title the variable name
#' @param description the variable description
#' @param is.col.integer is the variable/column integer valued?
#' @param n.runs.col the name of the column with the number of repetitions
#' @param lower.bound an optional lower bound
#' @param upper.bound an optional upper bound
#' @return a list with the auto-generated members and code
#' @include names.R
#' @include base_conditions.R
#' @include create_column.R
#' @include force.R
#' @export create.stat.columns
create.stat.columns <- function(title, description,
                                is.col.integer=FALSE,
                                lower.bound=0L,
                                upper.bound=NA_integer_,
                                n.runs.col=.col.n.runs) {
  old.options <- options(warn=2);
  stopifnot(is.character(title),
            length(title) == 1L,
            nchar(title) > 0L,
            is.character(n.runs.col),
            length(n.runs.col) == 1L,
            nchar(n.runs.col) > 0L,
            is.character(description),
            length(description) == 1L,
            nchar(description) > 0L,
            is.logical(is.col.integer),
            !is.na(is.col.integer),
            length(is.col.integer) == 1L,
            length(lower.bound) == 1L,
            length(upper.bound) == 1L);

  title <- trimws(title);
  stopifnot(nchar(title) > 0L);
  description <- trimws(description);
  stopifnot(nchar(description) > 0L);

  # do the minimum
  min.title <- paste0(title, ".", .col.min.name);
  min.desc  <- paste0("the minimum value of ", description);
  mean.title <- paste0(title, ".", .col.mean.name);
  mean.desc  <- paste0("the arithmetic mean of ", description);
  med.title <- paste0(title, ".", .col.med.name);
  med.desc  <- paste0("the median of ", description);
  mode.title <- paste0(title, ".", .col.mode.name);
  mode.desc  <- paste0("the mode, i.e., the most frequent value, of ", description);
  max.title <- paste0(title, ".", .col.max.name);
  max.desc  <- paste0("the maximum value of ", description);
  sd.title <- paste0(title, ".", .col.sd.name);
  sd.desc  <- paste0("the standard deviation of ", description);

  # add conditions
  conditions <- c(
          .numeric.conditions(min.title, is.col.integer),
          .bound.conditions(min.title, is.col.integer, lower.bound, upper.bound),
          .numeric.conditions(mean.title, FALSE),
          .bound.conditions(mean.title, FALSE, lower.bound, upper.bound),
          .numeric.conditions(med.title, FALSE),
          .bound.conditions(med.title, FALSE, lower.bound, upper.bound),
          .numeric.conditions(mode.title, FALSE),
          .bound.conditions(mode.title, FALSE, lower.bound, upper.bound),
          .numeric.conditions(max.title, FALSE),
          .bound.conditions(max.title, FALSE, lower.bound, upper.bound),
          .numeric.conditions(sd.title, FALSE),
          .bound.conditions(sd.title, FALSE, 0L, NA_real_),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                    max.title, ") | (x$", min.title, " <= x$",
                    max.title, "))" ),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                    mean.title, ") | (x$", min.title, " <= x$",
                    mean.title, "))" ),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                     med.title, ") | (x$", min.title, " <= x$",
                     med.title, "))" ),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mode.title, ") | (x$", min.title, " <= x$",
                mode.title, "))" ),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                max.title, ") | (x$", mean.title, " <= x$",
                max.title, "))" ),
         paste0("all(is.na(x$", med.title, ") | is.na(x$",
                max.title, ") | (x$", med.title, " <= x$",
                max.title, "))" ),
         paste0("all(is.na(x$", mode.title, ") | is.na(x$",
                max.title, ") | (x$", mode.title, " <= x$",
                max.title, "))" ),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                max.title, ") | is.na(x$", sd.title,
                ") | xor((x$", sd.title, " <= 0) , (x$",
                min.title, " < x$", max.title, ")))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                min.title, " >= x$", med.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                max.title, " <= x$", med.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mean.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                min.title, " >= x$", mean.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mean.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                max.title, " <= x$", mean.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                min.title, " >= x$", mode.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                max.title, " <= x$", mode.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                mean.title, " == x$", med.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                mean.title, " == x$", mode.title, "))"),
         paste0("all(is.na(x$", mode.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (x$",
                mode.title, " == x$", med.title, "))"),
         paste0("all(is.na(x$", sd.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", sd.title, " <= 0))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                max.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", min.title, " >= x$",
                max.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mean.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", min.title, " >= x$",
                mean.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", min.title, " >= x$",
                med.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", min.title, " >= x$",
                mode.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mean.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", max.title, " <= x$",
                mean.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", max.title, " <= x$",
                med.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", max.title, " <= x$",
                mode.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", mean.title, " == x$",
                med.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", mean.title, " == x$",
                mode.title, "))"),
         paste0("all(is.na(x$", mode.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                n.runs.col, ") | (x$", n.runs.col,
                " > 1L) | (x$", mode.title, " == x$",
                med.title, "))")
         );

  .merge.for.num.1 <- function(from, to) {
    c(paste0("if(any(!is.na(x$", to, "))) {"),
      paste0("  temp <- ((!is.na(x$", n.runs.col, ")) & (x$", n.runs.col,
             " == 1L) & (!is.na(x$", from, ")) & is.na(x$",
             to, "));"),
      "  if(any(temp)) {",
      "    changed <- TRUE;",
      paste0("    x$", to, "[temp] <- x$", from, "[temp];"),
      .force("    ", paste0("x$", to), "x"),
      "  }",
      "}")
  };

  .merge.for.sd.0 <- function(from, to) {
    c(paste0("if(any(!is.na(x$", to, "))) {"),
      paste0("  temp <- ((!is.na(x$", sd.title, ")) & (x$", sd.title,
             " <= 0) & (!is.na(x$", from, ")) & is.na(x$",
             to, "));"),
      "  if(any(temp)) {",
      "    changed <- TRUE;",
      paste0("    x$", to, "[temp] <- x$", from, "[temp];"),
      .force("    ", paste0("x$", to), "x"),
      "  }",
      "}")
  };

  .sd.for.num.1 <- function() {
    c(paste0("if(any(!is.na(x$", sd.title, "))) {"),
      paste0("  temp <- ((!is.na(x$", n.runs.col, ")) & (x$", n.runs.col,
             " == 1L) & is.na(x$", sd.title, "));"),
      "  if(any(temp)) {",
      "    changed <- TRUE;",
      paste0("    x$", sd.title, "[temp] <- 0;"),
      .force("    ", paste0("x$", sd.title), "x"),
      "  }",
      "}")
  };

  .fix.from.min.max <- function(to, value=paste0("x$", min.title, "[temp]")) {
    c(paste0("if(any(!is.na(x$", sd.title, "))) {"),
      paste0("  temp <- ((!is.na(x$", min.title, ")) & (!is.na(x$",
             max.title, ")) & (is.na(x$", to, ")) & (x$",
             max.title, " <= x$", min.title, "));"),
      "  if(any(temp)) {",
      "    changed <- TRUE;",
      paste0("    x$", to, "[temp] <- ", value, ";"),
      .force("    ", paste0("x$", to), "x"),
      "  }",
      "}")
  };

  # data mergers
  mergers <- c(
      .merge.for.num.1(min.title, mean.title),
      .merge.for.num.1(min.title, med.title),
      .merge.for.num.1(min.title, mode.title),
      .merge.for.num.1(min.title, max.title),
      .merge.for.num.1(mean.title, min.title),
      .merge.for.num.1(mean.title, med.title),
      .merge.for.num.1(mean.title, mode.title),
      .merge.for.num.1(mean.title, max.title),
      .merge.for.num.1(med.title, min.title),
      .merge.for.num.1(med.title, mean.title),
      .merge.for.num.1(med.title, mode.title),
      .merge.for.num.1(med.title, max.title),
      .merge.for.num.1(mode.title, min.title),
      .merge.for.num.1(mode.title, mean.title),
      .merge.for.num.1(mode.title, med.title),
      .merge.for.num.1(mode.title, max.title),
      .merge.for.num.1(max.title, min.title),
      .merge.for.num.1(max.title, mean.title),
      .merge.for.num.1(max.title, med.title),
      .merge.for.num.1(max.title, mode.title),
      .merge.for.sd.0(min.title, mean.title),
      .merge.for.sd.0(min.title, med.title),
      .merge.for.sd.0(min.title, mode.title),
      .merge.for.sd.0(min.title, max.title),
      .merge.for.sd.0(mean.title, min.title),
      .merge.for.sd.0(mean.title, med.title),
      .merge.for.sd.0(mean.title, mode.title),
      .merge.for.sd.0(mean.title, max.title),
      .merge.for.sd.0(med.title, min.title),
      .merge.for.sd.0(med.title, mean.title),
      .merge.for.sd.0(med.title, mode.title),
      .merge.for.sd.0(med.title, max.title),
      .merge.for.sd.0(mode.title, min.title),
      .merge.for.sd.0(mode.title, mean.title),
      .merge.for.sd.0(mode.title, med.title),
      .merge.for.sd.0(mode.title, max.title),
      .merge.for.sd.0(max.title, min.title),
      .merge.for.sd.0(max.title, mean.title),
      .merge.for.sd.0(max.title, med.title),
      .merge.for.sd.0(max.title, mode.title),
      .sd.for.num.1(),
      .fix.from.min.max(mean.title),
      .fix.from.min.max(med.title),
      .fix.from.min.max(mode.title),
      .fix.from.min.max(sd.title, value="0")
  );

  # combine
  if(is.col.integer) {
    t = "integer";
  } else {
    t = "double";
  }
  all <- create.columns(
              columns=list(min=create.column(min.title, min.desc, t),
                           mean=create.column(mean.title, mean.desc, "double"),
                           med=create.column(med.title, med.desc, "double"),
                           mode=create.column(mode.title, mode.desc, "double"),
                           max=create.column(max.title, max.desc, t),
                           sd=create.column(sd.title, sd.desc, "double")),
              conditions = conditions,
              mergers = mergers);

  all <- force(all);

  options(old.options);
  return(all);
}
