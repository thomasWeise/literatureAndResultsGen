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
#' @param number.column the name of the column with the number of repetitions
#' @param lower.bound an optional lower bound
#' @param upper.bound an optional upper bound
#' @return a list with the auto-generated members and code
#' @export create.stat.columns
create.stat.columns <- function(title, description,
                                number.column="n.runs",
                                is.col.integer=FALSE,
                                lower.bound=0L,
                                upper.bound=NA_integer_) {
  stopifnot(is.character(title),
            length(title) == 1L,
            nchar(title) > 0L,
            is.character(number.column),
            length(number.column) == 1L,
            nchar(number.column) > 0L,
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

  # validity conditions
  conditions.not.for.sd <- character(0);
  conditions.for.all <- "all(is.na(x$\uffff) | is.finite(x$\uffff))";
  conditions.for.int <- character(0);
  if(is.col.integer) {
    conditions.for.int <- c(conditions.for.int,
                            "is.integer(x$\uffff)");
  }

  # add lower bound condition
  if(!is.na(lower.bound)) {
    stopifnot(is.finite(lower.bound));
    if(is.col.integer) {
      stopifnot(is.integer(lower.bound));
      conditions.not.for.sd <- c(conditions.not.for.sd,
                                 paste0("all(is.na(x$\uffff) | (x$\uffff >= ", lower.bound, "L))"));
    } else {
      stopifnot(is.numeric(lower.bound));
      conditions.not.for.sd <- c(conditions.not.for.sd,
                                 paste0("all(is.na(x$\uffff) | (x$\uffff >= ", lower.bound, "))"));
    }
  }

  # add upper bound condition
  if(!is.na(upper.bound)) {
    if(!is.na(lower.bound)) {
      stopifnot(upper.bound >= lower.bound);
    }
    stopifnot(is.finite(upper.bound));
    if(is.col.integer) {
      stopifnot(is.integer(upper.bound));
      conditions.not.for.sd <- c(conditions.not.for.sd,
                                 paste0("all(is.na(x$\uffff) | (x$\uffff <= ", upper.bound, "L))"));
    } else {
      stopifnot(is.numeric(upper.bound));
      conditions.not.for.sd <- c(conditions.not.for.sd,
                                 paste0("all(is.na(x$\uffff) | (x$\uffff <= ", upper.bound, "))"));
    }
  }

  # do the minimum
  min.title <- paste0(title, ".min");
  min.desc  <- paste0("the minimum value of ", description);
  mean.title <- paste0(title, ".mean");
  mean.desc  <- paste0("the arithmetic mean of ", description);
  med.title <- paste0(title, ".med");
  med.desc  <- paste0("the median of ", description);
  mode.title <- paste0(title, ".mode");
  mode.desc  <- paste0("the mode, i.e., the most frequent value, of ", description);
  max.title <- paste0(title, ".max");
  max.desc  <- paste0("the maximum value of ", description);
  sd.title <- paste0(title, ".sd");
  sd.desc  <- paste0("the standard deviation of ", description);

  # add conditions
  conditions <- c(
          gsub("\uffff", min.title,
             c(conditions.for.all,
               conditions.not.for.sd,
               conditions.for.int), fixed=TRUE),
          gsub("\uffff", mean.title,
             c(conditions.for.all, conditions.not.for.sd), fixed=TRUE),
          gsub("\uffff", med.title,
               c(conditions.for.all, conditions.not.for.sd), fixed=TRUE),
          gsub("\uffff", mode.title,
               c(conditions.for.all, conditions.not.for.sd), fixed=TRUE),
          gsub("\uffff", max.title,
               c(conditions.for.all,
                 conditions.not.for.sd,
                 conditions.for.int), fixed=TRUE),
          gsub("\uffff", sd.title,
               c(conditions.for.all, "(x$\uffff >= 0)"),
               fixed=TRUE),
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
                ") | xor((x$", sd.title, " <= 0) , (",
                min.title, " < x$", max.title, ")))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mean.title, ") | is.na(x$", sd.title,
                ") | xor((x$", sd.title, " <= 0) , (",
                min.title, " < x$", mean.title, ")))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                max.title, ") | is.na(x$", sd.title,
                ") | xor((x$", sd.title, " <= 0) , (",
                mean.title, " < x$", max.title, ")))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                min.title, " >= x$", med.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                max.title, " <= x$", med.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mean.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                min.title, " >= x$", mean.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mean.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                max.title, " <= x$", mean.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                min.title, " >= x$", mode.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                max.title, " <= x$", mode.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                mean.title, " == x$", med.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                mode.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                mean.title, " == x$", mode.title, "))"),
         paste0("all(is.na(x$", mode.title, ") | is.na(x$",
                med.title, ") | is.na(x$", sd.title,
                ") | (x$", sd.title, " > 0) | (",
                mode.title, " == x$", med.title, "))"),
         paste0("all(is.na(x$", sd.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", sd.title, " <= 0))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                max.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", min.title, " >= x$",
                max.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mean.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", min.title, " >= x$",
                mean.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", min.title, " >= x$",
                med.title, "))"),
         paste0("all(is.na(x$", min.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", min.title, " >= x$",
                mode.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mean.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", max.title, " <= x$",
                mean.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", max.title, " <= x$",
                med.title, "))"),
         paste0("all(is.na(x$", max.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", max.title, " <= x$",
                mode.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", mean.title, " == x$",
                med.title, "))"),
         paste0("all(is.na(x$", mean.title, ") | is.na(x$",
                mode.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", mean.title, " == x$",
                mode.title, "))"),
         paste0("all(is.na(x$", mode.title, ") | is.na(x$",
                med.title, ") | is.na(x$",
                number.column, ") | (x$", number.column,
                " > 1L) | (x$", mode.title, " == x$",
                med.title, "))")
         );
  # combine
  all <- list(columns = list(min=list(title=min.title,
                                    description=min.desc),
                             mean=list(title=mean.title,
                                      description=mean.desc),
                             med=list(title=med.title,
                                      description=med.desc),
                            mode=list(title=mode.title,
                                      description=mode.desc),
                             max=list(title=max.title,
                                      description=max.desc),
                             sd=list(title=sd.title,
                                      description=sd.desc)),
              conditions = conditions);

  return(all);
}
