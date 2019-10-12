
#' @title Extend an Instance Data Frame by Adding the Best-Known Solutions from
#'   Literature
#' @description Take an results data frame and extract, for each instance, the
#'   best-known solutions as well as the accompanying references.
#' @param instances the instance frame
#' @param results the results data frame
#' @param are.objective.values.ints are the objective values integers?
#' @param is.time.int are time values integers?
#' @return the expanded instance frame
#' @include names.R
#' @include types.R
#' @export append.bks.to.instance.frame
append.bks.to.instance.frame <- function(instances, results,
                                         are.objective.values.ints=TRUE,
                                         is.time.int=TRUE) {
  old.options <- options(warn=2);

# get the result column names
  .col.best.f.min <- paste0(.col.best.f, ".", .col.min.name);
  .col.best.f.mean <- paste0(.col.best.f, ".", .col.mean.name);
  .col.best.f.med <- paste0(.col.best.f, ".", .col.med.name);
  .col.best.f.mode <- paste0(.col.best.f, ".", .col.mode.name);
  .col.best.f.max <- paste0(.col.best.f, ".", .col.max.name);

# get the time column names
  .col.reach.best.f.min.time.min <- paste0(.col.reach.best.f.min.time, ".", .col.min.name);
  .col.reach.best.f.min.time.mean <- paste0(.col.reach.best.f.min.time, ".", .col.mean.name);
  .col.reach.best.f.min.time.med <- paste0(.col.reach.best.f.min.time, ".", .col.med.name);
  .col.reach.best.f.min.time.mode <- paste0(.col.reach.best.f.min.time, ".", .col.mode.name);
  .col.reach.best.f.min.time.max <- paste0(.col.reach.best.f.min.time, ".", .col.max.name);
  .col.total.time.max <- paste0(.col.total.time, ".", .col.max.name);
  .col.budget.time <- .col.budget.time;

# check the data
  stopifnot(is.data.frame(instances),
            nrow(instances) > 0L,
            ncol(instances) >= 2L,
            all(c(.col.inst.id, .col.inst.opt.bound.lower) %in% colnames(instances)),
            is.data.frame(results),
            nrow(results) > 0L,
            ncol(results) >= 3L,
            all(c(.col.inst.id,
                  .col.inst.opt.bound.lower,
                  .col.ref.id,
                  .col.ref.year) %in% colnames(results)),
            sum(c(.col.best.f.min,
                  .col.best.f.mean,
                  .col.best.f.med,
                  .col.best.f.mode,
                  .col.best.f.max) %in% colnames(results)) > 0L,
            sum(c(.col.reach.best.f.min.time.min,
                  .col.reach.best.f.min.time.mean,
                  .col.reach.best.f.min.time.med,
                  .col.reach.best.f.min.time.mode,
                  .col.reach.best.f.min.time.max,
                  .col.total.time.max,
                  .col.budget.time) %in% colnames(results)) > 0L
            );

  if(exists("logger", globalenv())) {
    logger <- get("logger", globalenv());
    if(is.function(logger)) {
      logger("Now computing the best-known solutions from results.");
    } else { logger <- NULL; }
  } else { logger <- NULL; }

  if(!is.null(logger)) {
    logger("First extracting instance/literature relations.");
  }

# extracting instance and reference information
  inst.id <- as.character(unname(unlist(instances[.col.inst.id])));
  stopifnot(all(is.character(inst.id)),
            all(!(is.na(inst.id))),
            all(nchar(inst.id) > 0L),
            length(inst.id) == nrow(instances));

  inst.opt.bound.lower <- unname(unlist(instances[.col.inst.opt.bound.lower]));
  stopifnot(all(is.numeric(inst.opt.bound.lower)),
            all(is.finite(inst.opt.bound.lower)),
            length(inst.opt.bound.lower) == nrow(instances),
            all(!is.na(inst.opt.bound.lower)));

  results.inst.id <- as.character(unname(unlist(results[.col.inst.id])));
  stopifnot(all(is.character(results.inst.id)),
            all(!(is.na(results.inst.id))),
            all(nchar(results.inst.id) > 0L),
            length(results.inst.id) == nrow(results));

  results.ref.id <- as.character(unname(unlist(results[.col.ref.id])));
  stopifnot(all(is.character(results.ref.id)),
            all(!(is.na(results.ref.id))),
            all(nchar(results.ref.id) > 0L),
            length(results.ref.id) == nrow(results));

  results.inst.opt.bound.lower <- unname(unlist(results[.col.inst.opt.bound.lower]));
  stopifnot(all(is.numeric(results.inst.opt.bound.lower)),
            all(is.finite(results.inst.opt.bound.lower)),
            length(results.inst.opt.bound.lower) == nrow(results),
            all(!is.na(results.inst.opt.bound.lower)));


  if(!is.null(logger)) {
    logger("Finished extracting instance/literature relations, extracting best results information.");
  }

# extracting best result information
  have.best.f.min <- FALSE;

  if(.col.best.f.min %in% colnames(results)) {
    results.best.f.min <- unname(unlist(results[.col.best.f.min]));
    stopifnot(all(is.na(results.best.f.min) | is.finite(results.best.f.min)),
              all(is.na(results.best.f.min) | (results.best.f.min >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.min)),
              length(results.best.f.min) == nrow(results));
    if(are.objective.values.ints) {
      stopifnot(all(is.integer(results.best.f.min)));
    }
    have.best.f.min <- TRUE;
  } else {
    results.best.f.min <- NA_integer_;
  }

  if(.col.best.f.mean %in% colnames(results)) {
    results.best.f.mean <- unname(unlist(results[.col.best.f.mean]));
    stopifnot(all(is.na(results.best.f.mean) | is.finite(results.best.f.mean)),
              all(is.na(results.best.f.mean) | (results.best.f.mean >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.mean)),
              length(results.best.f.mean) == nrow(results));
    have.best.f.min <- TRUE;
  } else {
    results.best.f.mean <- NA_integer_;
  }

  if(.col.best.f.med %in% colnames(results)) {
    results.best.f.med <- unname(unlist(results[.col.best.f.med]));
    stopifnot(all(is.na(results.best.f.med) | is.finite(results.best.f.med)),
              all(is.na(results.best.f.med) | (results.best.f.med >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.med)),
              length(results.best.f.med) == nrow(results));
    have.best.f.min <- TRUE;
  } else {
    results.best.f.med <- NA_integer_;
  }

  if(.col.best.f.mode %in% colnames(results)) {
    results.best.f.mode <- unname(unlist(results[.col.best.f.mode]));
    stopifnot(all(is.na(results.best.f.mode) | is.finite(results.best.f.mode)),
              all(is.na(results.best.f.mode) | (results.best.f.mode >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.mode)),
              length(results.best.f.mode) == nrow(results));
    have.best.f.min <- TRUE;
  } else {
    results.best.f.mode <- NA_integer_;
  }

  if(.col.best.f.max %in% colnames(results)) {
    results.best.f.max <- unname(unlist(results[.col.best.f.max]));
    stopifnot(all(is.na(results.best.f.max) | is.finite(results.best.f.max)),
              all(is.na(results.best.f.max) | (results.best.f.max >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.max)),
              length(results.best.f.max) == nrow(results));
    if(are.objective.values.ints) {
      stopifnot(all(is.integer(results.best.f.max)));
    }
    have.best.f.min <- TRUE;
  } else {
    results.best.f.max <- NA_integer_;
  }

  stopifnot(have.best.f.min);
  rm("have.best.f.min");

  if(!is.null(logger)) {
    logger("computing the best values obtained by any run.");
  }
  results.best.f <- vapply(seq_len(nrow(results)), function(i) {
    res <- results.best.f.min[[i]];
    res.2 <- results.best.f.mean[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- results.best.f.med[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- results.best.f.mode[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- results.best.f.max[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    stopifnot(is.numeric(res),
              !is.na(res),
              is.finite(res),
              res >= results.inst.opt.bound.lower[[i]]);
    return(res);
  }, NA_real_);

  rm("results.best.f.min");
  rm("results.best.f.mean");
  rm("results.best.f.med");
  rm("results.best.f.mode");
  rm("results.best.f.max");

  stopifnot(length(results.best.f) == nrow(results),
            all(is.na(results.best.f) | (results.best.f >= results.inst.opt.bound.lower)));

  results.best.f <- try.convert.numeric.to.int(x=results.best.f,
                                              stopIfFails=are.objective.values.ints,
                                              canFloor=are.objective.values.ints);
  results.best.f <- force(results.best.f);
  results.best.f <- do.call(force, list(results.best.f));
  stopifnot(length(results.best.f) == nrow(results),
            all(is.na(results.best.f) | (results.best.f >= results.inst.opt.bound.lower)));

  if(all(is.integer(results.best.f))) {
    template <- NA_integer_;
  } else {
    template <- NA_real_;
  }
  template <- force(template);

  if(!is.null(logger)) {
    logger("best values determined, now determining the required runtimes.");
  }

  have.time <- FALSE;

  if(.col.reach.best.f.min.time.min %in% colnames(results)) {
    reach.best.f.min.time.min <- unname(unlist(results[.col.reach.best.f.min.time.min]));
    stopifnot(all(is.na(reach.best.f.min.time.min) | is.finite(reach.best.f.min.time.min)),
              all(is.na(reach.best.f.min.time.min) | (reach.best.f.min.time.min >= 0L)),
              all(is.numeric(reach.best.f.min.time.min)),
              length(reach.best.f.min.time.min) == nrow(results));
    if(is.time.int) {
      stopifnot(all(is.integer(reach.best.f.min.time.min)));
    }
    have.time <- TRUE;
  } else {
    reach.best.f.min.time.min <- NA_integer_;
  }

  if(.col.reach.best.f.min.time.mean %in% colnames(results)) {
    reach.best.f.min.time.mean <- unname(unlist(results[.col.reach.best.f.min.time.mean]));
    stopifnot(all(is.na(reach.best.f.min.time.mean) | is.finite(reach.best.f.min.time.mean)),
              all(is.na(reach.best.f.min.time.mean) | (reach.best.f.min.time.mean >= 0L)),
              all(is.numeric(reach.best.f.min.time.mean)),
              length(reach.best.f.min.time.mean) == nrow(results));
    have.time <- TRUE;
  } else {
    reach.best.f.min.time.mean <- NA_integer_;
  }

  if(.col.reach.best.f.min.time.med %in% colnames(results)) {
    reach.best.f.min.time.med <- unname(unlist(results[.col.reach.best.f.min.time.med]));
    stopifnot(all(is.na(reach.best.f.min.time.med) | is.finite(reach.best.f.min.time.med)),
              all(is.na(reach.best.f.min.time.med) | (reach.best.f.min.time.med >= 0L)),
              all(is.numeric(reach.best.f.min.time.med)),
              length(reach.best.f.min.time.med) == nrow(results));
    have.time <- TRUE;
  } else {
    reach.best.f.min.time.med <- NA_integer_;
  }

  if(.col.reach.best.f.min.time.mode %in% colnames(results)) {
    reach.best.f.min.time.mode <- unname(unlist(results[.col.reach.best.f.min.time.mode]));
    stopifnot(all(is.na(reach.best.f.min.time.mode) | is.finite(reach.best.f.min.time.mode)),
              all(is.na(reach.best.f.min.time.mode) | (reach.best.f.min.time.mode >= 0L)),
              all(is.numeric(reach.best.f.min.time.mode)),
              length(reach.best.f.min.time.mode) == nrow(results));
    have.time <- TRUE;
  } else {
    reach.best.f.min.time.mode <- NA_integer_;
  }

  if(.col.reach.best.f.min.time.max %in% colnames(results)) {
    reach.best.f.min.time.max <- unname(unlist(results[.col.reach.best.f.min.time.max]));
    stopifnot(all(is.na(reach.best.f.min.time.max) | is.finite(reach.best.f.min.time.max)),
              all(is.na(reach.best.f.min.time.max) | (reach.best.f.min.time.max >= 0L)),
              all(is.numeric(reach.best.f.min.time.max)),
              length(reach.best.f.min.time.max) == nrow(results));
    if(is.time.int) {
      stopifnot(all(is.integer(reach.best.f.min.time.max)));
    }
    have.time <- TRUE;
  } else {
    reach.best.f.min.time.max <- NA_integer_;
  }

  if(.col.total.time.max %in% colnames(results)) {
    total.time.max <- unname(unlist(results[.col.total.time.max]));
    stopifnot(all(is.na(total.time.max) | is.finite(total.time.max)),
              all(is.na(total.time.max) | (total.time.max >= 0L)),
              all(is.numeric(total.time.max)),
              length(total.time.max) == nrow(results));
    if(is.time.int) {
      stopifnot(all(is.integer(total.time.max)));
    }
    have.time <- TRUE;
  } else {
    total.time.max <- NA_integer_;
  }

  if(.col.budget.time %in% colnames(results)) {
    budget.time <- unname(unlist(results[.col.budget.time]));
    stopifnot(all(is.na(budget.time) | is.finite(budget.time)),
              all(is.na(budget.time) | (budget.time >= 0L)),
              all(is.numeric(budget.time)),
              length(budget.time) == nrow(results));
    if(is.time.int) {
      stopifnot(all(is.integer(budget.time)));
    }
    have.time <- TRUE;
  } else {
    budget.time <- NA_integer_;
  }

  stopifnot(have.time);
  rm("have.time");

  time <- vapply(seq_len(nrow(results)), function(i) {
    res <- reach.best.f.min.time.min[[i]];
    res.2 <- reach.best.f.min.time.mean[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- reach.best.f.min.time.med[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- reach.best.f.min.time.mode[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- reach.best.f.min.time.max[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- total.time.max[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    res.2 <- budget.time[[i]];
    if(is.na(res) || ((!is.na(res.2)) && (res.2 < res))) { res <- res.2; res <- force(res); }
    stopifnot(is.numeric(res),
              is.na(res) | is.finite(res),
              is.na(res) | (res >= 0L));
    return(res);
  }, NA_real_);

  rm("reach.best.f.min.time.min");
  rm("reach.best.f.min.time.mean");
  rm("reach.best.f.min.time.med");
  rm("reach.best.f.min.time.mode");
  rm("reach.best.f.min.time.max");
  rm("total.time.max");
  rm("budget.time");

  stopifnot(length(time) == nrow(results),
            all(is.na(time) | (time >= 0L)));

  time <- try.convert.numeric.to.int(x=time,
                                     stopIfFails=FALSE,
                                     canFloor=FALSE);
  time <- force(time);
  time <- do.call(force, list(time));
  stopifnot(length(time) == nrow(results),
            all(is.na(time) | (time >= 0L)));

  if(all(is.integer(time))) {
    time.template <- NA_integer_;
  } else {
    time.template <- NA_real_;
  }
  time.template <- force(time.template);

  if(!is.null(logger)) {
    logger("time values determined, now computing bks values.");
  }

  results.ref.year <- unname(unlist(results[.col.ref.year]));
  stopifnot(all(!(is.na(results.ref.year))),
            all(is.integer(results.ref.year)),
            all(is.finite(results.ref.year)),
            length(results.ref.year) == nrow(results));

# integer computation
  results.bks <- vapply(seq_along(inst.id), function(i) {
    id <- inst.id[[i]];
    bound <- inst.opt.bound.lower[[i]];
    stopifnot(nchar(id) > 0L,
              is.finite(bound));
    sel <- (results.inst.id == id);
    stopifnot(all(results.inst.opt.bound.lower[sel] == bound));

    sel <- sel & (!is.na(results.best.f));
    if(any(sel)) {
      best <- min(results.best.f[sel], na.rm = TRUE);
      stopifnot(!is.na(best),
                is.finite(best),
                best >= bound);
      return(best);
    }
    return(template);
  }, template);

  if(!is.null(logger)) {
    logger("bks values determined, now computing bks references.");
  }

# get the bks, now getting the references
  results.bks.refs <- vapply(seq_along(inst.id), function(i) {
    bks <- results.bks[[i]];
    if(is.na(bks)) {
      return(NA_character_);
    }
    id <- inst.id[[i]];
    sel <- (results.inst.id == id) & (results.best.f <= bks);
    stopifnot(!any(is.na(sel)));
    sel[is.na(sel)] <- FALSE;
    stopifnot(sum(sel) > 0L);
    refs <- results.ref.id[sel];
    stopifnot(all(is.character(refs)),
              length(refs) > 0L,
              all(nchar(refs) > 0L),
              !any(is.na(refs)));
    years <- results.ref.year[sel];
    stopifnot(all(is.integer(years)),
              all(is.finite(years)),
              all(years > 0L));

    ndup <- !duplicated(refs);
    refs <- refs[ndup];
    stopifnot(all(is.character(refs)),
              length(refs) > 0L,
              all(nchar(refs) > 0L),
              !any(is.na(refs)));

# only one reference? good
    if(length(refs) <= 1L) {
      return(refs[[1L]]);
    }
    years <- years[ndup];
    stopifnot(all(is.integer(years)),
              all(is.finite(years)),
              all(years > 0L));

# multiple references? take the earliest ones
    years.min <- min(years);
    stopifnot(is.finite(years.min), years.min > 0L);

    refs <- refs[years <= years.min];
    stopifnot(length(refs) > 0L,
              !any(is.na(refs)),
              all(nchar(refs) > 0L));

# only one earliest reference? good
    if(length(refs) <= 1L) {
      return(refs[[1L]]);
    }

    refs <- paste(sort(refs), sep=";", collapse=";");
  }, NA_character_);

  results.bks.refs <- force(results.bks.refs);
  results.bks.refs <- do.call(force, list(results.bks.refs));

  stopifnot(all(is.na(results.bks) == is.na(results.bks.refs)),
            all(is.na(results.bks) | (nchar(results.bks.refs) > 0L)),
            all((!is.na(results.bks.refs)) == (is.finite(results.bks))),
            all(is.na(results.bks) | (results.bks >= inst.opt.bound.lower)));

  if(!is.null(logger)) {
    logger("bks references determined, now computing fastest bks computation");
  }

  # get the bks, now getting the references
  bks.time <- vapply(seq_along(inst.id), function(i) {
    bks <- results.bks[[i]];
    if(is.na(bks)) {
      return(NA_integer_);
    }
    id <- inst.id[[i]];
    sel <- (results.inst.id == id) & (results.best.f <= bks);
    stopifnot(!any(is.na(sel)));
    sel[is.na(sel)] <- FALSE;
    stopifnot(sum(sel) > 0L);
    times <- time[sel];
    stopifnot(all(is.numeric(times)));
    sel <- !is.na(times);
    if(any(sel)) {
      return(min(times[sel], na.rm = TRUE));
    }
    return(NA_integer_);
  }, time.template);

  bks.time <- try.convert.numeric.to.int(x=bks.time,
                                         stopIfFails=FALSE,
                                         canFloor=FALSE);

  bks.time <- force(bks.time);
  bks.time <- do.call(force, list(bks.time));

  # get the bks, now getting the references
  bks.time.refs <- vapply(seq_along(inst.id), function(i) {
    bks <- results.bks[[i]];
    if(is.na(bks)) {
      return(NA_character_);
    }
    btime <- bks.time[[i]];
    if(is.na(btime)) {
      return(NA_character_);
    }
    id <- inst.id[[i]];
    sel <- (results.inst.id == id) &
           (results.best.f <= bks) &
           (time <= btime);
    stopifnot(any(!is.na(sel)));
    sel[is.na(sel)] <- FALSE;
    stopifnot(sum(sel) > 0L);
    refs <- results.ref.id[sel];
    stopifnot(all(is.character(refs)),
              length(refs) > 0L,
              all(nchar(refs) > 0L),
              !any(is.na(refs)));
    years <- results.ref.year[sel];
    stopifnot(all(is.integer(years)),
              all(is.finite(years)),
              all(years > 0L));

    ndup <- !duplicated(refs);
    refs <- refs[ndup];
    stopifnot(all(is.character(refs)),
              length(refs) > 0L,
              all(nchar(refs) > 0L),
              !any(is.na(refs)));

    # only one reference? good
    if(length(refs) <= 1L) {
      return(refs[[1L]]);
    }
    years <- years[ndup];
    stopifnot(all(is.integer(years)),
              all(is.finite(years)),
              all(years > 0L));

    # multiple references? take the earliest ones
    years.min <- min(years);
    stopifnot(is.finite(years.min), years.min > 0L);

    refs <- refs[years <= years.min];
    stopifnot(length(refs) > 0L,
              !any(is.na(refs)),
              all(nchar(refs) > 0L));

    # only one earliest reference? good
    if(length(refs) <= 1L) {
      return(refs[[1L]]);
    }

    refs <- paste(sort(refs), sep=";", collapse=";");
  }, NA_character_);

  bks.time.refs <- force(bks.time.refs);
  bks.time.refs <- do.call(force, list(bks.time.refs));

  stopifnot(all(is.na(bks.time) == is.na(bks.time.refs)),
            all(is.na(bks.time) | (nchar(bks.time.refs) > 0L)),
            all((!is.na(bks.time.refs)) == (is.finite(bks.time))),
            all(is.na(bks.time) | (bks.time >= 0L)));

  if(!is.null(logger)) {
    logger("done computing all the infos, now creating the frame");
  }

# done, make the frame
  results.bks <- try.convert.numeric.to.int(x=results.bks,
                                            stopIfFails=are.objective.values.ints,
                                            canFloor=are.objective.values.ints);

  instances[.col.inst.bks] <- results.bks;
  instances[.col.inst.bks.ref] <- results.bks.refs;
  instances[.col.inst.bks.time] <- bks.time;
  instances[.col.inst.bks.time.ref] <- bks.time.refs;

  instances <- force(instances);
  instances <- do.call(force, list(instances));

  if(!is.null(logger)) {
    logger("Finished computing the best-known solutions and their corresponding runtimes from the results.");
  }
  options(old.options);

  return(instances);
}
