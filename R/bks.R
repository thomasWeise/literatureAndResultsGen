
#' @title Extend an Instance Data Frame by Adding the Best-Known Solutions from
#'   Literature
#' @description Take an results data frame and extract, for each instance, the
#'   best-known solutions as well as the accompanying references.
#' @param instances the instance frame
#' @param results the results data frame
#' @param are.objective.values.ints are the objective values integers?
#' @return the expanded instance frame
#' @include names.R
#' @include types.R
#' @export append.bks.to.instance.frame
append.bks.to.instance.frame <- function(instances, results,
                                         are.objective.values.ints=TRUE) {
  old.options <- options(warn=2);

  .col.best.f.min <- paste0(.col.best.f, ".", .col.min.name);
  .col.best.f.mean <- paste0(.col.best.f, ".", .col.mean.name);
  .col.best.f.med <- paste0(.col.best.f, ".", .col.med.name);
  .col.best.f.mode <- paste0(.col.best.f, ".", .col.mode.name);
  .col.best.f.max <- paste0(.col.best.f, ".", .col.max.name);

  stopifnot(is.data.frame(instances),
            nrow(instances) > 0L,
            ncol(instances) >= 2L,
            all(c(.col.inst.id, .col.inst.opt.bound.lower) %in% colnames(instances)),
            is.data.frame(results),
            nrow(results) > 0L,
            ncol(results) >= 3L,
            all(c(.col.inst.id, .col.inst.opt.bound.lower, .col.ref.id, .col.ref.year) %in% colnames(results)),
            sum(c(.col.best.f.min,
                  .col.best.f.mean, .col.best.f.med, .col.best.f.mode,
                  .col.best.f.max) %in% colnames(results)) > 0L
            );

  if(exists("logger")) {
    logger("Now computing the best-known solutions from results.");
  }

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

  have <- FALSE;

  if(.col.best.f.min %in% colnames(results)) {
    results.best.f.min <- unname(unlist(results[.col.best.f.min]));
    stopifnot(all(is.na(results.best.f.min) | is.finite(results.best.f.min)),
              all(is.na(results.best.f.min) | (results.best.f.min >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.min)),
              length(results.best.f.min) == nrow(results));
    if(are.objective.values.ints) {
      stopifnot(all(is.integer(results.best.f.min)));
    }
    have <- TRUE;
  } else {
    results.best.f.min <- NA_integer_;
  }

  if(.col.best.f.mean %in% colnames(results)) {
    results.best.f.mean <- unname(unlist(results[.col.best.f.mean]));
    stopifnot(all(is.na(results.best.f.mean) | is.finite(results.best.f.mean)),
              all(is.na(results.best.f.mean) | (results.best.f.mean >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.mean)),
              length(results.best.f.mean) == nrow(results));
    have <- TRUE;
  } else {
    results.best.f.mean <- NA_integer_;
  }

  if(.col.best.f.med %in% colnames(results)) {
    results.best.f.med <- unname(unlist(results[.col.best.f.med]));
    stopifnot(all(is.na(results.best.f.med) | is.finite(results.best.f.med)),
              all(is.na(results.best.f.med) | (results.best.f.med >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.med)),
              length(results.best.f.med) == nrow(results));
    have <- TRUE;
  } else {
    results.best.f.med <- NA_integer_;
  }

  if(.col.best.f.mode %in% colnames(results)) {
    results.best.f.mode <- unname(unlist(results[.col.best.f.mode]));
    stopifnot(all(is.na(results.best.f.mode) | is.finite(results.best.f.mode)),
              all(is.na(results.best.f.mode) | (results.best.f.mode >= results.inst.opt.bound.lower)),
              all(is.numeric(results.best.f.mode)),
              length(results.best.f.mode) == nrow(results));
    have <- TRUE;
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
    have <- TRUE;
  } else {
    results.best.f.max <- NA_integer_;
  }

  stopifnot(have);

  if(exists("logger")) {
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

  if(exists("logger")) {
    logger("best values determined.");
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

# get the bks, now getting the references
  results.bks.refs <- vapply(seq_along(inst.id), function(i) {
    bks <- results.bks[[i]];
    if(is.na(bks)) {
      return(NA_character_);
    }
    id <- inst.id[[i]];
    sel <- (results.inst.id == id) & (results.best.f <= bks);
    stopifnot(!any(is.na(sel)),
              sum(sel) > 0L);
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
            all((!is.na(results.bks)) == (nchar(results.bks.refs) > 0L)),
            all((!is.na(results.bks.refs)) == (is.finite(results.bks))),
            all(is.na(results.bks) | (results.bks >= inst.opt.bound.lower)));

  results.bks <- try.convert.numeric.to.int(x=results.bks,
                                            stopIfFails=are.objective.values.ints,
                                            canFloor=are.objective.values.ints);

  instances[.col.inst.bks] <- results.bks;
  instances[.col.inst.bks.ref] <- results.bks.refs;

  instances <- force(instances);
  instances <- do.call(force, list(instances));

  if(exists("logger")) {
    logger("Finished computing the best-known solutions from results.");
  }
  options(old.options);

  return(instances);
}
