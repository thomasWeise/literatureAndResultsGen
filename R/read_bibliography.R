.temp.test.bib <- function(bibliography) {

  ret <- system(paste0("pdflatex --version"));
  if(ret != 0L) { return(FALSE); }
  ret <- system(paste0("bibtex --version"));
  if(ret != 0L) { return(FALSE); }

  temp.dir <- tempfile();
  stopifnot(!file.exists(temp.dir));
  dir.create(path=temp.dir, recursive = TRUE);
  temp.dir <- normalizePath(temp.dir, mustWork = TRUE);
  stopifnot(dir.exists(temp.dir));

  bibliography <- unlist(c(unlist(bibliography), ""));

  bibliography.dest <- file.path(temp.dir, "bibliography.bib");
  writeLines(text=bibliography,
             con=bibliography.dest);
  stopifnot(file.exists(bibliography.dest),
            file.size(bibliography.dest) >= (sum(nchar(bibliography))+length(bibliography)));

  latex.name.base <- "article";
  latex.name <- paste0(latex.name.base, ".tex");
  latex.file <- file.path(temp.dir, latex.name);
  latex.text <- c("\\documentclass{article}%",
                  "\\begin{document}%",
                  "\\author{bla}%",
                  "\\title{bla}%",
                  "\\maketitle%",
                  "\\cite{*}%",
                  "\\bibliographystyle{unsrt}%",
                  paste0("\\bibliography{bibliography}%"),
                  "\\end{document}%",
                  "\\endinput%");
  latex.text <- force(latex.text);

  writeLines(text = latex.text,
             con=latex.file);
  stopifnot(file.exists(latex.file),
            file.size(latex.file) >= sum(nchar(latex.text)) + length(latex.text));

  pdflatex.args <- paste0("cd '", temp.dir, "' && pdflatex -halt-on-error -interaction=nonstopmode ", latex.name);
  bibtex.args <- paste0("cd '", temp.dir, "' && bibtex ", latex.name.base);
  stopifnot(system(pdflatex.args) == 0L);
  stopifnot(system(bibtex.args) == 0L);
  stopifnot(system(pdflatex.args) == 0L);
  stopifnot(system(bibtex.args) == 0L);
  stopifnot(system(pdflatex.args) == 0L);
  stopifnot(system(bibtex.args) == 0L);
  output <- system(intern=TRUE, paste0(bibtex.args, " 2>&1"));
  stopifnot(attr(output, "status") == 0L);

  stopifnot(length(output) > 0L,
            sum(nchar(output)) > 0L);

  warnings <- ((grepl("Warning--", output, fixed=TRUE) |
                grepl("Error--", output, fixed=TRUE)) & ((!
                grepl("Warning--edition", output, fixed=TRUE))));
  if(any(warnings)) {
    stop(paste(output[warnings], sep="\n", collapse="\n"));
  }

  stopifnot(file.exists(file.path(temp.dir, paste0(latex.name.base, ".pdf"))));
  bbl.file <- file.path(temp.dir, paste0(latex.name.base, ".bbl"));
  stopifnot(file.exists(bbl.file));

  bibliography2 <- readLines(bbl.file);
  bibliography2 <- force(bibliography2);
  stopifnot(length(bibliography2) > 0L,
            sum(nchar(bibliography2)) > 0L);

  unlink(temp.dir, recursive = TRUE);
}


#' @title Generate a Bibliographic List from a BibTeX File
#' @description This function loads and parses a bibliography from the given
#'   bibtex file and turns it into a data frame. The goal is to allow for easy
#'   look-up and processing of BibTeX data along with an R package.
#' @param bib.file the file
#' @return a \code{data.frame} with five columns, namely \code{ref.id}: the
#'   BibTeX entry's ID, \code{ref.type}: the BibTeX entry's entry type,
#'   \code{ref.year}: the year of the entry, \code{ref.as.bibtex}: the raw
#'   bibtex code, and \code{ref.as.text} a formatted representation of the
#'   BibTeX that can be copy-pasted into package documentations
#' @export read.bibliography
#' @importFrom bibtex read.bib
#' @include names.R
read.bibliography <- function(bib.file) {
  old.options <- options(warn=2);
  stopifnot(is.character(bib.file),
            length(bib.file) == 1L,
            nchar(bib.file) > 0L,
            file.exists(bib.file));
  bib.file <- normalizePath(bib.file, mustWork=TRUE);
  stopifnot(file.size(bib.file) > 0L);

  bib.text <- readLines(bib.file);
  stopifnot(length(bib.text) > 0L,
            sum(nchar(bib.text)) > 0L);
  bib.text <- trimws(bib.text);
  stopifnot(length(bib.text) > 0L,
            sum(nchar(bib.text)) > 0L);
  bib.text <- gsub("\\t", " ", bib.text, fixed=TRUE);

  # find all string constants
  strings <- grep("^\\s*@string\\s*\\{", bib.text);
  if(length(strings) > 0L) {
    # we got string constants
    consts   <- lapply(bib.text[strings],
                       function(s) {
                         s <- sub("^\\s*@string\\s*\\{\\s*(.*?)\\s*=\\s*\\\"(.*?)\\\"\\s*\\}",
                                      "\\1\uffff\\2", s);
                         stopifnot(nchar(s) > 3L,
                                   grepl("\uffff", s, fixed = TRUE));
                         s <- strsplit(s, "\uffff", fixed = TRUE);
                         stopifnot(length(s) == 1L);
                         s <- s[[1L]];
                         stopifnot(length(s) == 2L,
                                   all(nchar(s) > 0L));
                         return(s);
                       });
    consts.names <- unique(vapply(consts, function(i) i[[1L]], ""));
    consts <- vapply(consts, function(i) i[[2L]], "");
    stopifnot(all(nchar(consts) > 0L),
              all(nchar(consts.names) > 0L),
              length(consts) == length(consts.names),
              length(consts) == length(strings));
    names(consts) <- consts.names;
    rm("consts.names");
    bib.text <- bib.text[-strings];
    # now we have name-constant pairs
    # and can replace them
    for(name in names(consts)) {
      use.name <- name;
      use.val  <- consts[name];

      for(char in c("\\", "{", "}", "[", "]", ".", "$", "^", "(", ")", "|", "*", "+", "?")) {
        rep <- paste0("\\", char);
        use.name <- gsub(char, rep, use.name, fixed=TRUE);
        use.val <- gsub(char, rep, use.val, fixed=TRUE);
      }

      expr <- paste0("(\\s*=\\s*)", use.name, "(\\s*,*)$");
      found <- grep(expr, bib.text);
      if(length(found) > 0L) {
        bib.text[found] <- gsub(expr,
                                paste0("\\1", use.val, "\\2"),
                                bib.text[found]);
      }

      expr <- paste0("(\\s*=\\s*)", use.name, "(\\s*\\#)");
      found <- grep(expr, bib.text);
      if(length(found) > 0L) {
        bib.text[found] <- gsub(expr,
                                paste0("\\1", use.val, "\\2"),
                                bib.text[found]);
      }

      expr <- paste0("(\\s*\\#\\s*)", use.name, "(\\s*\\#*)");
      found <- grep(expr, bib.text);
      if(length(found) > 0L) {
        bib.text[found] <- gsub(expr,
                                paste0("\\1", use.val, "\\2"),
                                bib.text[found]);
      }
    }
  }
  rm("strings");

  # merge all potentially split strings
  bib.text <- gsub("\\}\\s*#\\s*\\{(.*?)\\}\\s*#\\s*\\{", "\\{\\1\\}", bib.text);
  stopifnot(length(bib.text) > 0L,
            sum(nchar(bib.text)) > 0L);
  bib.text <- gsub("\\}\\s*#\\s*\\{", "", bib.text);
  stopifnot(length(bib.text) > 0L,
            sum(nchar(bib.text)) > 0L);
  bib.text <- trimws(bib.text, which="right");
  stopifnot(length(bib.text) > 0L,
            sum(nchar(bib.text)) > 0L);

  # deal with unescaped % signs
  bib.text <- gsub("([^\\])\\%", "\\1\\\\%", bib.text, fixed=FALSE);

  #detect entries
  expr <- "^\\s*\\@([a-zA-Z]+)\\{([a-zA-Z0-9,:_]+?)\\,\\s*$";
  entries <- grep(expr, bib.text);
  stopifnot(length(entries) > 0L,
            is.integer(entries));
  entries.names <- unique(vapply(entries, function(i) sub(expr, "\\2", bib.text[[i]]), ""));
  stopifnot(length(entries.names) == length(entries),
            all(nchar(entries.names) > 0L));
  entries.types <- tolower(trimws(vapply(entries, function(i) sub(expr, "\\1", bib.text[[i]]), "")));
  stopifnot(length(entries.types) == length(entries),
            all(nchar(entries.types) > 0L));
  entries <- lapply(seq_along(entries),
                    function(i) {
                      start <- entries[[i]];
                      if(i < length(entries)) {
                        end <- entries[[i+1L]] - 1L;
                      } else {
                        end <- length(bib.text);
                      }
                      stopifnot(end > start);
                      entry <- bib.text[start:end];
                      stopifnot(length(entry) > 0L,
                                sum(nchar(entry)) > 0L);
                      entry <- entry[nchar(trimws(entry)) > 0L];
                      l <- length(entry);
                      stopifnot(l > 0L,
                                sum(nchar(entry)) > 0L);
                      end <- trimws(entry[[l]]);
                      stopifnot(nchar(end) > 0L);
                      if(end == "},") { end <- "}" };
                      stopifnot(end == "}");
                      start <- trimws(entry[[1L]]);
                      stopifnot(nchar(start) > 0L);
                      if(l > 2L) {
                        entry <- c(start, vapply(gsub("\\s+", " ", entry[2L:(l-1L)]),
                                                 function(xx) paste0("  ", xx), ""),
                                   end);
                      } else {
                        entry[[l]] <- end;
                        entry[[1L]] <- start;
                      }
                      stopifnot(length(entry) > 0L,
                                sum(nchar(entry)) > 0L);
                      return(entry);
                    });

  stopifnot(length(entries) == length(entries.names),
            all(sum(vapply(entries, function(i) sum(vapply(i, nchar, 0L)), 0L)) > 0L));

# pipe everything through bibtex once, as sanity check
  .dummy <- "aBcs5G'+";
  .replace <- function(vv,ll) gsub(.dummy, paste0("\\", vv),
                              gsub(vv, paste0("\\", vv),
                              gsub(paste0("\\", vv), .dummy, ll, fixed=TRUE),
                              fixed=TRUE), fixed=TRUE);
  l <- .replace("%", .replace("&", .replace("_", unlist(entries, recursive = TRUE))));
  .temp.test.bib(l);
  rm("l");
  rm(".replace");
  rm(".dummy");
# done piping everything through bibtex once, as sanity check

  entries <- vapply(entries, function(i) paste(unlist(i), sep="\n", collapse="\n"), "");
  stopifnot(length(entries) == length(entries.names),
            all(nchar(entries) > 0L));

  entries.years <- vapply(entries, function(e) {
    e <- gsub("\\s+", "", e);
    stopifnot(nchar(e) > 9L);
    y <- gsub(".*[\\},]year=[\\{]?([0-9]+).*", "\\1", e);
    stopifnot(nchar(y) == 4L);
    y <- as.integer(y);
    stopifnot(!is.na(y),
               is.finite(y),
               y > 1900L,
               y < 3000L);
    return(y);
  }, NA_integer_)

  order <- order(entries.names, entries.years);
  entries.names <- entries.names[order];
  entries.types <- entries.types[order];
  entries.years <- entries.years[order];
  entries <- entries[order];

  # format the entries as text
  tempfile <- tempfile(fileext=".bib");
  formatted <- vapply(entries,
                      function(e) {
                        use <- gsub("\\_", "_", e, fixed=TRUE);
                        stopifnot(nchar(use) > 0L);
                        use <- strsplit(use, "\n", fixed=TRUE);
                        stopifnot(length(use) == 1L);
                        use <- use[[1L]];
                        stopifnot(length(use) > 0L,
                                  all(nchar(use) > 0L));
                        stopifnot(!file.exists(tempfile));
                        writeLines(text=use, con=tempfile);
                        stopifnot(file.exists(tempfile));

                        read <- read.bib(file=tempfile);
                        stopifnot(typeof(read) == "list",
                                  length(read) == 1L);

                        file.remove(tempfile);
                        stopifnot(!file.exists(tempfile));

                        read <- read[[1L]];
                        stopifnot(typeof(read) == "list",
                                  class(read) == "bibentry");
                        read <- suppressWarnings(format(read, style="text"));
                        stopifnot(length(read) == 1L,
                                  nchar(read) > 0L);

                        read <- gsub("\n", " ", read, fixed=TRUE);
                        read <- gsub("\\'i", "í", read, fixed=TRUE);
                        l1 <- nchar(read);
                        read <- sub("\\_(.+?)\\_\\,", "\\1\\,", read, fixed=FALSE);
                        l2 <- nchar(read);
                        if(l1 >= l2) {
                          read <- sub("\\_(.+?)\\_\\.", "\\1\\.", read, fixed=FALSE);
                        } else {
                          l1 <- l2;
                        }
                        read <- sub("\\*(.+?)\\*\\,", "\\1\\,", read, fixed=FALSE);
                        l2 <- nchar(read);
                        if(l1 >= l2) {
                          read <- sub("\\*(.+?)\\*\\(", "\\1\\(", read, fixed=FALSE);
                        } else {
                          l1 <- l2;
                        }
                        read <- gsub("([a-zA-Z0-9])~([0-9])", "\\1 \\2", read, fixed=FALSE);
                        return(read);
                      }, "");
  stopifnot(length(formatted) == length(entries),
            !file.exists(tempfile));

  args <- list(entries.names, as.factor(entries.types), entries.years, entries, formatted,
               FALSE);
  names(args) <- c(.col.ref.id, .col.ref.type, .col.ref.year, .col.ref.bibtex, .col.ref.text,
                   "stringsAsFactors");
  result <- do.call(data.frame, args);
  result <- force(result);
  options(old.options);
  return(result);
}
