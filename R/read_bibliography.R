#' @title Generate a Bibliographic List from a BibTeX File
#' @description This function loads and parses a bibliography from the given
#'   bibtex file and turns it into a data frame. The goal is to allow for
#'   easy look-up and processing of BibTeX data along with an R package.
#' @param bib.file the file
#' @return a \code{data.frame} with four columns, namely \code{id}: the BibTeX
#'   entry's ID, \code{type}: the BibTeX entry's entry type, \code{bibtex}: the
#'   raw bibtex code, and \code{text} a formatted representation of the BibTeX
#'   that can be copy-pasted into package documentations
#' @export read.bibliography
#' @importFrom bibtex read.bib
read.bibliography <- function(bib.file) {
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
  entries <- vapply(entries, function(i) paste(unlist(i), sep="\n", collapse="\n"), "");
  stopifnot(length(entries) == length(entries.names),
            all(nchar(entries) > 0L));


  order <- order(entries.names);
  entries.names <- entries.names[order];
  entries.types <- entries.types[order];
  entries <- entries[order];

  # format the entries
  tempfile <- tempfile(fileext=".bib");
  formatted <- vapply(entries,
                      function(e) {
                        use <- gsub("\\_", "_", e, fixed=TRUE);
                        stopifnot(nchar(use) > 0L);
                        use <- strsplit(use, "\\n", fixed=TRUE);
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
                        read <- format(read, style="text");
                        stopifnot(length(read) == 1L,
                                  nchar(read) > 0L);
                        return(read);
                      }, "");
  stopifnot(length(formatted) == length(entries),
            !file.exists(tempfile));

  result <- data.frame(id=entries.names,
                       type=as.factor(entries.types),
                       bibtex=entries,
                       text=formatted,
                       stringsAsFactors = FALSE);

  return(result);
}
