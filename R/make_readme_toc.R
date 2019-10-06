# create the link to a line
.create.github.readme.link <- function(toc) {
  stopifnot(is.character(toc),
            length(toc) == 1L,
            !is.na(toc),
            nchar(toc) > 0L);
  toc <- trimws(toc);
  while(startsWith(toc, "#")) {
    toc <- substr(toc, 2L, nchar(toc));
    toc <- trimws(toc);
  }
  toc <- gsub("[^ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 -]", "", toc);
  toc <- gsub("\\s+", "-", toc);
  toc <- gsub("--", "-", toc, fixed=TRUE);
  toc <- paste0("#", tolower(toc));
  stopifnot(nchar(toc) > 1L);
  return(toc);
}

#' @title Number the Sections in a README.md File and Create a Table of Contents
#' @description The sections in a README.md file are automatically numbered
#'   hierarchically. Then, the line containing the string "{toc}" is replaced
#'   with a table of contents.
#' @param text multiple lines of text of a README.md file
#' @param min.level the minimum section nesting level included in the table of
#'   contents. This is set to 2, meaning that there should be at most one "#"
#'   level with the overall title, while levels starting with "##" are included
#'   into the table of contents.
#' @return the text with sections numbered and a table of contents included
#' @export make.readme.toc
make.readme.toc <- function(text, min.level=2L) {
  stopifnot(is.character(text),
            length(text) > 0L,
            all(!(is.na(text))),
            is.integer(min.level),
            min.level > 0L,
            is.finite(min.level),
            !is.na(min.level),
            length(min.level) == 1L);

  levels <- rep.int(0L, 10L);

  text <- trimws(text, which="right");

# handle included source code examples
  toggles <- startsWith(text, "```");
  if(any(toggles)) {
    stopifnot((sum(toggles) %% 2L) == 0L);
    allowed <- TRUE;
    for(i in seq_along(toggles)) {
      allowed <- xor(allowed, toggles[[i]]);
      toggles[[i]] <- allowed;
    }
  } else {
    toggles <- rep.int(TRUE, length(text));
  }

# done handline code, now building toc
  depth <- min.level + 1L;
  toc <- character(0);

  for(i in seq_along(text)) {
    if(toggles[[i]]) {
      line <- text[[i]];
      stopifnot(is.character(line),
                length(line) == 1L,
                !is.na(line));
      line <- trimws(line);
      if(startsWith(line, "#")) {

        depth <- 0L;

        line <- trimws(line);
        while(startsWith(line, "#")) {
          line <- substr(line, 2L, nchar(line));
          line <- trimws(line);
          depth <- depth + 1L;
        }

        stopifnot(depth > 0L,
                  !startsWith("#", line),
                  nchar(line) > 0L,
                  depth <= length(levels));


        current <- levels[[depth]] + 1L;
        if(depth < length(levels)) {
          levels[seq.int(from=(depth+1L), to=length(levels))] <- 0L;
        }
        levels[[depth]] <- current;

        repl <- paste0(paste(rep.int("#", depth), sep="", collapse=""), " ");
        depth.string <- "";
        depth.sep <- "";
        if(depth >= min.level) {
          depth.string <- paste0(paste(levels[seq.int(from=min.level, to=depth)], sep=".", collapse="."), ". ");
          repl <- paste0(repl, depth.string);
          if(depth > min.level) {
            depth.sep <- paste(rep.int("  ", (depth - min.level)), sep="", collapse="");
          }
        }
        repl <- paste0(repl, line);
        if(depth >= min.level) {
          toc <- unname(unlist(c(toc, paste0(depth.sep, "- [", depth.string, line, "](",
                                             .create.github.readme.link(repl), ")"))));
        }

        text[[i]] <- repl;
      }
    }
  }

  find <- grep("{toc}", trimws(text), fixed=TRUE);
  stopifnot(is.integer(find),
            length(find) %in% c(0L, 1L));
  if(length(find) == 1L) {
    if(find > 1L) {
      text.1 <- text[1L:(find-1L)];
    } else {
      text.1 <- character(0);
    }
    if(find < length(text)) {
      text.2 <- text[(find+1L):length(text)];
    } else {
      text.2 <- character(0);
    }

    if(length(toc) > 0L) {
      text <- unname(unlist(c(text.1,
                              paste0(paste(rep.int("#", min.level), sep="", collapse=""), " Table of Contents"),
                              "",
                              toc,
                              "",
                              text.2)));
    } else {
      text <- unname(unlist(c(text.1, text.2)));
    }
  }

  text <- force(text);
  text <- do.call(force, list(text));
  return(text);
}
