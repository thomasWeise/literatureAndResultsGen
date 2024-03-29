% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_md_references.R
\name{make.md.references}
\alias{make.md.references}
\title{Generate an Markdown Style References}
\usage{
make.md.references(refs, bibliography, logger = NULL,
  subst.doi = "doi:\\\\[\\\\1\\\\]\\\\(\\\\2\\\\)",
  subst.url = "\\\\<\\\\1\\\\>", first.line.start = "# References",
  first.line.end = "", first.line.as.separate.line = TRUE,
  make.text.before = NULL, make.text.after = NULL,
  normal.line.start = "* ", normal.line.end = "",
  between.two.lines = NULL, after.first.line = if
  (is.null(normal.line.start)) {     NULL } else {     "" }, sort = TRUE)
}
\arguments{
\item{refs}{an arbitrary collection of vectors with reference IDs}

\item{bibliography}{a bibliography data set}

\item{logger}{an optional logger function}

\item{subst.doi}{a substitute regular expression for dois: "\\1" is the doi,
"\\2" is the full link (optional, default value is supplied)}

\item{subst.url}{a substitute regular expression for urls, "\\1" is the url
(optional, default value is supplied)}

\item{first.line.start}{the starting string of the first line (optional,
default value is supplied)}

\item{first.line.end}{the ending string of the first line (optional, default
value is supplied)}

\item{first.line.as.separate.line}{should \code{first.line.start} be a
separate line (\code{TRUE}) or should it be the beginning pasted in front
of a normal line of references (\code{FALSE}) (optional, default value is
supplied)}

\item{make.text.before}{a function to which the reference id is passed and
which should return a string to be pasted before the actual reference text
(or \code{""}); \code{NULL} is supplied by default, meaning that no strings
are added}

\item{make.text.after}{a function to which the reference id is passed and
which should return a string to be pasted after the actual reference text,
(or \code{""}); \code{NULL} is supplied by default, meaning that}

\item{normal.line.start}{a string to paste at the beginning of every normal
line (optional, default value is supplied)}

\item{normal.line.end}{a string to paste at the end of every normal line
(optional, default value is supplied)}

\item{between.two.lines}{a string to be pasted as separate line between two
normal lines (optional, default value is supplied)}

\item{after.first.line}{the string to be inserted as line after the first
line (optional, default value is supplied)}

\item{sort}{should the bibliographic references be sorted?}
}
\value{
the multi-line reference text
}
\description{
Generate an Markdown formatted references.
}
