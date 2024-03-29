% File src/library/base/man/xtfrm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2023 R Core Team
% Distributed under GPL 2 or later

\name{xtfrm}
\alias{xtfrm}
\alias{xtfrm.default}
\alias{xtfrm.factor}
\alias{xtfrm.AsIs}
%% Moved to package survival.
%% \alias{xtfrm.Surv}
\alias{xtfrm.Date}
\alias{xtfrm.POSIXct}
\alias{xtfrm.POSIXlt}
\alias{xtfrm.difftime}
\alias{xtfrm.data.frame}
\title{Auxiliary Function for Sorting and Ranking}
\description{
  A generic auxiliary function that produces a numeric vector which
  will sort in the same order as \code{x}.
}
\usage{
xtfrm(x)
}
\arguments{
  \item{x}{an \R object.}
}
\details{
  This is a special case of ranking, but as a less general function than
  \code{\link{rank}} is more suitable to be made generic.  The default
  method is similar to \code{rank(x, ties.method = "min",
    na.last = "keep")}, so \code{NA} values are given rank \code{NA} and all
  tied values are given equal integer rank.

  The \code{\link{factor}} method extracts the codes.
  %% The \code{\link[survival]{Surv}} method sorts first on times and
  %% then on status code(s), finally on \code{timme2} if present.  Note
  %% that with the conventional status codes this sorts individuals
  %% still alive before deaths.

  The default method will unclass the object if
  \code{\link{is.numeric}(x)} is true but otherwise make use of
  \code{==} and \code{>} methods for the class of \code{x[i]} (for
  integers \code{i}), and the \code{is.na} method for the class of
  \code{x}, but might be rather slow when doing so.

  This is an \link{internal generic} \link{primitive}, so S3 or S4
  methods can be written for it.
  Differently to other internal generics, the default method is called
  explicitly when no other dispatch has happened.
}
\value{
  A numeric (usually integer) vector of the same length as \code{x}.
}
\seealso{
  \code{\link{rank}}, \code{\link{sort}}, \code{\link{order}}.
}
\keyword{univar}
\keyword{manip}
