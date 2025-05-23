% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_eqlength.R
\name{check_eqlength}
\alias{check_eqlength}
\title{Check if input arguments have the same length}
\usage{
check_eqlength(..., keepgoing = FALSE)
}
\arguments{
\item{...}{Input arguments of unknown length.}

\item{keepgoing}{A logical value specifying if the function
should continue or terminate upon finding input arguments of
non-equal length. The default, \code{FALSE}, produces an
error and terminates the function.}
}
\value{
\code{TRUE} or \code{FALSE} depending on the result
of the test. Nothing is returned if the stop function is invoked.
}
\description{
Calculate the length of all input arguments to see if they
are equal. Entries that are \code{NULL}, and thus, have
a length of zero are ignored. An optional trigger to
\code{\link[=stop]{stop()}} is provided with a tailored error message.
}
\examples{
\dontshow{
testthat::expect_error(ss3sim:::check_eqlength(),
  regex = "At least",
  label = "check_eqlength didn't error with zero input args"
)
testthat::expect_error(ss3sim:::check_eqlength(yes = 1:2, no = 3:5),
  regex = "Not all.+yes",
  label = "check_eqlength didn't error with unequal-lengthed vectors"
)
testthat::expect_true(ss3sim:::check_eqlength(yes = 1:2, no = 3:4),
  label = "check_eqlength didn't return TRUE."
)
testthat::expect_false(ss3sim:::check_eqlength(yes = 1:2, no = 3:5, keepgoing = TRUE),
  label = "check_eqlength didn't return TRUE."
)
testthat::expect_true(ss3sim:::check_eqlength(yes = NULL, no = NULL),
  label = "check-eqlength didn't return TRUE for all NULLs"
)
}

}
\author{
Kelli F. Johnson
}
