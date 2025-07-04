% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_table_meta.R
\name{stat_table_meta}
\alias{stat_table_meta}
\alias{stat_table_meta_name}
\alias{stat_table_meta_default}
\alias{stat_table_meta_get}
\alias{stat_table_meta_set}
\alias{stat_table_meta_assert}
\alias{stat_table_class_name}
\alias{stat_table_class_set}
\title{\code{stat_table} Metadata}
\usage{
stat_table_meta_name()

stat_table_meta_default()

stat_table_meta_get(x)

stat_table_meta_set(x, meta)

stat_table_meta_assert(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  stat_table = NULL
)

stat_table_class_name()

stat_table_class_set(x)
}
\arguments{
\item{x}{\verb{[stat_table]} (no default)

A \code{stat_table} object.}

\item{meta}{\verb{[list]} (no default)

List of metadata to save into \code{stat_table} object.}

\item{x_nm}{See \verb{[dbc::handle_args_inplace]}.}

\item{call}{See \verb{[dbc::handle_args_inplace]}.}

\item{assertion_type}{See \verb{[dbc::handle_args_inplace]}.}

\item{stat_table}{\verb{[NULL, stat_table]} (default \code{NULL})
\itemize{
\item \code{NULL}: No additional check.
\item \code{stat_table}: Check that \code{stratum_col_nms} and \code{value_col_nms} are
column names of \code{stat_table}.
}}
}
\description{
Handle the metadata of a \code{stat_table}.
}
\section{Functions}{

\strong{stabli::stat_table_meta_name}

Get name of \code{stat_table} attribute containing metadata.


\strong{stabli::stat_table_meta_default}

Get default metadata of a \code{stat_table}.


\strong{stabli::stat_table_meta_get}

Get metadata of a \code{stat_table}.


\strong{stabli::stat_table_meta_set}

Set (assign) metadata of a \code{stat_table}.


\strong{stabli::stat_table_meta_assert}

Assert that an object is a proper metadata object for a \code{stat_table}.
Such an object must pass the following checks:
\itemize{
\item Is a \code{list}.
\item Has all elements with correct classes that
\code{stabli::stat_table_meta_default()} returns.
\item Metadata \code{stratum_col_nms} and \code{value_col_nms} must be names of columns
in the \code{stat_table} object. This is checked if \code{!is.null(stat_table)}.
}


\strong{stabli::stat_table_class_name}

Get name of \code{stat_table} class, i.e. returns
\code{"stat_table"}.


\strong{stabli::stat_table_class_set}

Set an object in place to have class
\code{"stat_table"}.
}

\examples{

# stabli::stat_table_meta_get
st <- stabli::stat_table(
  data.table::data.table(a = 1:3, v = 3:1),
  list(
    stratum_col_nms = "a",
    value_col_nms = "v"
  )
)
stopifnot(identical(
  stabli::stat_table_meta_get(st)[["stratum_col_nms"]],
  "a"
))

# stabli::stat_table_meta_set
st <- stabli::stat_table(
  data.table::data.table(a = 1:3, v = 3:1)
)
stabli::stat_table_meta_set(
  st,
  list(stratum_col_nms = "a", value_col_nms = "v")
)
stopifnot(identical(
  stabli::stat_table_meta_get(st)[["stratum_col_nms"]],
  "a"
))

# stabli::stat_table_meta_assert
stabli::stat_table_meta_assert(
  list(stratum_col_nms = "a", value_col_nms = "v")
)
st <- stabli::stat_table(
  data.table::data.table(a = 1:3, v = 3:1),
  list(
    stratum_col_nms = "a",
    value_col_nms = "v"
  )
)
stabli::stat_table_meta_assert(
  list(stratum_col_nms = "a", value_col_nms = "v"),
  stat_table = st
)
}
