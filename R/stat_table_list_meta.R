
#' @title `stat_table_list` Metadata
#' @description
#' Handle the metadata of a `stat_table_list`.
#' @name stat_table_list_meta
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_meta_name",
#'   "stat_table_list_meta"
#' )
stat_table_list_meta_name <- function() {
  # @codedoc_comment_block stabli::stat_table_list_meta_name
  # Get name of `stat_table_list` attribute containing its metadata.
  # @codedoc_comment_block stabli::stat_table_list_meta_name
  "stat_table_list_meta"
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_meta_default",
#'   "stat_table_list_meta"
#' )
stat_table_list_meta_default <- function() {
  # @codedoc_comment_block stabli::stat_table_list_meta_default
  # Get default metadata object for a `stat_table_list` object.
  # @codedoc_comment_block stabli::stat_table_list_meta_default
  list()
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_meta_assert",
#'   "stat_table_list_meta"
#' )
stat_table_list_meta_assert <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  stat_table_list = NULL
) {
  #' @param x_nm See `[dbc::handle_args_inplace]`.
  #' @param call See `[dbc::handle_args_inplace]`.
  #' @param assertion_type See `[dbc::handle_args_inplace]`.
  dbc::handle_args_inplace()
  # @codedoc_comment_block stabli::stat_table_list_meta_assert
  # The metadata object of a `stat_table_list` must pass these checks:
  #
  # - Must be a named `list` object.
  # @codedoc_comment_block stabli::stat_table_list_meta_assert
  dbc::assert_is_uniquely_named_list(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  if ("meta_dt" %in% names(x)) {
    # @codedoc_comment_block stabli::stat_table_list_meta_assert
    # - If the metadata object contains element `meta_dt`, it must be a
    #   `data.table` object with as many rows as the `stat_table_list` object
    #   has elements.
    # @codedoc_comment_block stabli::stat_table_list_meta_assert
    dbc::assert_is_data_table(
      x = x[["meta_dt"]],
      x_nm = sprintf("%s[[\"meta_dt\"]]", x_nm),
      call = call,
      assertion_type = assertion_type
    )
    #' @param stat_table_list `[NULL, integer]` (default `NULL`)
    #'
    #' Check `nrow(x[["meta_dt"]])` against this value if given where
    #' `x` is the metadata object of a `stat_table_list`.
    #'
    #' - `NULL`: No check is performed.
    #' - `integer`: This is checked against.
    if (!is.null(stat_table_list)) {
      dbc::assert_is_identical(
        x = length(stat_table_list),
        y = nrow(x[["meta_dt"]]),
        y_nm = sprintf("nrow(%s[[\"meta_dt\"]])", x_nm),
        call = call,
        assertion_type = assertion_type
      )
    }
  }
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_meta_get",
#'   "stat_table_list_meta"
#' )
stat_table_list_meta_get <- function(x) {
  # @codedoc_comment_block stabli::stat_table_list_meta_get
  # Get `stat_table_list` metadata.
  # @codedoc_comment_block stabli::stat_table_list_meta_get
  #' @param x `[stat_table_list]` (no default)
  #'
  #' A `stat_table_list` object.
  data.table::copy(attr(x, stat_table_list_meta_name()))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_meta_set",
#'   "stat_table_list_meta"
#' )
stat_table_list_meta_set <- function(x, meta) {
  # @codedoc_comment_block stabli::stat_table_list_meta_set
  # Set (assign) `stat_table_list` metadata.
  # @codedoc_comment_block stabli::stat_table_list_meta_set
  #' @param meta `[list]` (no default)
  #'
  #' Metadata object for a `stat_table_list`. See what
  #' `stabli::stat_table_list_meta_assert` requires it to look like.
  stat_table_list_meta_assert(meta, stat_table_list = x)
  data.table::setattr(x, stat_table_list_meta_name(), meta)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_class_name",
#'   "stat_table_list_meta"
#' )
stat_table_list_class_name <- function() {
  # @codedoc_comment_block stabli::stat_table_list_class_name
  # Get name of `stat_table_list` class. Returns
  # `${deparse1(stabli::stat_table_list_class_name())}`.
  # @codedoc_comment_block stabli::stat_table_list_class_name
  "stat_table_list"
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_class_set",
#'   "stat_table_list_meta"
#' )
stat_table_list_class_set <- function(x) {
  # @codedoc_comment_block stabli::stat_table_list_class_set
  # Set class of a `stat_table_list` object.
  # @codedoc_comment_block stabli::stat_table_list_class_set
  data.table::setattr(x, "class", c(stat_table_list_class_name(), "list"))
}
