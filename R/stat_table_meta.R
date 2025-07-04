#' @title `stat_table` Metadata
#' @description
#' Handle the metadata of a `stat_table`.
#' @name stat_table_meta
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_meta_name",
#'   "stat_table_meta"
#' )
stat_table_meta_name <- function() {
  # @codedoc_comment_block stabli::stat_table_meta_name
  # Get name of `stat_table` attribute containing metadata.
  # @codedoc_comment_block stabli::stat_table_meta_name
  "stat_table_meta"
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_meta_default",
#'   "stat_table_meta"
#' )
stat_table_meta_default <- function() {
  # @codedoc_comment_block stabli::stat_table_meta_default
  # Get default metadata of a `stat_table`.
  # @codedoc_comment_block stabli::stat_table_meta_default
  list(
    stratum_col_nms = character(0L),
    value_col_nms = character(0L)
  )
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_meta_get",
#'   "stat_table_meta"
#' )
#' @examples
#'
#' # stabli::stat_table_meta_get
#' st <- stabli::stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' stopifnot(identical(
#'   stabli::stat_table_meta_get(st)[["stratum_col_nms"]],
#'   "a"
#' ))
stat_table_meta_get <- function(x) {
  # @codedoc_comment_block stabli::stat_table_meta_get
  # Get metadata of a `stat_table`.
  # @codedoc_comment_block stabli::stat_table_meta_get
  #' @param x `[stat_table]` (no default)
  #'
  #' A `stat_table` object.
  lapply(attr(x, stat_table_meta_name()), intersect, y = names(x))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_meta_set",
#'   "stat_table_meta"
#' )
#' @examples
#'
#' # stabli::stat_table_meta_set
#' st <- stabli::stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1)
#' )
#' stabli::stat_table_meta_set(
#'   st,
#'   list(stratum_col_nms = "a", value_col_nms = "v")
#' )
#' stopifnot(identical(
#'   stabli::stat_table_meta_get(st)[["stratum_col_nms"]],
#'   "a"
#' ))
stat_table_meta_set <- function(x, meta) {
  # @codedoc_comment_block stabli::stat_table_meta_set
  # Set (assign) metadata of a `stat_table`.
  # @codedoc_comment_block stabli::stat_table_meta_set
  #' @param meta `[list]` (no default)
  #'
  #' List of metadata to save into `stat_table` object.
  if (is.null(meta)) {
    meta <- stabli::stat_table_meta_default()
  } else {
    stat_table_meta_assert(x = meta, stat_table = x)
  }
  data.table::setattr(x, name = stat_table_meta_name(), value = meta)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_meta_assert",
#'   "stat_table_meta"
#' )
#' @examples
#'
#' # stabli::stat_table_meta_assert
#' stabli::stat_table_meta_assert(
#'   list(stratum_col_nms = "a", value_col_nms = "v")
#' )
#' st <- stabli::stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' stabli::stat_table_meta_assert(
#'   list(stratum_col_nms = "a", value_col_nms = "v"),
#'   stat_table = st
#' )
stat_table_meta_assert <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  stat_table = NULL
) {
  #' @param x_nm See `[dbc::handle_args_inplace]`.
  #' @param call See `[dbc::handle_args_inplace]`.
  #' @param assertion_type See `[dbc::handle_args_inplace]`.
  dbc::handle_args_inplace()

  # @codedoc_comment_block stabli::stat_table_meta_assert
  # Assert that an object is a proper metadata object for a `stat_table`.
  # Such an object must pass the following checks:
  #
  # - Is a `list`.
  # @codedoc_comment_block stabli::stat_table_meta_assert
  dbc::assert_is_list(
    x,
    x_nm = NULL,
    call = NULL,
    assertion_type = NULL
  )
  # @codedoc_comment_block stabli::stat_table_meta_assert
  # - Has all elements with correct classes that
  #   `stabli::stat_table_meta_default()` returns.
  # @codedoc_comment_block stabli::stat_table_meta_assert
  default <- stabli::stat_table_meta_default()
  dbc::assert_has_names(x, required_names = names(default))
  main_call <- match.call()
  lapply(names(default), function(meta_nm) {
    dbc::assert_has_class(
      x = x[[meta_nm]],
      required_class = class(default[[meta_nm]]),
      x_nm = sprintf("%s[[\"%s\"]]", x_nm, meta_nm),
      call = main_call,
      assertion_type = assertion_type
    )
    dbc::assert_is_nonNA(
      x = x[[meta_nm]],
      x_nm = sprintf("%s[[\"%s\"]]", x_nm, meta_nm),
      call = main_call,
      assertion_type = assertion_type
    )
    # @codedoc_comment_block stabli::stat_table_meta_assert
    # - Metadata `stratum_col_nms` and `value_col_nms` must be names of columns
    #   in the `stat_table` object. This is checked if `!is.null(stat_table)`.
    # @codedoc_comment_block stabli::stat_table_meta_assert
    col_nm_meta_nm_set <- c("stratum_col_nms", "value_col_nms")
    #' @param stat_table `[NULL, stat_table]` (default `NULL`)
    #'
    #' - `NULL`: No additional check.
    #' - `stat_table`: Check that `stratum_col_nms` and `value_col_nms` are
    #'   column names of `stat_table`.
    if (!is.null(stat_table) && meta_nm %in% col_nm_meta_nm_set) {
      dbc::assert_vector_elems_are_in_set(
        x = x[["stratum_col_nms"]],
        set = names(stat_table),
        x_nm = sprintf("%s[[\"%s\"]]", x_nm, meta_nm),
        call = main_call,
        assertion_type = assertion_type
      )
    }
  })
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_class_name",
#'   "stat_table_meta"
#' )
stat_table_class_name <- function() {
  # @codedoc_comment_block stabli::stat_table_class_name
  # Get name of `stat_table` class, i.e. returns
  # `${deparse1(stabli::stat_table_class_name())}`.
  # @codedoc_comment_block stabli::stat_table_class_name
  "stat_table"
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_class_set",
#'   "stat_table_meta"
#' )
stat_table_class_set <- function(x) {
  # @codedoc_comment_block stabli::stat_table_class_set
  # Set an object in place to have class
  # `${deparse1(stabli::stat_table_class_name())}`.
  # @codedoc_comment_block stabli::stat_table_class_set
  dbc::assert_is_data_table(x)
  data.table::setattr(
    x,
    "class",
    union(stat_table_class_name(), class(x))
  )
}
