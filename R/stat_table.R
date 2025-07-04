#' @title `stat_table` Objects
#' @description
#' Class `stat_table` methods and functions.
#' @param x
#' - `stat_table`, `as.stat_table`, `stat_table_set`:
#'   `[data.table]` (mandatory, no default)
#'   object to turn into a `stat_table`
#' - other functions: `[stat_table]` (mandatory, no default)
#' @name stat_table
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_set",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::stat_table_set
#' st <- data.table::data.table(a = 1:3, v = 3:1)
#' stabli::stat_table_set(
#'   st,
#'   list(stratum_col_nms = "a", value_col_nms = "v")
#' )
#' stopifnot(identical(
#'   stabli::stat_table_meta_get(st)[["stratum_col_nms"]],
#'   "a"
#' ))
stat_table_set <- function(
  x,
  meta = NULL
) {
  # @codedoc_comment_block stabli::stat_table_set
  # Make a `data.table` into a `stat_table` without taking a copy.
  # @codedoc_comment_block stabli::stat_table_set
  dbc::assert_is_data_table(x)
  stat_table_class_set(x)
  #' @param meta `[NULL, list]` (default `NULL`)
  #'
  #' List of metadata for a `stat_table`. See documentation for
  #' `stabli::stat_table_meta_set` for its definition.
  stat_table_meta_set(x, meta)
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::stat_table
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
stat_table <- function(
  x,
  meta = NULL
) {
  # @codedoc_comment_block stabli::stat_table
  # Make a `data.table` into a `stat_table` by taking a copy.
  # @codedoc_comment_block stabli::stat_table
  x <- data.table::copy(x)
  stat_table_set(x, meta)
  x[]
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_assert",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::stat_table_assert
#' st <- stabli::stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' stat_table_assert(st)
stat_table_assert <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  #' @param x_nm See `[dbc::handle_args_inplace]`.
  #' @param call See `[dbc::handle_args_inplace]`.
  #' @param assertion_type See `[dbc::handle_args_inplace]`.
  dbc::handle_args_inplace()
  # @codedoc_comment_block stabli::stat_table_assert
  # Assert that an object is a proper `stat_table` object. A `stat_table` must
  # pass these checks:
  #
  # - Must be of class `$(stat_table_class_name()}`.
  # @codedoc_comment_block stabli::stat_table_assert
  dbc::assert_has_class(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_class = stat_table_class_name()
  )
  # @codedoc_comment_block stabli::stat_table_assert
  # - Must have metadata that passes `stabli::stat_table_meta_assert`.
  # @codedoc_comment_block stabli::stat_table_assert
  stat_table_meta_assert(
    x = stat_table_meta_get(x),
    x_nm = sprintf("stat_table_meta_get(%s)", x_nm),
    call = call,
    assertion_type = assertion_type,
    stat_table = x
  )
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::as.stat_table",
#'   "stat_table"
#' )
as.stat_table <- function(
  x,
  meta = NULL
) {
  # @codedoc_comment_block stabli::as.stat_table
  # Make a `stat_table` object by taking a copy.
  # @codedoc_comment_block stabli::as.stat_table
  UseMethod("as.stat_table")
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::as.stat_table.stat_table",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::as.stat_table.stat_table
#' st <- stabli::as.stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' st <- stabli::as.stat_table(st)
#' stat_table_assert(st)
as.stat_table.stat_table <- function(
  x,
  meta = NULL
) {
  # @codedoc_comment_block stabli::as.stat_table.stat_table
  # Make a `stat_table` from another `stat_table` object by taking a copy.
  # If `is.null(meta)`, use the metadata from `x`.
  # @codedoc_comment_block stabli::as.stat_table.stat_table
  out <- data.table::as.data.table(x)
  if (is.null(meta)) {
    meta <- stabli::stat_table_meta_get(x)
  }
  stabli::stat_table_set(
    x = out,
    meta = meta
  )
  return(out)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::as.stat_table.data.table",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::as.stat_table.data.table
#' st <- stabli::as.stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' stat_table_assert(st)
as.stat_table.data.table <- function(x, meta) {
  # @codedoc_comment_block stabli::as.stat_table.stat_table
  # Make a `stat_table` from a `data.table` object by taking a copy.
  # @codedoc_comment_block stabli::as.stat_table.stat_table
  out <- data.table::as.data.table(x)
  stat_table_set(
    x = out,
    meta = meta
  )
  return(out)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::print.stat_table",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::print.stat_table
#' st <- stabli::as.stat_table(
#'   data.table::data.table(a = 1:3, v = 3:1),
#'   list(
#'     stratum_col_nms = "a",
#'     value_col_nms = "v"
#'   )
#' )
#' print(st)
print.stat_table <- function(x, ...) {
  # @codedoc_comment_block stabli::print.stat_table
  # `print` method for class `stat_table`. Passes additional arguments via
  # `...` to the next method, i.e. `print.data.table`.
  # @codedoc_comment_block stabli::print.stat_table
  if (!data.table::shouldPrint(x)) {
    # should not print in this instance at least: x[, "my_col" := value]
    return(invisible(NULL))
  }
  meta <- stabli::stat_table_meta_get(x)
  meta <- lapply(meta, intersect, x = names(x))
  cat("*** stat_table object ***\n")
  lapply(names(meta), function(meta_nm) {
    cat(sprintf("* %s = %s\n", meta_nm, deparse1(meta[[meta_nm]])))
  })
  NextMethod()
}

#' @eval codedoc::pkg_doc_fun(
#'   "\\Qstabli::[.stat_table\\E",
#'   "stat_table"
#' )
#' @param ... passed to next method (see `?"["`)
#' @examples
#'
#' # stabli::`[.stat_table`
#' dt <- data.table::CJ(sex = 1:2, agegroup = 1:18)
#' dt[, "n" := sample(1e3L, .N)]
#' stabli::stat_table_set(
#'   dt,
#'   meta = list(
#'     stratum_col_nms = c("sex", "agegroup"),
#'     value_col_nms = "n"
#'   )
#' )
#'
#' stopifnot(
#'   inherits(dt[1:3, ], class(dt)[1]),
#'   dt[1:3, sum(.SD[[1]]), .SDcols = "sex"] == 3
#' )
"[.stat_table" <- function(x, ...) {
  # @codedoc_comment_block stabli::[.stat_table
  # Subsetting / extraction method for class `stat_table`.
  # @codedoc_comment_block stabli::[.stat_table
  y <- NextMethod()
  if (is.data.frame(y)) {
    stabli::stat_table_meta_set(y, stabli::stat_table_meta_get(x))
  }
  return(y)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_setnames",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::stat_table_setnames
#' dt <- data.table::CJ(sex = 1:2, agegroup = 1:18)
#' dt[, "n" := sample(1e3L, .N)]
#' stabli::stat_table_set(
#'   dt,
#'   meta = list(
#'     stratum_col_nms = c("sex", "agegroup"),
#'     value_col_nms = "n"
#'   )
#' )
#' stabli::stat_table_setnames(dt, c("sex", "n"), c("my_sex", "my_n"))
#' stopifnot(
#'   c("my_sex", "my_n") %in% names(dt),
#'   "my_sex" %in% stabli::stat_table_meta_get(dt)[["stratum_col_nms"]],
#'   "my_n" %in% stabli::stat_table_meta_get(dt)[["value_col_nms"]]
#' )
stat_table_setnames <- function(
  x,
  old,
  new,
  skip_absent = NULL
) {
  # @codedoc_comment_block stabli::stat_table_setnames
  # `stabli::stat_table_setnames` changes column names and updates `stat_table`
  # metadata to reflect the changes.
  # @codedoc_comment_block stabli::stat_table_setnames
  if (is.null(skip_absent)) {
    skip_absent <- formals(data.table::setnames)[["skip_absent"]]
  }
  dbc::assert_is_data_table(x)
  nm_dt <- data.table::data.table(
    old = names(x)
  )
  #' @param new `[character]` (no default)
  #'
  #' See `[data.table::setnames]`.
  #' @param old `[character]` (no default)
  #'
  #' See `[data.table::setnames]`.
  #' @param skip_absent `[NULL, logical]` (default `NULL`)
  #'
  #' See `[data.table::setnames]`.
  #' - `NULL`: Use default defined in `[data.table::setnames]`.
  #' - `logical`: Use this.
  meta <- stat_table_meta_get(x)
  data.table::setnames(x = x, old = old, new = new, skip_absent = skip_absent)
  #' @importFrom data.table :=
  nm_dt[j = "new" := names(x)]
  nm_dt <- nm_dt[nm_dt[["old"]] != nm_dt[["new"]], ]
  meta <- lapply(meta, function(col_nm_set) {
    dt <- data.table::setDT(list(col_nm = col_nm_set))
    i.new <- NULL # for R CMD CHECK.
    #' @importFrom data.table :=
    dt[
      i = nm_dt,
      on = c(col_nm = "old"),
      j = "col_nm" := i.new
    ]
    dt[["col_nm"]]
  })
  stabli::stat_table_meta_set(x = x, meta = meta)
  return(x[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::rbind.stat_table",
#'   "stat_table"
#' )
#' @examples
#'
#' # stabli::rbind.stat_table
#' st1 <- stabli::stat_table(data.table::data.table(
#'   a = 1:3,
#'   v = 3:1
#' ), list(stratum_col_nms = "a", value_col_nms = "v"))
#' st2 <- stabli::stat_table(data.table::data.table(
#'   b = 1:3,
#'   v = 3:1
#' ), list(stratum_col_nms = "b", value_col_nms = "v"))
#' big_st <- rbind(st1, st2, use.names = TRUE, fill = TRUE)
#' stopifnot(
#'   nrow(big_st) == nrow(st1) + nrow(st2),
#'   identical(stabli::stat_table_meta_get(big_st)[["stratum_col_nms"]],
#'             c("a", "b"))
#' )
rbind.stat_table <- function(...) {
  # @codedoc_comment_block stabli::rbind.stat_table
  # `rbind` method for class `stat_table`. Performs the following steps:
  #
  # -
  # @codedoc_comment_block stabli::rbind.stat_table
  al <- list(...)
  dt_indices <- which(vapply(al, inherits, logical(1L), what = "data.table"))
  small_metas <- lapply(dt_indices, function(dt_index) {
    stabli::stat_table_assert(
      al[[dt_index]],
      x_nm = sprintf("list(...)[[%i]]", dt_index)
    )
    stabli::stat_table_meta_get(al[[dt_index]])
  })
  meta_nms <- unique(unlist(lapply(small_metas, names)))
  big_meta <- lapply(meta_nms, function(meta_nm) {
    unique(unlist(lapply(small_metas, function(small_meta) {
      if (meta_nm %in% names(small_meta)) {
        return(small_meta[[meta_nm]])
      } else {
        return(NULL)
      }
    })))
  })
  names(big_meta) <- meta_nms
  on.exit({
    lapply(al[dt_indices], function(dt) {
      stabli::stat_table_class_set(dt)
    })
  })
  lapply(al[dt_indices], function(dt) {
    data.table::setattr(
      dt,
      "class",
      setdiff(class(dt), stabli::stat_table_class_name())
    )
    NULL
  })
  out <- do.call(rbind, al)
  stabli::stat_table_set(out, meta = big_meta)
  return(out[])
}
