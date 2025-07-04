#' @title List of Statistics Tables
#' @description 
#' A convenient list of `stat_table` objects.
#' @name stat_table_list
NULL

#' @eval codedoc::pkg_doc_fun("stabli::stat_table_list", "stat_table_list")
stat_table_list <- function(
  x,
  meta = NULL
) {
  # @codedoc_comment_block stabli::stat_table_list
  # Create a `stat_table_list` object by taking a copy.
  # @codedoc_comment_block stabli::stat_table_list

  # @codedoc_comment_block examples(stabli::stat_table_list)
  # @codedoc_comment_block R_package_example(stabli)
  # st1 <- stabli::stat_table(
  #   data.table::data.table(a = 1:3, v = 3:1),
  #   list(
  #     stratum_col_nms = "a",
  #     value_col_nms = "v"
  #   )
  # )
  # st2 <- stabli::stat_table(
  #   data.table::data.table(b = 1:3, v = 3:1),
  #   list(
  #     stratum_col_nms = "b",
  #     value_col_nms = "v"
  #   )
  # )
  # stl <- stabli::stat_table_list(
  #   list(tab1 = st1, tab2 = st2),
  #   list(meta_dt = data.table::data.table(stat_fun_nm = c("fun1", "fun1")))
  # )
  # @codedoc_comment_block R_package_example(stabli)
  # @codedoc_comment_block examples(stabli::stat_table_list)
  #' @param x `[list]` (no default)
  #'
  #' A named `list` of `stat_table` objects.
  dbc::assert_has_class(x, required_class = "list")
  dbc::assert_is_uniquely_named(x)
  lapply(seq_along(x), function(i) {
    dbc::assert_has_class(
      x[[i]],
      required_class = "stat_table"
    )
  })
  #' @param meta `[NULL, list]` (no default)
  #'
  #' List of metadata as accepted by `stabli::stat_table_list_meta_assert`.
  #'
  #' - `NULL`: No metadata.
  #' - `list`: See what `stabli::stat_table_list_meta_assert` specifies.
  if (is.null(meta)) {
    meta <- stat_table_list_meta_default()
  } else {
    stat_table_list_meta_assert(meta)
  }
  out <- data.table::copy(x)
  stat_table_list_set(
    out,
    meta = meta
  )
  return(out)
}

#' @eval codedoc::pkg_doc_fun("stabli::as.stat_table_list", "stat_table_list")
as.stat_table_list <- function(x, meta) {
  UseMethod("as.stat_table_list")
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::as.stat_table_list.stat_table_list", 
#'   "stat_table_list"
#')
as.stat_table_list.stat_table_list <- function(x, meta = NULL) {
  out <- as.list(x)
  stat_table_list_set(x = out, meta = meta)
  return(out)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::as.stat_table_list.list", 
#'   "stat_table_list"
#')
as.stat_table_list.list <- function(x, meta = NULL) {
  out <- as.list(x)
  stat_table_list_set(
    x = out,
    meta = meta
  )
  return(out)
}

#' @eval codedoc::pkg_doc_fun("stabli::stat_table_list_set", "stat_table_list")
stat_table_list_set <- function(x, meta = NULL) {
  stat_table_list_class_set(x)
  stat_table_list_meta_set(x, meta = meta)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_list_assert",
#'   "stat_table_list"
#' )
stat_table_list_assert <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  #' @param x_nm See `[dbc::handle_args_inplace]`.
  #' @param call See `[dbc::handle_args_inplace]`.
  #' @param assertion_type See `[dbc::handle_args_inplace]`.
  dbc::handle_args_inplace()
  dbc::assert_has_class(
    x = x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    required_class = stat_table_list_class_name()
  )
  dbc::assert_is_uniquely_named(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type
  )
  stabli::stat_table_list_meta_assert(
    x = stabli::stat_table_list_meta_get(x),
    x_nm = sprintf("stabli::stat_table_list_meta_get(%s)", x_nm),
    call = call,
    assertion_type = assertion_type,
    stat_table_list = x
  )
  lapply(seq_along(x), function(i) {
    stabli::stat_table_assert(
      x = x[[i]],
      x_nm = sprintf("x[[%s]]", i),
      call = call,
      assertion_type = assertion_type
    )
  })
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "\\Qstabli::[.stat_table_list\\E",
#'   "stat_table_list"
#' )
#' @examples
#' 
#' # "[.stat_table_list"
#' stl <- stabli::stat_table_list(
#'   list(
#'     tab1 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     ),
#'     tab2 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     )
#'   )
#' )
#' stl_1 <- stl[1]
#' stopifnot(
#'   inherits(stl_1, "stat_table_list"),
#'   length(stl_1) == 1,
#'   identical(stl_1[[1]], stl[[1]])
#' )
"[.stat_table_list" <- function(x, i, ...) {
  #' @param i passed to next method, see `? "["`.
  #' @param ... passed to next method, see `? "["`.
  y <- NextMethod()
  x_meta <- stat_table_list_meta_get(x)
  y_meta <- x_meta
  if ("meta_dt" %in% names(y_meta)) {
    y_meta[["meta_dt"]] <- y_meta[["meta_dt"]][i, ]
  }
  stat_table_list_set(y, y_meta)
  return(y)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::print.stat_table_list",
#'   "stat_table_list"
#' )
print.stat_table_list <- function(x, ...) {
  # @codedoc_comment_block stabli::print.stat_table_list
  # `print` method for class `stat_table_list`. Performs the following steps:
  #
  # - Prints length of the `stat_table_list` object.
  # @codedoc_comment_block stabli::print.stat_table_list
  cat("* stat_table_list of length", length(x), "\n")
  # @codedoc_comment_block stabli::print.stat_table_list
  # - Prints a `data.table` containing
  #   + `name`: The name of the table from `names(x)`.
  # @codedoc_comment_block stabli::print.stat_table_list
  info_dt <- data.table::data.table(name = names(x))
  meta <- stabli::stat_table_list_meta_get(x)
  if ("meta_dt" %in% names(meta)) {
    # @codedoc_comment_block stabli::print.stat_table_list
    #   + Every column of `stabli::stat_table_list_meta_get(x)[["meta_dt"]]`
    #     if it exists.
    # @codedoc_comment_block stabli::print.stat_table_list
    info_dt <- cbind(info_dt, meta[["meta_dt"]])
  }
  lapply(seq_along(x), function(i) {
    st <- x[[i]]
    st_meta_list <- stat_table_meta_get(st)
    # @codedoc_comment_block stabli::print.stat_table_list
    #   + `dim`: Number of rows and columns.
    # @codedoc_comment_block stabli::print.stat_table_list
    data.table::set(
      info_dt,
      i = i,
      j = "dim",
      value = sprintf("(%i, %i)", nrow(st), ncol(st))
    )
    # @codedoc_comment_block stabli::print.stat_table_list
    #   + Every element of the metadata of the corresponding `stat_table`.
    # @codedoc_comment_block stabli::print.stat_table_list
    data.table::set(
      info_dt,
      i = i,
      j = names(st_meta_list),
      value = st_meta_list
    )
    return(info_dt)
  })
  # @codedoc_comment_block stabli::print.stat_table_list
  # - Additional arguments are passed via `...` to `print.data.table`.
  # @codedoc_comment_block stabli::print.stat_table_list
  print(info_dt, ...)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::c.stat_table_list",
#'   "stat_table_list"
#' )
#' @examples
#'
#' # c.stat_table_list
#' stl_1 <- stabli::stat_table_list(
#'   list(
#'     tab1 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     ),
#'     tab2 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     )
#'   )
#' )
#' stl_2 <- stabli::stat_table_list(
#'   list(
#'     tab3 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     ),
#'     tab4 = stabli::stat_table(
#'       data.table::data.table(a = 1:3, v = 3:1),
#'       list(
#'         stratum_col_nms = "a",
#'         value_col_nms = "v"
#'       )
#'     )
#'   )
#' )
#' big_stl <- c(stl_1, stl_2)
#' stabli::stat_table_list_assert(big_stl)
c.stat_table_list <- function(...) {
  # @codedoc_comment_block stabli::c.stat_table_list
  # Collect method for `stat_table_list` objects. Performs the following steps:
  #
  # - Turn data collected via `list(...)` into normal lists.
  # @codedoc_comment_block stabli::c.stat_table_list
  stll <- list(...)
  main_call <- match.call()
  out <- lapply(seq_along(stll), function(i) {
    stat_table_list_assert(
      x = stll[[i]],
      x_nm = sprintf("list(...)[[%i]]", i),
      call = main_call
    )
    out <- as.list(stll[[i]])
    data.table::setattr(out, "class", "list")
    return(out)
  })

  # @codedoc_comment_block stabli::c.stat_table_list
  # - Collect metadata from each element of `list(...)` into one big list of
  #   metadata. `meta_dt` are combined with `data.table::rbindlist`. Other
  #   elements are combined with `c` and a call to `unique` to drop duplicates.
  # @codedoc_comment_block stabli::c.stat_table_list
  out_meta <- local({
    mll <- lapply(stll, stat_table_list_meta_get)
    meta_nms <- unique(unlist(lapply(mll, names)))
    meta <- lapply(meta_nms, function(meta_nm) {
      one_meta_by_i <- lapply(mll, function(meta_i) {
        if (!meta_nm %in% names(meta_i)) {
          NULL
        } else {
          meta_i[[meta_nm]]
        }
      })
      if (meta_nm == "meta_dt") {
        data.table::rbindlist(one_meta_by_i)
      } else {
        unique(do.call(c, one_meta_by_i))
      }
    })
    names(meta) <- meta_nms
    meta
  })
  stat_table_list_meta_assert(
    out_meta,
    assertion_type = "prod_interim"
  )
  out <- do.call(c, out)

  # @codedoc_comment_block stabli::c.stat_table_list
  # - Call `stabli::stat_table_list_set` using collected data and metadata.
  # - Return the result, a long `stat_table_list`.
  # @codedoc_comment_block stabli::c.stat_table_list
  stabli::stat_table_list_set(out, meta = out_meta)
  return(out)
}
