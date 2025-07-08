#' @title Make `stat_table`
#' @description
#' Functions to make it easy to write functions which produce `stat_table`
#' objects.
#' @name stat_table_make
NULL

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::stat_table_make_from_expr",
#'   "stat_table_make"
#' )
#' @examples
#'
#' # stabli::stat_table_make_from_expr
#' my_stat_fun_1 <- function(
#'   x,
#'   by = NULL,
#'   subset = NULL,
#'   by_style = NULL
#' ) {
#'   out <- stabli:::stat_table_make_from_expr(
#'     expr = quote(list(n = .N, mu = mean(b))),
#'     meta_expr = quote(list(
#'       stratum_col_nms = as.character(names(by)),
#'       value_col_nms = c("n", "mu")
#'     )),
#'     dataset_nm = "x"
#'   )
#'   return(out[])
#' }
#' 
#' my_stat_fun_2 <- function(
#'   x,
#'   by = NULL,
#'   subset = NULL,
#'   by_style = NULL
#' ) {
#'   out <- stabli:::stat_table_make_from_expr(
#'     expr = quote({
#'       dt <- data.table::data.table(
#'         interval_no = 1:5,
#'         time_lo = 0:4,
#'         time_up = 1:5,
#'         n = NA_integer_,
#'         d = NA_integer_
#'       )
#'       if (.N > 0) {
#'         data.table::set(
#'           dt,
#'           j = c("n", "d"),
#'           value = list(
#'             n = sample(size = 5, x = 10L, replace = TRUE),
#'             d = sample(size = 5, x = 3L, replace = TRUE)
#'           )
#'         )
#'       }
#'       dt[]
#'     }),
#'     meta_expr = quote(list(
#'       stratum_col_nms = c(names(by), "interval_no"),
#'       value_col_nms = c("n", "d")
#'     )),
#'     dataset_nm = "x"
#'   )
#'   return(out[])
#' }
#'
#' # If you don't want subsetting feature for some reason
#' my_stat_fun_3 <- function(
#'   x,
#'   by = NULL
#' ) {
#'   subset <- NULL
#'   subset_style <- NULL
#'   out <- stabli:::stat_table_make_from_expr(
#'     expr = quote(list(n = .N, mu = mean(b))),
#'     meta_expr = quote(list(
#'       stratum_col_nms = as.character(names(by)),
#'       value_col_nms = c("n", "mu")
#'     )),
#'     dataset_nm = "x"
#'   )
#'   return(out[])
#' }
#' 
#' my_dataset <- data.table::data.table(
#'   a = sample(1:5, size = 1e6L, replace = TRUE)
#' )
#' my_dataset[j = "b" := a + runif(n = 1e6L)]
#' 
#' st_1 <- my_stat_fun_1(
#'   x = my_dataset,
#'   by = "a",
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_1) == 5)
#' 
#' st_2 <- my_stat_fun_2(
#'   x = my_dataset,
#'   by = "a",
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_2) == 5 * 5)
#' 
#' st_3 <- my_stat_fun_2(
#'   x = my_dataset,
#'   by = NULL,
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_3) == 5)
#' 
#' st_4 <- my_stat_fun_1(
#'   x = my_dataset,
#'   by = NULL,
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_4) == 1)
#' 
#' st_5 <- my_stat_fun_1(
#'   x = my_dataset,
#'   by = list(a = 1:6),
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_5) == 6)
#' 
#' st_6 <- my_stat_fun_2(
#'   x = my_dataset,
#'   by = list(a = 1:6),
#'   subset = data.table::data.table(a = 3:5),
#'   by_style = "keep_empty"
#' )
#' stopifnot(nrow(st_6) == 6 * 5)
#'
#' st_7 <- my_stat_fun_3(
#'   x = my_dataset,
#'   by = "a"
#' )
#' stopifnot(nrow(st_7) == 5)
stat_table_make_from_expr <- function(
  expr,
  meta_expr = NULL,
  dataset_nm = "dataset",
  arg_by_nm = "by",
  arg_subset_nm = "subset",
  arg_by_style_nm = "by_style",
  eval_env = NULL,
  calling_env = NULL
) {
  # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.3.0")
  # New function `stabli::stat_table_make_from_expr`.
  # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.3.0")
  #' @param expr `[name, call]` (no default)
  #'
  #' An R expression to evaluate for one stratum in your dataset.
  #' E.g. `quote(list(n = .N, mu = mean(my_col)))`.
  dbc::assert_has_one_of_classes(
    expr,
    classes = c("name", "call", "{"),
    assertion_type = "prod_input"
  )
  #' @param meta_expr `[NULL, name, call]` (default `NULL`)
  #'
  #' An R expression to evaluate which produces the `stat_table` metadata
  #' object. `NULL` causes an internally defined default to be used.
  dbc::assert_has_one_of_classes(
    meta_expr,
    classes = c("NULL", "name", "call", "{"),
    assertion_type = "prod_input"
  )
  if (is.null(eval_env)) {
    eval_env <- parent.frame(1L)
  }
  if (is.null(calling_env)) {
    calling_env <- parent.frame(2L)
  }
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  # `stabli::stat_table_make_from_expr` is a general solution for writing
  # functions to produce `stat_table` objects. You can essentially write
  # a wrapper for this function with a custom `expr` to evaluate in every
  # stratum which then returns a `stat_table` object. The following steps are
  # performed:
  #
  # - `stabli::handle_arg_by_et_subset_et_by_style_inplace` is called.
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  stabli::handle_arg_by_et_subset_et_by_style_inplace(
    #' @param dataset_nm See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    dataset_nm = dataset_nm,
    #' @param arg_by_nm See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    arg_by_nm = arg_by_nm,
    #' @param arg_subset_nm See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    arg_subset_nm = arg_subset_nm,
    #' @param arg_by_style_nm See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    arg_by_style_nm = arg_by_style_nm,
    #' @param eval_env See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    eval_env = eval_env,
    #' @param calling_env See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    calling_env = calling_env
  )
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  # - A `data.table` expression is generated based on whether stratification
  #   and / or subsetting is necessary. E.g. `dataset[i = subset, j = .N]`,
  #   if `expr = quote(.N)`.
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  #' @importFrom data.table .SD .EACHI
  dt_expr <- substitute(
    DATASET[
      i = SUBSET,
      j = .SD[
        i = BY,
        on = names(BY),
        j = EXPR,
        keyby = .EACHI
      ],
      .SDcols = SDCOLS
    ],
    list(
      SDCOLS = local({
        sd_cols <- intersect(names(eval_env[[dataset_nm]]), union(
          names(eval_env[[arg_by_nm]]),
          all.names(expr, functions = FALSE)
        ))
        if (length(sd_cols) == 0) {
          sd_cols <- names(eval_env[[dataset_nm]])
        }
        sd_cols
      }),
      SUBSET = parse(text = arg_subset_nm)[[1]],
      BY = parse(text = arg_by_nm)[[1]],
      DATASET = parse(text = dataset_nm)[[1]],
      EXPR = expr
    )
  )
  if (inherits(eval_env[[arg_subset_nm]], "data.table")) {
    dt_expr[["on"]] <- names(eval_env[[arg_subset_nm]])
  }
  if (is.null(eval_env[[arg_by_nm]])) {
    dt_expr[["j"]][c("i", "on", "keyby")] <- NULL
  } else {
    dt_expr[["j"]][["nomatch"]] <- switch(
      eval_env[[arg_by_style_nm]],
      "keep_empty" = NA,
      "drop_empty" = 0L
    )
  }
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  # - The expression is evaluated in `eval_env`.
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  out <- eval(dt_expr, envir = eval_env)

  # @codedoc_comment_block stabli::stat_table_make_from_expr
  # - The result is at this point a `data.table`. The `stat_table` metadata is
  #   evaluated next. By default `stratum_col_nms` and `value_col_nms`
  #   are inferred from `by` and the column names of the result at this point.
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  if (is.null(meta_expr)) {
    meta <- list(
      # as.character: in case by is NULL
      stratum_col_nms = as.character(names(eval_env[[arg_by_nm]])),
      value_col_nms = setdiff(names(out), names(eval_env[[arg_by_nm]]))
    )
  } else {
    # @codedoc_comment_block stabli::stat_table_make_from_expr
    #   Non-default `meta_expr` is evaluated in `eval_env`.
    # @codedoc_comment_block stabli::stat_table_make_from_expr
    meta <- eval(meta_expr, envir = eval_env)
  }
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  # - `stabli::stat_table_set` is called on the result with the collected
  #   metadata. The resulting `stat_table` is returned.
  # @codedoc_comment_block stabli::stat_table_make_from_expr
  stabli::stat_table_set(
    out,
    meta = meta
  )
  return(out[])
}
