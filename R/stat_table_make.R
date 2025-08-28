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
stat_table_make_from_expr <- function(
  expr,
  meta_expr = NULL,
  dataset_nm = "dataset",
  arg_by_nm = "by",
  arg_subset_nm = "subset",
  arg_by_style_nm = "by_style",
  stratification_vame = NULL,
  eval_env = NULL,
  calling_env = NULL
) {
  # @codedoc_comment_block R_package_example(stabli)
  # @codedoc_comment_block examples(stabli::stat_table_make_from_expr)
  # # stabli::stat_table_make_from_expr
  # my_stat_fun_1 <- function(
  #   x,
  #   by = NULL,
  #   subset = NULL,
  #   by_style = NULL
  # ) {
  #   out <- stabli:::stat_table_make_from_expr(
  #     expr = quote(list(n = .N, mu = mean(b))),
  #     meta_expr = quote(list(
  #       stratum_col_nms = as.character(names(by)),
  #       value_col_nms = c("n", "mu")
  #     )),
  #     dataset_nm = "x"
  #   )
  #   return(out[])
  # }
  # 
  # my_stat_fun_2 <- function(
  #   x,
  #   by = NULL,
  #   subset = NULL,
  #   by_style = NULL
  # ) {
  #   out <- stabli:::stat_table_make_from_expr(
  #     expr = quote({
  #       dt <- data.table::data.table(
  #         interval_no = 1:5,
  #         time_lo = 0:4,
  #         time_up = 1:5,
  #         n = NA_integer_,
  #         d = NA_integer_
  #       )
  #       if (.N > 0) {
  #         data.table::set(
  #           dt,
  #           j = c("n", "d"),
  #           value = list(
  #             n = sample(size = 5, x = 10L, replace = TRUE),
  #             d = sample(size = 5, x = 3L, replace = TRUE)
  #           )
  #         )
  #       }
  #       dt[]
  #     }),
  #     meta_expr = quote(list(
  #       stratum_col_nms = c(names(by), "interval_no"),
  #       value_col_nms = c("n", "d")
  #     )),
  #     dataset_nm = "x"
  #   )
  #   return(out[])
  # }
  #
  # # If you don't some args in your function
  # my_stat_fun_3 <- function(
  #   x,
  #   by = NULL
  # ) {
  #   subset <- NULL
  #   by_style <- NULL
  #   out <- stabli:::stat_table_make_from_expr(
  #     expr = quote(list(n = .N, mu = mean(b))),
  #     meta_expr = quote(list(
  #       stratum_col_nms = as.character(names(by)),
  #       value_col_nms = c("n", "mu")
  #     )),
  #     dataset_nm = "x"
  #   )
  #   return(out[])
  # }
  # 
  # my_dataset <- data.table::data.table(
  #   a = sample(1:5, size = 1e6L, replace = TRUE)
  # )
  # my_dataset[j = "b" := a + runif(n = 1e6L)]
  # 
  # st_1 <- my_stat_fun_1(
  #   x = my_dataset,
  #   by = "a",
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_1) == 5)
  # 
  # st_2 <- my_stat_fun_2(
  #   x = my_dataset,
  #   by = "a",
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_2) == 5 * 5)
  # 
  # st_3 <- my_stat_fun_2(
  #   x = my_dataset,
  #   by = NULL,
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_3) == 5)
  # 
  # st_4 <- my_stat_fun_1(
  #   x = my_dataset,
  #   by = NULL,
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_4) == 1)
  # 
  # st_5 <- my_stat_fun_1(
  #   x = my_dataset,
  #   by = list(a = 1:6),
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_5) == 6)
  # 
  # st_6 <- my_stat_fun_2(
  #   x = my_dataset,
  #   by = list(a = 1:6),
  #   subset = data.table::data.table(a = 3:5),
  #   by_style = "keep_empty"
  # )
  # stopifnot(nrow(st_6) == 6 * 5)
  #
  # st_7 <- my_stat_fun_3(
  #   x = my_dataset,
  #   by = "a"
  # )
  # stopifnot(nrow(st_7) == 5)
  # @codedoc_comment_block examples(stabli::stat_table_make_from_expr)
  # @codedoc_comment_block R_package_example(stabli)
  # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.3.0")
  # New function `stabli::stat_table_make_from_expr`.
  # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.3.0")
  #' @param expr `[name, call]` (no default)
  #'
  #' An R expression to evaluate for one stratum in your dataset.
  #' E.g. `quote(list(n = .N, mu = mean(my_col)))`.
  dbc::assert_is_unevaluated_expression(
    expr,
    assertion_type = "prod_input"
  )
  #' @param meta_expr `[NULL, name, call]` (default `NULL`)
  #'
  #' An R expression to evaluate which produces the `stat_table` metadata
  #' object. `NULL` causes an internally defined default to be used.
  dbc::assert_is_unevaluated_expression(
    meta_expr,
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
    calling_env = calling_env,
    # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.4.0")
    # `stabli::stat_table_make_from_expr` gains argument
    # `stratification_vame`.
    # @codedoc_comment_block news("stabli::stat_table_make_from_expr", "2025-07-08", "0.4.0")
    #' @param stratification_vame See `[handle_arg_by_et_subset_et_by_style_inplace]`.
    stratification_vame = stratification_vame
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

#' @eval codedoc::pkg_doc_fun(
#'   regex = "stabli::stat_table_make_from_by_list",
#'   rdname = "stat_table_make"
#' )
stat_table_make_from_by_list <- function(
  fun_nm,
  dataset,
  by_list,
  subset = NULL,
  by_style = NULL,
  arg_list = NULL,
  optional_steps = NULL,
  harmonisation_vame = NULL,
  stratification_vame = NULL
) {
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # `stabli::stat_table_make_from_by_list` performs the following steps:
  #
  # - Run `optional_steps[["on_entry"]](env = main_env)` if that `optional_steps` element
  #   exists. `main_env` is the evaluation environment of
  #   `stabli::stat_table_make_from_by_list`.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  main_env <- environment()
  #' @param optional_steps `[NULL, list]` (default `NULL`)
  #'
  #' - `NULL`: No additional optional steps are run.
  #' - `list`: This named list of functions is used to perform optional steps
  #'   during run. See detailed description of steps performed by function
  #'   to see how this can be used.
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](env = main_env)
  }
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `on.exit(optional_steps[["on_exit"]](env = main_env))` if that
  #   `optional_steps` element exists.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  if ("on_exit" %in% names(optional_steps)) {
    on.exit(optional_steps[["on_exit"]](env = main_env))
  }
  dbc::assert_is_character_nonNA_atom(fun_nm)
  dbc::assert_is_function(eval(parse(text = fun_nm)))
  dbc::assert_is_data_table(dataset)
  dbc::assert_has_one_of_classes(
    harmonisation_vame,
    classes = c("NULL", "VariableMetadata")
  )
  dbc::assert_has_one_of_classes(
    stratification_vame,
    classes = c("NULL", "VariableMetadata")
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `stabli::handle_arg_by_et_subset_et_by_style_inplace`.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  #' @param subset Handled by `[handle_arg_by_et_subset_et_by_style_inplace]`.
  #' @param by_style Handled by `[handle_arg_by_et_subset_et_by_style_inplace]`.
  stabli::handle_arg_by_et_subset_et_by_style_inplace(
    handle_arg_nms = c("subset", "by_style")
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `optional_steps[["pre_lapply"]](env = main_env)` if that `optional_steps`
  #   element exists.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  if ("pre_lapply" %in% names(optional_steps)) {
    optional_steps[["pre_lapply"]](env = main_env)
  }
  #' @param by_list `[list]` (no default)
  #'
  #' Each element is a passed in turn to function named `fun_nm`
  #' as argument `by`.
  assert_is_arg_by_list(by_list, dataset = dataset)
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `lapply` on `by_list`. For each element
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  big_stat_table <- lapply(
    by_list,
    function(by) {
      anon_fun_env <- environment()
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `optional_steps[["lapply_on_entry"]](env = anon_fun_env)`
      #       if that `optional_steps` element exists. `anon_fun_env` is the
      #       evaluation environment of the anonymous function passed to
      #       `lapply` defined in the body of
      #       `stabli::stat_table_make_from_by_list`.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      if ("lapply_on_entry" %in% names(optional_steps)) {
        optional_steps[["lapply_on_entry"]](env = anon_fun_env)
      }
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `on.exit(optional_steps[["lapply_on_exit"]](env = anon_fun_env))`
      #       if that `optional_steps` element exists.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      if ("lapply_on_exit" %in% names(optional_steps)) {
        on.exit(optional_steps[["lapply_on_exit"]](env = anon_fun_env))
      }
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Create argument list `arg_list_i` for calling function named
      #       `fun_nm`. It is created by collecting `dataset`, `subset`, `by`
      #       (current element of `by_list`), and
      #       `by_style` into a list, and by appending argument
      #       `arg_list` to it. `by` is additionally processed by
      #       `stabli::handle_arg_by`.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      arg_list_i <- local({
        arg_list_i <- mget(c("dataset", "subset", "by", "by_style"),
                           inherits = TRUE)
        #' @param stratification_vame `[VariableMetadata, NULL]` (default `NULL`)
        #'
        #' Passed to `[handle_arg_by]`.
        arg_list_i[["by"]] <- stabli::handle_arg_by(
          by = arg_list_i[["by"]],
          dataset = arg_list_i[["dataset"]],
          stratification_vame = stratification_vame
        )
        #' @param arg_list `[NULL, list]` (default `NULL`)
        #'
        #' Additional arguments to pass to function named `fun_nm`.
        #'
        #' - `NULL`: Don't pass additional arguments.
        #' - `list`: These arguments will be included in call to `fun_nm`.
        #'   See detailed description of steps performed to see how this works.
        arg_list_i <- c(arg_list_i, arg_list)
        # @codedoc_comment_block design(stabli::stat_table_make_from_by_list)
        # `stabli::stat_table_make_from_by_list` is intended for making "official"
        # statistical tables. It is not meant for more exploratory statistical
        # work where it is run simply to see what is in the data. Instead when
        # one uses  `stabli::stat_table_make_from_by_list`, one should already be
        # familiar with the data and know what to expect.
        #
        # One choice due to the intended purpose of
        # `stabli::stat_table_make_from_by_list` is to silently drop all `NA`
        # strata from `by` of class `data.table`. It is assumed that
        # @codedoc_comment_block design(stabli::stat_table_make_from_by_list)
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        #     + If `arg_list_i[["by"]]` is at this point a `data.table` and
        #       contains any `NA` strata, those `NA` strata are dropped.
        #       `arg_list_i[["by"]]` contains `NA` strata at least if
        #       `stratification_vame`
        #       allows for `NA` values for `by`.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        arg_list_i[["by"]] <- stats::na.omit(arg_list_i[["by"]])
        arg_list_i
      })
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `optional_steps[["lapply_pre_stat_fun_call"]](env = anon_fun_env)`
      #       if that `optional_steps` element exists.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      if ("lapply_pre_stat_fun_call" %in% names(optional_steps)) {
        optional_steps[["lapply_pre_stat_fun_call"]](env = anon_fun_env)
      }
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Call function named `fun_nm` using `arg_list_i`.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #' @param fun_nm `[character]` (no default)
      #'
      #' Must be the name of a function, e.g. `"my_fun"`, `"mypkg::my_fun"`.
      #' The function must return a `stat_table` --- see `[stat_table]`.
      small_stat_table <- call_with_arg_list(fun_nm, arg_list_i)
      if (!inherits(small_stat_table, "stat_table")) {
        stop(sprintf(
          paste0(
            "Output of function named `fun_nm = \"%s\"` must be of class ",
            "`stat_table`. See `?stabli::stat_table`",
          ),
          fun_nm
        ))
      }
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `optional_steps[["lapply_post_stat_fun_call"]](env = anon_fun_env)`
      #       if that `optional_steps` element exists.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      if ("lapply_post_stat_fun_call" %in% names(optional_steps)) {
        optional_steps[["lapply_post_stat_fun_call"]](env = anon_fun_env)
      }
      harmo_meta <- NULL
      pre_harmo_stat_table_meta <-
        stabli::stat_table_meta_get(small_stat_table)
      if (!is.null(harmonisation_vame)) {
        #' @param harmonisation_vame `[NULL, VariableMetadata]` (default `NULL`)
        #'
        #' Contains metadata for harmonising results.
        #' See `?vame::VariableMetadata`.
        #'
        #' - `NULL`: No harmonisation attempted.
        #' - `VariableMetadata`:
        #'   See the detailed description of `stabli::stat_table_make_from_by_list`.
        # @codedoc_comment_block news("stabli::stat_table_make_from_by_list", "2025-07-09", "0.4.0")
        # `stabli::stat_table_make_from_by_list` arg `optional_steps` elem
        # gains element `lapply_pre_harmonisation_call`.
        # @codedoc_comment_block news("stabli::stat_table_make_from_by_list", "2025-07-09", "0.4.0")
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        #     + If `harmonisation_vame` is not `NULL`:
        #       * Run `optional_steps[["lapply_pre_harmonisation_call"]](env = anon_fun_env)`
        #         if that `optional_steps` element exists.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        if ("lapply_pre_harmonisation_call" %in% names(optional_steps)) {
          optional_steps[["lapply_pre_harmonisation_call"]](env = anon_fun_env)
        }
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        #       * Call `harmonisation_vame@vame_harmonise_dt` on the
        #         partial statistics table.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        pre_harmo_stat_table_meta <- stabli::stat_table_meta_get(
          small_stat_table
        )
        harmonisation_vame@vame_harmonise_dt(
          small_stat_table,
          inplace = TRUE
        )
        # @codedoc_comment_block news("stabli::stat_table_make_from_by_list", "2025-07-09", "0.4.0")
        # `stabli::stat_table_make_from_by_list` arg `optional_steps` elem
        # gains element `lapply_post_harmonisation_call`.
        # @codedoc_comment_block news("stabli::stat_table_make_from_by_list", "2025-07-09", "0.4.0")
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        #       * Run `optional_steps[["lapply_post_harmonisation_call"]](env = anon_fun_env)`
        #         if that `optional_steps` element exists.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        if ("lapply_post_harmonisation_call" %in% names(optional_steps)) {
          optional_steps[["lapply_post_harmonisation_call"]](env = anon_fun_env)
        }
        harmo_meta <- attr(small_stat_table, "vame_harmonise_dt_meta")
        dbc::assert_prod_interim_is_list(harmo_meta)
      }
      if (!is.null(harmo_meta)) {
        stabli::stat_table_set(
          small_stat_table,
          lapply(pre_harmo_stat_table_meta, function(meta_datum) {
            if (!is.character(meta_datum)) {
              return(meta_datum)
            }
            indices <- match(meta_datum, harmo_meta[["dt_col_nms"]])
            do_replace <- !is.na(indices)
            meta_datum[do_replace] <- harmo_meta[["dt_col_nms_harmonised"]][
              indices[do_replace]
            ]
            meta_datum
          })
        )
      }
      # stat_table_meta <- stabli::stat_table_meta_get(small_stat_table)
      # invisible(lapply(names(stat_table_meta), function(meta_nm) {
      #   meta <- stat_table_meta[[meta_nm]]
      #   if (!is.null(harmo_meta)) {
      #     m <- match(meta, harmo_meta[["dt_col_nms"]])
      #     has_match <- !is.na(m)
      #     meta[has_match] <- harmo_meta[["dt_col_nms_harmonised"]][
      #       m[has_match]
      #     ]
      #   }
      #   main_env[["complete_stat_table_meta"]][[meta_nm]] <- union(
      #     main_env[["complete_stat_table_meta"]][[meta_nm]],
      #     meta
      #   )
      # }))
      return(small_stat_table[])
    }
  )
  big_meta <- local({
    meta_by_small_stat_table <- lapply(
      big_stat_table,
      stabli::stat_table_meta_get
    )
    meta_nms <- c("stratum_col_nms", "value_col_nms")
    big_meta <- lapply(meta_nms, function(meta_nm) {
      unique(unlist(meta_by_small_stat_table, function(small_meta) {
        small_meta[[meta_nm]]
      }))
    })
    names(big_meta) <- meta_nms
    big_meta
  })
  lapply(big_stat_table, function(small_stat_table) {
    add_col_nms <- setdiff(
      unlist(big_meta),
      names(small_stat_table)
    )
    add_dt <- data.table::setDT(list(rep(NA, nrow(small_stat_table))))
    lapply(add_col_nms, function(add_col_nm) {
      if (!is.null(harmonisation_vame)) {
        data.table::setnames(add_dt, paste0(add_col_nm, "_not_applicable"))
        add_dt <- harmonisation_vame@vame_make(
          var_nms = add_col_nm,
          data = list(df = add_dt)
        )
      } else {
        data.table::setnames(add_dt, add_col_nm)
      }
      data.table::set(
        small_stat_table,
        j = add_col_nm,
        value = add_dt
      )
      NULL
    })
    NULL
  })
  big_stat_table <- call_with_arg_list(
    rbind,
    c(
      big_stat_table,
      use.names = TRUE,
      fill = TRUE
    )
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `optional_steps[["post_lapply"]](env = main_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  if ("post_lapply" %in% names(optional_steps)) {
    optional_steps[["post_lapply"]](env = main_env)
  }
  big_meta <- stabli::stat_table_meta_get(big_stat_table)
  data.table::setcolorder(
    big_stat_table,
    intersect(c(
      big_meta[["stratum_col_nms"]],
      setdiff(names(big_stat_table), unlist(big_meta)),
      big_meta[["value_col_nms"]]
    ), names(big_stat_table))
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  #
  # @codedoc_insert_comment_block return(stabli::stat_table_make_from_by_list)
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # @codedoc_comment_block return(stabli::stat_table_make_from_by_list)
  # Returns a `data.table` with the additional class `stat_table`.
  # @codedoc_comment_block return(stabli::stat_table_make_from_by_list)
  return(big_stat_table[])
}

