#' @title `tab_list`
#' @description
#' Functions related to `tab_list`.
#' @name stat_table_list_make
NULL

#' @eval codedoc::pkg_doc_fun(
#'   regex = "stabli::stat_table_list_make_from_settings",
#'   rdname = "stat_table_list_make"
#' )
stat_table_list_make_from_settings <- function(
  dataset,
  settings,
  optional_steps = NULL,
  harmonisation_vame = NULL,
  stratification_vame = NULL
) {
  main_fun_env <- environment()
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  # Performs the following steps:
  # - Calls `optional_steps[["on_entry"]](env = main_fun_env)` if that
  #   `optional_steps` element exists. `main_fun_env` is the temporary evaluation
  #   environment of `stabli::stat_table_list_make_from_settings`.
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  if ("on_entry" %in% names(optional_steps)) {
    optional_steps[["on_entry"]](env = main_fun_env)
  }
  # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2024-10-23", "0.1.0")
  # New exported fun `[stabli::stat_table_list_make_from_settings]`.
  # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2024-10-23", "0.1.0")

  #' @param dataset `[data.table]` (no default)
  #'
  #' The main dataset.
  dbc::assert_is_data_table(dataset)

  #' @param settings `[data.table]` (no default)
  #'
  #' The settings table. We effectively pass each row of this table to
  #' `stabli::stat_table_make_from_by_list`. Must have columns
  #' - `tab_nm` `[character]`: Name of the table in the produced list.
  #'   E.g. `"count_time_series"`. Not passed to
  #'   `stabli::stat_table_make_from_by_list` but used to determine name of
  #'   output element.
  #' - `fun_nm` `[character]`: Each `settings[["fun_nm"]][i]` is passed.
  #'   E.g. `"mypkg::myfun"`.
  #' - `by_list` `[list]`: Each
  #'   `settings[["by_list"]][[i]]` passed.
  #' - `arg_list` `[character, list]`: Each
  #'   `settings[["arg_list"]][i]` or `settings[["arg_list"]][[i]]` passed.
  dbc::assert_is_data_table_with_required_names(
    settings,
    required_names = c(
      "tab_nm", "fun_nm", "by_list", "arg_list"
    )
  )
  dbc::assert_is_character_nonNA_vector(
    settings[["tab_nm"]]
  )
  dbc::assert_has_no_duplicates(
    settings[["tab_nm"]]
  )
  dbc::assert_is_character_nonNA_vector(
    settings[["fun_nm"]]
  )
  dbc::assert_is_list(
    settings[["by_list"]]
  )
  dbc::assert_is_one_of(
    settings[["arg_list"]],
    funs = list(dbc::report_is_character_nonNA_vector,
                dbc::report_is_list)
  )

  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  # - Calls `on.exit(optional_steps[["on_exit"]](env = main_fun_env))` if that
  #   `optional_steps` element exists.
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  if ("on_exit" %in% names(optional_steps))  {
    on.exit(optional_steps[["on_exit"]](env = main_fun_env))
  }

  stl <- lapply(seq_len(nrow(settings)), function(i) {
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    # - `lapply`'s through row numbers of `settings`. For each `i`:
    #   + Calls `optional_steps[["lapply_on_entry"]](env = lapply_fun_env)` if
    #     that `optional_steps` element exists. `lapply_fun_env` is the evaluation
    #     environment of the anonymous function used in the `lapply` call.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    lapply_fun_env <- environment()
    if ("lapply_on_entry" %in% names(optional_steps))  {
      optional_steps[["lapply_on_entry"]](env = lapply_fun_env)
    }
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `on.exit(optional_steps[["lapply_on_exit"]](env = lapply_fun_env))`
    #     if that `optional_steps` element exists.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    if ("lapply_on_exit" %in% names(optional_steps))  {
      on.exit(optional_steps[["lapply_on_exit"]](env = lapply_fun_env))
    }
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Collects `arg_list` for calling `stabli::stat_table_make_from_by_list`.
    #     `settings[["arg_list"]][i]`
    #     is parsed and evaluated if it is of class `character`. If it is a
    #     quoted expression it is also evaluated first. The evaluation
    #     environment is the environment where
    #     `stabli::stat_table_list_make_from_settings` is called. The result of the evaluation
    #     must be a `list` or `NULL`. Otherwise each
    #     `settings[["arg_list"]]` element must be a `list` or `NULL`.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    dbc::assert_is_one_of(
      settings[["arg_list"]][[i]],
      funs = list(dbc::report_is_NULL,
                  dbc::report_is_list,
                  dbc::report_is_unevaluated_expression)
    )
    arg_list_i <- settings[["arg_list"]][[i]]
    if (is.character(arg_list_i)) {
      arg_list_i <- parse(text = arg_list_i)[[1]]
    }
    if (dbc::test_is_unevaluated_expression(arg_list_i)) {
      arg_list_i <- eval(arg_list_i, envir = parent.frame(1L))
    }
    if (!inherits(arg_list_i, c("list", "NULL"))) {
      stop(sprintf("`settings[[\"arg_list\"]][[%i]]` ", i),
           "did not evaluate into a `list` nor `NULL`.")
    } else if (is.null(arg_list_i)) {
      arg_list_i <- as.list(arg_list_i)
    }
    arg_list_i[["by_list"]] <-
      settings[["by_list"]][[i]]
    arg_list_i[["dataset"]] <- dataset
    arg_list_i[["fun_nm"]] <- settings[["fun_nm"]][i]
    arg_list_i[["harmonisation_vame"]] <- harmonisation_vame
    arg_list_i[["stratification_vame"]] <- stratification_vame

    # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2025-07-09", "0.4.0")
    # `stabli::stat_table_list_make_from_settings` arg `optional_steps` elem
    # `lapply_pre_stat_fun_call` renamed to `lapply_pre_stat_table_call`.
    # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2025-07-09", "0.4.0")
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `optional_steps[["lapply_pre_stat_table_call"]](env = lapply_fun_env)`
    #     if that `optional_steps` element exists.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    if ("lapply_pre_stat_table_call" %in% names(optional_steps))  {
      optional_steps[["lapply_pre_stat_table_call"]](env = lapply_fun_env)
    }

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `stabli::stat_table_make_from_by_list` with `arg_list_i`.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    st <- call_with_arg_list(
      fun = stabli::stat_table_make_from_by_list,
      arg_list = arg_list_i
    )

    # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2025-07-09", "0.4.0")
    # `stabli::stat_table_list_make_from_settings` arg `optional_steps` elem
    # `lapply_post_stat_fun_call` renamed to `lapply_post_stat_table_call`.
    # @codedoc_comment_block news("stabli::stat_table_list_make_from_settings", "2025-07-09", "0.4.0")
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `optional_steps[["lapply_post_stat_table_call"]](env = lapply_fun_env)`
    #     if that `optional_steps` element exists.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    if ("lapply_post_stat_table_call" %in% names(optional_steps))  {
      optional_steps[["lapply_post_stat_table_call"]](env = lapply_fun_env)
    }

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Asserts that the produced object is of class `stat_table`.
    #     See `[stat_table]`.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    stabli::stat_table_assert(st, assertion_type = "general")
    return(st[])
  })
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  #  - Sets the names of the list produced by `lapply` to
  #    `settings[["tab_nm"]]`.
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  names(stl) <- settings[["tab_nm"]]

  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  # - Calls `stabli::stat_table_list_set` with
  #   `meta = list(meta_dt = settings)` --- without column `tab_nm`.
  #
  # @codedoc_insert_comment_block return(stabli::stat_table_list_make_from_settings)
  # @codedoc_comment_block stabli::stat_table_list_make_from_settings
  #' @importFrom data.table .SD
  settings <- settings[j = .SD, .SDcols = setdiff(names(settings), "tab_nm")]
  stabli::stat_table_list_set(stl, list(meta_dt = settings))

  # @codedoc_comment_block return(stabli::stat_table_list_make_from_settings)
  # Returns a `stat_table_list`.
  # @codedoc_comment_block return(stabli::stat_table_list_make_from_settings)
  return(stl)
}

#' @eval codedoc::pkg_doc_fun(
#'   regex = "stabli::stat_table_make_from_by_list",
#'   rdname = "stat_table_list_make"
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
  dbc::assert_is_list(by_list)
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
