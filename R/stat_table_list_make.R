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
