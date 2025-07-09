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
    required.names = c(
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
    arg_list_i <- settings[["arg_list"]][[i]]
    dbc::assert_has_one_of_classes(
      arg_list_i,
      classes = c("list", "NULL", "character", "call", "name")
    )
    if (is.character(arg_list_i)) {
      arg_list_i <- parse(text = arg_list_i)[[1]]
    }
    if (inherits(arg_list_i, c("call", "name"))) {
      arg_list_i <- eval(arg_list_i, envir = parent.frame(1L))
    }
    if (!inherits(arg_list_i, c("list", "NULL"))) {
      stop(sprintf("`settings[[\"arg_list\"]][[%i]]` ", i),
           "did not evaluate into a `list` nor `NULL`.")
    }
    if (is.null(arg_list_i)) {
      arg_list_i <- as.list(arg_list_i)
    }
    arg_list_i[["by_list"]] <-
      settings[["by_list"]][[i]]
    arg_list_i[["dataset"]] <- dataset
    arg_list_i[["fun_nm"]] <- settings[["fun_nm"]][i]
    arg_list_i[["harmonisation_vame"]] <- harmonisation_vame
    arg_list_i[["stratification_vame"]] <- stratification_vame
    names(arg_list_i) <- gsub("_", ".", names(arg_list_i))

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `optional_steps[["lapply_pre_stat_fun_call"]](env = lapply_fun_env)`
    #     if that `optional_steps` element exists.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    if ("lapply_pre_stat_fun_call" %in% names(optional_steps))  {
      optional_steps[["lapply_pre_stat_fun_call"]](env = lapply_fun_env)
    }

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `stabli::stat_table_make_from_by_list` with `arg_list`.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    st <- do.call(
      stabli::stat_table_make_from_by_list,
      arg_list_i,
      quote = TRUE
    )

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Calls `optional_steps[["lapply_post_stat_fun_call"]](env = lapply_fun_env)`
    #     if that `optional_steps` element exists.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    if ("lapply_post_stat_fun_call" %in% names(optional_steps))  {
      optional_steps[["lapply_post_stat_fun_call"]](env = lapply_fun_env)
    }

    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    #   + Asserts that the produced object is of class `stat_table`.
    #     See `[stat_table]`.
    # @codedoc_comment_block stabli::stat_table_list_make_from_settings
    dbc::assert_inherits(x = st, required.class = "stat_table")
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
  complete_stat_table_meta <- list(
    stratum_col_nms = character(0L),
    value_col_nms = character(0L)
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `lapply` on `by_list`. For each element
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  dt <- lapply(
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
      #     + Create argument list `arg_list` for calling function named
      #       `fun_nm`. It is created by collecting `dataset`, `subset`, `by`
      #       (current element of `by_list`), and
      #       `by_style` into a list, and by appending
      #       `arg_list` to it. `by` is additionally processed by
      #       `stabli::handle_arg_by`.
      #       Only those elements of `arg_list` are kept whose names are
      #       argument names of `fun_nm`.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      arg_list <- local({
        arg_list <- mget(c("dataset", "subset", "by", "by_style"),
                         inherits = TRUE)
        #' @param stratification_vame `[VariableMetadata, NULL]` (default `NULL`)
        #'
        #' Passed to `[handle_arg_by]`.
        arg_list[["by"]] <- stabli::handle_arg_by(
          by = arg_list[["by"]],
          dataset = arg_list[["dataset"]],
          stratification_vame = arg_list[["stratification_vame"]]
        )
        #' @param arg_list `[NULL, list]` (default `NULL`)
        #'
        #' Additional arguments to pass to function named `fun_nm`.
        #'
        #' - `NULL`: Don't pass additional arguments.
        #' - `list`: These arguments will be included in call to `fun_nm`.
        #'   See detailed description of steps performed to see how this works.
        arg_list <- c(arg_list, arg_list)
        arg_list <- arg_list[
          intersect(names(arg_list), names(formals(eval(parse(text = fun_nm)))))
        ]
        stop(
          "eikö ole mitään muuta keinoa päästä NA-arvoista eroon? ",
          "olisiko rumaa tallentaa NA-arvoton kopio arvoavaruuksista ",
          "vame-olioon? no kyllähän se varmaan olisi."
        )
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
        #     + If `arg_list[["by"]]` is at this point a `data.table` and
        #       contains any `NA` strata, those `NA` strata are dropped.
        #       `arg_list[["by"]]` contains `NA` strata at least if
        #       `stratification_vame`
        #       allows for `NA` values for `by`.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        arg_list[["by"]] <- stats::na.omit(arg_list[["by"]])
        arg_list
      })
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `optional_steps[["lapply_pre_stat_fun_call"]](env = anon_fun_env)`
      #       if that `optional_steps` element exists.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      if ("lapply_pre_stat_fun_call" %in% names(optional_steps)) {
        optional_steps[["lapply_pre_stat_fun_call"]](env = anon_fun_env)
      }
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #     + Run `do.call(fun_nm, arg_list, quote = TRUE)`.
      # @codedoc_comment_block stabli::stat_table_make_from_by_list
      #' @param fun_nm `[character]` (no default)
      #'
      #' Must be the name of a function, e.g. `"my_fun"`, `"mypkg::my_fun"`.
      #' The function must return a `data.table` with the additional class
      #' `stat_table` --- see `[stat_table_set]`.
      sub_dt <- do.call(fun_nm, arg_list, quote = TRUE)
      if (!inherits(sub_dt, "stat_table")) {
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
      stat_table_meta <- attr(sub_dt, "stat_table_meta")
      dbc::assert_prod_interim_is_list(stat_table_meta)
      if (!is.null(harmonisation_vame)) {
        #' @param harmonisation_vame `[NULL, VariableMetadata]` (default `NULL`)
        #'
        #' Contains metadata for harmonising results.
        #' See `?vame::VariableMetadata`.
        #'
        #' - `NULL`: No harmonisation attempted.
        #' - `VariableMetadata`:
        #'   See the detailed description of `stabli::stat_table_make_from_by_list`.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        #   + Call `harmonisation_vame@vame_harmonise_dt` on the
        #     partial statistics table if `harmonisation_vame` was not `NULL`.
        # @codedoc_comment_block stabli::stat_table_make_from_by_list
        harmonisation_vame@vame_harmonise_dt(
          sub_dt,
          inplace = TRUE
        )
        harmo_meta <- attr(sub_dt, "vame_harmonise_dt_meta")
        dbc::assert_prod_interim_is_list(harmo_meta)
      } else {
        harmo_meta <- NULL
      }
      # stop("miten varmistan, että tuloksessa on kaikki ositemuuttujat? jos ",
      #      "by_list sisältää vain osan ositemuuttujista mutta haluan että ",
      #      "tuloksessa on kaikki. onko tosiaan pakko olla uusi syöte. ",
      #      "voisiko mieluummin tehdä vaikka cd-kohtaisen vamen jonka kaikki ",
      #      "muuttujat harmonisoidaan / luodaan.")
      # stop("mutta en aina halua kaikkia. esim. basis pitää muuntaa mutta ",
      #      "se ei esiinny kuin yhdessä taulussa.")
      # stop("pitäisikö harmonisation_vame-oliossa olla merkittynä että tämä on pakollinen? ",
      #      "var_dt$mandatory = TRUE?")
      # stop("no miten sitten tehdään arvomuuttujien suhteen? ehkä niitä ei ",
      #      "ollenkaan harmonisoida vamen avulla? vaan ne tilastofunktiot ",
      #      "saa hoitaa senkin?")
      # stop("Onko tämä nimikikkailu aivan pakollista. sotkee koodia. ",
      #      "dg_ca_staty -> cd_ofst_hosp -> hosp aika omituisesti toteutettu. ",
      #      "mieluummin olisi maker suoraan muuttujalle hosp, mutta esim. ",
      #      "sex -> sex ei vissiin voi toimia. cd_sex -> sex?")
      invisible(lapply(names(stat_table_meta), function(meta_nm) {
        meta <- stat_table_meta[[meta_nm]]
        if (!is.null(harmo_meta)) {
          m <- match(meta, harmo_meta[["dt_col_nms"]])
          has_match <- !is.na(m)
          meta[has_match] <- harmo_meta[["dt_col_nms_harmonised"]][
            m[has_match]
          ]
        }
        main_env[["complete_stat_table_meta"]][[meta_nm]] <- union(
          main_env[["complete_stat_table_meta"]][[meta_nm]],
          meta
        )
      }))
      return(sub_dt[])
    }
  )
  lapply(dt, function(sub_dt) {
    add_col_nms <- setdiff(
      unlist(complete_stat_table_meta[c("stratum_col_nms", "value_col_nms")]),
      names(sub_dt)
    )
    add_dt <- data.table::setDT(list(rep(NA, nrow(sub_dt))))
    lapply(add_col_nms, function(add_col_nm) {
      data.table::setnames(add_dt, add_col_nm)
      if (!is.null(harmonisation_vame)) {
        data.table::setnames(add_dt, paste0(add_col_nm, "_not_applicable"))
        add_dt <- harmonisation_vame@vame_make(
          var_nms = add_col_nm,
          data = list(df = add_dt)
        )
      }
      data.table::set(
        sub_dt,
        j = add_col_nm,
        value = add_dt
      )
      NULL
    })
    NULL
  })
  dt <- data.table::rbindlist(dt, use.names = TRUE, fill = TRUE)
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Run `optional_steps[["post_lapply"]](env = main_env)`
  #   if that `optional_steps` element exists.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  if ("post_lapply" %in% names(optional_steps)) {
    optional_steps[["post_lapply"]](env = main_env)
  }
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # - Call `stabli::stat_table_set`.
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  stabli::stat_table_set(
    x = dt,
    meta = list(
      stratum_col_nms = complete_stat_table_meta[["stratum_col_nms"]],
      value_col_nms = complete_stat_table_meta[["value_col_nms"]]
    )
  )
  data.table::setcolorder(
    dt,
    intersect(c(
      complete_stat_table_meta[["stratum_col_nms"]],
      setdiff(names(dt), unlist(complete_stat_table_meta)),
      complete_stat_table_meta[["value_col_nms"]]
    ), names(dt))
  )
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  #
  # @codedoc_insert_comment_block return(stabli::stat_table_make_from_by_list)
  # @codedoc_comment_block stabli::stat_table_make_from_by_list
  # @codedoc_comment_block return(stabli::stat_table_make_from_by_list)
  # Returns a `data.table` with the additional class `stat_table`.
  # @codedoc_comment_block return(stabli::stat_table_make_from_by_list)
  return(dt[])
}
