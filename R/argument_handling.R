#' @title Argument Handling
#' @description
#' Handle typical input arguments used in statistical functions.
#' @name handle_arg
NULL

by_style_options <- function() {
  c("keep_empty", "drop_empty")
}
#' @eval codedoc::pkg_doc_fun(
#'   "stabli::assert_is_arg_by_style",
#'   "handle_arg"
#')
assert_is_arg_by_style <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("stabli::assert_is_arg_by_style", "2025-07-08", "0.3.0")
  # New function `stabli::assert_is_arg_by_style`.
  # @codedoc_comment_block news("stabli::assert_is_arg_by_style", "2025-07-08", "0.3.0")
  #' @param x Object to inspect.
  #' @param x_nm See `[dbc::handle_args_inplace]`.
  #' @param call See `[dbc::handle_args_inplace]`.
  #' @param assertion_type See `[dbc::handle_args_inplace]`.
  dbc::handle_args_inplace()
  # @codedoc_comment_block stabli::assert_is_arg_by_style
  # `stabli::assert_is_arg_by_style` runs assertions on its input.
  # `by_style` must be either `NULL` or one of
  # `${deparse1(by_style_options())}`.
  # @codedoc_comment_block stabli::assert_is_arg_by_style
  dbc::assert_is_one_of(
    x,
    funs = list(dbc::report_is_NULL,
                dbc::report_atom_is_in_set),
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    arg_list = list(set = by_style_options())
  )
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::assert_is_arg_subset",
#'   "handle_arg"
#')
assert_is_arg_subset <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  dataset = NULL
) {
  # @codedoc_comment_block news("stabli::assert_is_arg_subset", "2025-07-08", "0.3.0")
  # New function `stabli::assert_is_arg_subset`.
  # @codedoc_comment_block news("stabli::assert_is_arg_subset", "2025-07-08", "0.3.0")
  dbc::handle_args_inplace()
  # @codedoc_comment_block stabli::assert_is_arg_subset
  # `stabli::assert_is_arg_subset` performs assertions on its input.
  # `subset` must have one of classes
  # `c("NULL", "integer", "logical", "data.table")`.
  # @codedoc_comment_block stabli::assert_is_arg_subset
  dbc::assert_has_one_of_classes(
    x = x,
    classes = c("NULL", "integer", "logical", "data.table"),
    x_nm = "subset",
    call = call,
    assertion_type = assertion_type
  )
  #' @param dataset `[NULL, data.table]` (default `NULL`)
  #'
  #' This can be supplied to perform additional checks.
  #'
  #' - `NULL`: No additional checks.
  #' - `data.table`: See section **Functions**.
  if (!is.null(dataset)) {
    if (inherits(x, "logical")) {
      # @codedoc_comment_block stabli::assert_is_arg_subset
      # If `dataset` was supplied, additional checks are performed:
      #
      # - `logical` class `subset` must be of length `nrow(dataset)`.
      # @codedoc_comment_block stabli::assert_is_arg_subset
      dbc::assert_is_of_length(
        x = x,
        x_nm = "subset",
        call = call,
        assertion_type = assertion_type,
        expected_length = nrow(dataset)
      )
    } else if (inherits(x, "integer")) {
      # @codedoc_comment_block stabli::assert_is_arg_subset
      # - `integer` class `subset` must contain valid indices into `dataset`
      #   between `-nrow(dataset)` and `nrow(dataset)`.
      # @codedoc_comment_block stabli::assert_is_arg_subset
      dbc::assert_is_between_inclusive(
        x = x,
        x_nm = "subset",
        call = call,
        assertion_type = assertion_type,
        lo = -nrow(dataset),
        hi = nrow(dataset)
      )
    } else if (inherits(x, "data.table")) {
      # @codedoc_comment_block stabli::assert_is_arg_subset
      # - `data.table` class `subset` must have only column names found also in
      #   `dataset`.
      # @codedoc_comment_block stabli::assert_is_arg_subset
      dbc::assert_vector_elems_are_in_set(
        x = names(x),
        x_nm = "names(subset)",
        call = call,
        assertion_type = assertion_type,
        set = names(dataset)
      )
    }
  }
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::assert_is_arg_by",
#'   "handle_arg"
#')
assert_is_arg_by <- function(
  x,
  x_nm = NULL,
  call = NULL,
  assertion_type = NULL,
  dataset = NULL
) {
  # @codedoc_comment_block news("stabli::assert_is_arg_by", "2025-07-08", "0.3.0")
  # New function `stabli::assert_is_arg_by`.
  # @codedoc_comment_block news("stabli::assert_is_arg_by", "2025-07-08", "0.3.0")
  # @codedoc_comment_block stabli::assert_is_arg_by
  # `stabli::assert_is_arg_by` performs assertions on its input:
  #
  # - `by` must have one of classes
  #   `c("NULL", "data.table", "character", "list")`.
  # @codedoc_comment_block stabli::assert_is_arg_by
  dbc::handle_args_inplace()
  dbc::assert_has_one_of_classes(
    x,
    x_nm = x_nm,
    call = call,
    assertion_type = assertion_type,
    classes = c("NULL", "data.table", "character", "list")
  )
  # @codedoc_comment_block stabli::assert_is_arg_by
  # - `by` of class `list`: Every element must be a vector of some kind, a
  #   `data.table`, or `NULL`. Vector elements must be named.
  # @codedoc_comment_block stabli::assert_is_arg_by
  if (inherits(x, "list")) {
    lapply(seq_along(x), function(i) {
      dbc::assert_is_one_of(
        x = x[[i]],
        x_nm = sprintf("%s[[%i]]", x_nm, i),
        call = call,
        assertion_type = assertion_type,
        funs = list(
          dbc::report_is_NULL,
          dbc::report_is_data_table,
          dbc::report_is_vector
        )
      )
      if (!inherits(x[[i]], c("NULL", "data.table"))) {
        if (is.null(names(x)) || names(x)[i] == "") {
          stop(simpleError(sprintf(paste0(
            "Argument `by` was a `list` with a vector as (at least) one ",
            "element, but this element did not have a name. While e.g.",
            "`list(data.table::data.table(a = 1:3), b = 3:1)` is allowed, ",
            "`list(data.table::data.table(a = 1:3), 3:1)` is not. ",
            "The `by` argument had elements with these classes: `",
            deparse1(lapply(x, class)), "`"
          )), call = call))
        }
      }
    })
  }
  if (!is.null(dataset)) {
    if (inherits(x, "data.table")) {
      # @codedoc_comment_block stabli::assert_is_arg_by
      # - If `dataset` is supplied, additional checks are performed depending on
      #   the class of `by`:
      #   + `data.table`: All column names must also exist in `dataset`.
      # @codedoc_comment_block stabli::assert_is_arg_by
      dbc::assert_vector_elems_are_in_set(
        x = names(x),
        x_nm = "names(by)",
        call = call,
        assertion_type = assertion_type,
        set = names(dataset)
      )
    } else if (inherits(x, "character")) {
      # @codedoc_comment_block stabli::assert_is_arg_by
      #   + `character`: All elements must be column names of `dataset`.
      # @codedoc_comment_block stabli::assert_is_arg_by
      dbc::assert_vector_elems_are_in_set(
        x = x,
        x_nm = "by",
        call = call,
        assertion_type = assertion_type,
        set = names(dataset)
      )
    } else if (inherits(x, "list")) {
      # @codedoc_comment_block stabli::assert_is_arg_by
      #   + `list`: Names of vector-class elements and names of
      #     `data.table`-class elements' columns must be column names
      #     of `dataset`.
      # @codedoc_comment_block stabli::assert_is_arg_by
      lapply(seq_along(x), function(i) {
        if (inherits(x[[i]], "data.table")) {
          dbc::assert_vector_elems_are_in_set(
            x = names(x[[i]]),
            x_nm = sprintf("names(%s[[i]])", x_nm),
            call = call,
            assertion_type = assertion_type,
            set = names(dataset)
          )
        } else if (!inherits(x[[i]], "NULL")) {
          dbc::assert_atom_is_in_set(
            x = names(x)[i],
            x_nm = sprintf("names(%s)[i]", x_nm),
            call = call,
            assertion_type = assertion_type,
            set = names(dataset)
          )
        }
      })
    }
  }
  return(invisible(NULL))
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::handle_arg_subset",
#'   "handle_arg"
#')
handle_arg_subset <- function(
  arg_subset_nm = "subset",
  dataset_nm = "dataset",
  output_type = c("logical", "integer",  "as-is")[1],
  eval_env = NULL,
  calling_env = NULL,
  call = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("stabli::handle_arg_subset", "2025-07-08", "0.3.0")
  # New function `stabli::handle_arg_subset`.
  # @codedoc_comment_block news("stabli::handle_arg_subset", "2025-07-08", "0.3.0")
  #' @param eval_env `[NULL, environment]` (default `NULL`)
  #'
  #' Environment where the subset argument and dataset reside.
  #' So these must exist: `eval_env[[arg_subset_nm]]` and
  #' `eval_env[[dataset_nm]]`.
  #'
  #' - `NULL`: Use the environment where this function is called from.
  #'   Unless you are doing something unusual then this is correct.
  #' - `environment`: Some other environment.
  if (is.null(eval_env)) {
    eval_env <- parent.frame(1L)
  }
  #' @param calling_env `[NULL, environment]` (default `NULL`)
  #'
  #' Environment where the statistical function you have written is called from.
  #'
  #' - `NULL`: Use `parent.frame(2L)`.
  #'   Unless you are doing something unusual then this is correct.
  #' - `environment`: Some other environment.
  if (is.null(calling_env)) {
    calling_env <- parent.frame(2L)
  }
  call <- dbc::handle_arg_call(call)
  assertion_type <- dbc::handle_arg_assertion_type(assertion_type)
  #' @param arg_subset_nm `[character]` (default `"subset"`)
  #'
  #' Name of subsetting argument in your statistics function.
  dbc::assert_is_character_nonNA_atom(
    arg_subset_nm,
    assertion_type = "prod_input"
  )
  dbc::assert_atom_is_in_set(
    arg_subset_nm,
    assertion_type = "prod_input",
    set = ls(envir = eval_env)
  )
  #' @param dataset_nm `[character]` (default `"dataset"`)
  #'
  #' Name of dataset in your statistics function.
  dbc::assert_is_character_nonNA_atom(
    dataset_nm,
    assertion_type = "prod_input"
  )
  dbc::assert_atom_is_in_set(
    dataset_nm,
    assertion_type = "prod_input",
    set = ls(envir = eval_env)
  )
  #' @param output_type `[character]` (default `"logical"`)
  #'
  #' Type of output to return regardless of what results from evaluating the
  #' subset argument
  #' - `logical`: Output is a vector of `TRUE` / `FALSE` values indicating rows
  #'   in the dataset to be used in the analysis.
  #' - `integer`: Row numbers of the dataset.
  #' - `as-is`: Whatever your subset argument results in, that is returned.
  dbc::assert_atom_is_in_set(
    output_type,
    assertion_type = "prod_input",
    set = c("logical", "integer",  "as-is")
  )

  # @codedoc_comment_block stabli::handle_arg_subset
  # - Your subsetting expression --- determined via `arg_subset_nm` --- is
  #   collected as a language object (see e.g. `?is.call`).
  # @codedoc_comment_block stabli::handle_arg_subset
  subset_expr <- local({
    subset_arg_string <- arg_subset_nm # e.g. "my_subset"
    subset_arg_symbol <- parse(text = subset_arg_string)[[1]] # e.g. `my_subset`
    substitute_subset_arg_symbol <- substitute(
      substitute(SUBSET_ARG_SYMBOL),
      list(SUBSET_ARG_SYMBOL = subset_arg_symbol)
    ) # e.g. substitute(my_subset)
    subset_expr <- eval(substitute_subset_arg_symbol, eval_env) # e.g. `a == 1`
    subset_expr
  })

  # @codedoc_comment_block stabli::handle_arg_subset
  # - The expression is evaluated with your dataset --- identified via
  #   `dataset_nm` --- and `calling_env`. See `?eval`.
  # @codedoc_comment_block stabli::handle_arg_subset
  subset_value <- eval(
    subset_expr,
    envir = eval_env[[dataset_nm]],
    enclos = calling_env
  )
  assert_is_arg_subset(
    x = subset_value,
    x_nm = "subset",
    call = call,
    assertion_type = assertion_type,
    dataset = eval_env[[dataset_nm]]
  )
  # @codedoc_comment_block stabli::handle_arg_subset
  # - If `output_type == "as-is"`, return what the result.
  # @codedoc_comment_block stabli::handle_arg_subset
  if (output_type == "as-is") {
    return(subset_value)
  }
  # @codedoc_comment_block stabli::handle_arg_subset
  # - Otherwise turn a result of type `data.table`, `integer`, `logical`, or
  #   `NULL` into an `integer` or `logical` type as requested.
  # @codedoc_comment_block stabli::handle_arg_subset
  if (inherits(subset_value, "data.table")) {
    subset_value <- eval_env[[dataset_nm]][
      i = subset_value,
      on = names(subset_value),
      which = TRUE
    ] # to integer
  }
  if (inherits(subset_value, "integer")) {
    subset_value <- switch(
      output_type,
      integer = subset_value,
      logical = local({
        out <- rep(FALSE, nrow(eval_env[[dataset_nm]]))
        out[subset_value] <- TRUE
        out
      })
    )
  } else if (inherits(subset_value, "logical")) {
    subset_value <- switch(
      output_type,
      integer = which(subset_value),
      logical = subset_value
    )
  } else if (inherits(subset_value, "NULL")) {
    subset_value <- switch(
      output_type,
      integer = seq_len(nrow(eval_env[[dataset_nm]])),
      logical = rep(TRUE, nrow(eval_env[[dataset_nm]]))
    )
  }
  # @codedoc_comment_block stabli::handle_arg_subset
  # - Return converted result.
  # @codedoc_comment_block stabli::handle_arg_subset
  return(subset_value)
}

level_space_list_to_level_space_data_table <- function(
  x
) {
  # @codedoc_comment_block stabli:::level_space_list_to_level_space_data_table
  # - If `by` is a `list` of `data.table` / vector elements,
  #   combine its elements into one big `data.table`.
  # @codedoc_comment_block stabli:::level_space_list_to_level_space_data_table
  dbc::assert_is_list(x, assertion_type = "prod_input")

  # @codedoc_comment_block stabli:::level_space_list_to_level_space_data_table
  #   The big `data.table` is created by first determining indices into each
  #   element of the `list` using `data.table::CJ` on e.g.
  #   `list(seq_along(x[[2]]), seq_len(nrow(x[[2]])))`. Then the corresponding
  #   values are added into the big `data.table`.
  # @codedoc_comment_block stabli:::level_space_list_to_level_space_data_table
  x[vapply(x, is.null, logical(1L))] <- NULL
  dt <- call_with_arg_list(data.table::CJ, lapply(seq_along(x), function(i) {
    if (data.table::is.data.table(x[[i]])) {
      seq_len(nrow(x[[i]]))
    } else {
      seq_along(x[[i]])
    }
  }))
  pos_col_nms <- sprintf("_____x[[%i]]_____", seq_along(x))
  data.table::setnames(dt, names(dt), pos_col_nms)
  lapply(seq_along(x), function(i) {
    pos_col_nm <- pos_col_nms[i]
    x_i_is_dt <- data.table::is.data.table(x[[i]])
    x_i <- x[[i]]
    add_col_nms <- if (x_i_is_dt) names(x_i) else names(x)[i]
    pos_vec <- dt[[pos_col_nm]]
    data.table::set(
      x = dt,
      j = add_col_nms,
      value = if (x_i_is_dt) x_i[pos_vec, ] else x_i[pos_vec]
    )
    data.table::set(
      x = dt,
      j = pos_col_nm,
      value = NULL
    )
    NULL
  })
  data.table::setkeyv(dt, names(dt))
  return(dt[])
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::handle_arg_by",
#'   "handle_arg"
#')
handle_arg_by <- function(
  by,
  dataset,
  stratification_vame = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("stabli::handle_arg_by", "2025-07-08", "0.3.0")
  # New function `stabli::handle_arg_by`.
  # @codedoc_comment_block news("stabli::handle_arg_by", "2025-07-08", "0.3.0")
  # @codedoc_comment_block news("stabli::handle_arg_by", "2025-07-08", "0.4.0")
  # `stabli::handle_arg_by` gains argument `stratification_vame`.
  # @codedoc_comment_block news("stabli::handle_arg_by", "2025-07-08", "0.4.0")
  # @codedoc_comment_block stabli::handle_arg_by
  # `stabli::handle_arg_by` turns various types of input into a single
  # `data.table` containing strata in a dataset. Performs the following steps:
  #
  # - Check `by` against `dataset` using `stabli::assert_is_arg_by`. 
  # @codedoc_comment_block stabli::handle_arg_by
  # @codedoc_comment_block stabli::handle_arg_by::by
  # @param by `[NULL, data.table, character, list]` (default `NULL`)
  #
  # Determines stratification:
  #
  # - `NULL`: No stratification.
  # - `data.table`: Use this exact stratification.
  # - `character`: Stratify by these columns in the dataset.
  # - `list`: Combine elements into one big `data.table` and use that as
  #   stratification.
  # @codedoc_comment_block stabli::handle_arg_by::by
  assert_is_arg_by(by, assertion_type = assertion_type, dataset = dataset)
  if (data.table::is.data.table(by)) {
    # @codedoc_comment_block stabli::handle_arg_by
    # - If `by` is a `data.table`, it is only checked against `dataset`.
    # @codedoc_comment_block stabli::handle_arg_by
  } else if (is.character(by)) {
    # @codedoc_comment_block stabli::handle_arg_by
    # - If `by` is a `character` vector:
    # @codedoc_comment_block stabli::handle_arg_by
    stratum_col_nms <- by
    if (
      inherits(stratification_vame, "VariableMetadata") &&
        all(by %in% stratification_vame@var_meta_get_all("var_nm"))
    ) {
      #' @param stratification_vame `[VariableMetadata, NULL]` (default `NULL`)
      #'
      #' Optional `VariableMetadata` object which can be used to determine
      #' `by` in case columns names were supplied.
      #'
      #' - `NULL`: This feature not used.
      #' - `VariableMetadata`: `stratification_vame@vame_category_space_dt`
      #'   can be called on `character`-class `by`.
      # @codedoc_comment_block stabli::handle_arg_by
      #   + If `!is.null(stratification_vame)` and all the column names `by`
      #     are known by it, call
      #     `stratification_vame@vame_category_space_dt(var_nms = by)`.
      # @codedoc_comment_block stabli::handle_arg_by
      by <- stratification_vame@vame_category_space_dt(var_nms = by)
    } else {
      # @codedoc_comment_block stabli::handle_arg_by
      #   + Else collect those columns from `dataset`
      #     into a `data.table`. Drop duplicated strata.
      # @codedoc_comment_block stabli::handle_arg_by
      by <- dataset[
        i = !duplicated(dataset, by = stratum_col_nms),
        j = .SD,
        .SDcols = stratum_col_nms
      ]
      data.table::setkeyv(by, stratum_col_nms)
    }
  } else if (inherits(by, "list")) {
    # @codedoc_comment_block stabli::handle_arg_by
    # @codedoc_insert_comment_block stabli:::level_space_list_to_level_space_data_table
    # @codedoc_comment_block stabli::handle_arg_by
    by <- level_space_list_to_level_space_data_table(by)
    dbc::assert_prod_interim_is_data_table_with_required_names(
      x = dataset, required_names = names(by)
    )
  }
  return(by)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::handle_arg_by_style",
#'   "handle_arg"
#')
handle_arg_by_style <- function(
  by_style,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("stabli::handle_arg_by_style", "2025-07-08", "0.3.0")
  # New function `stabli::handle_arg_by_style`.
  # @codedoc_comment_block news("stabli::handle_arg_by_style", "2025-07-08", "0.3.0")
  # @codedoc_comment_block stabli::handle_arg_by_style
  # `stabli::handle_arg_by_style` runs assertions on `by_style`. It returns
  # `by_style` as-is except `by_style = NULL` causes
  # `"by_style_options()[1]"` to be returned.
  # @codedoc_comment_block stabli::handle_arg_by_style
  #' @param by_style Passed to `stabli::assert_is_arg_by_style`.
  assert_is_arg_by_style(
    by_style,
    assertion_type = assertion_type
  )
  if (is.null(by_style)) {
    by_style <- by_style_options()[1]
  }
  return(by_style)
}

#' @eval codedoc::pkg_doc_fun(
#'   "stabli::handle_arg_by_et_subset_et_by_style_inplace",
#'   "handle_arg"
#')
handle_arg_by_et_subset_et_by_style_inplace <- function(
  handle_arg_nms = NULL,
  dataset_nm = "dataset",
  arg_by_nm = "by",
  arg_subset_nm = "subset",
  arg_by_style_nm = "by_style",
  stratification_vame = NULL,
  eval_env = NULL,
  calling_env = NULL,
  call = NULL,
  assertion_type = NULL
) {
  # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.3.0")
  # New function `stabli::handle_arg_by_et_subset_et_by_style_inplace`.
  # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.3.0")
  # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.4.0")
  # `stabli::handle_arg_by_et_subset_et_by_style_inplace` gains argument
  # `handle_arg_nms`.
  # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.4.0")
  # @codedoc_comment_block stabli::handle_arg_by_et_subset_et_by_style_inplace
  # `stabli::handle_arg_by_et_subset_et_by_style_inplace` calls
  # `stabli::handle_arg_by`, `stabli::handle_arg_by_style`, and
  # `stabli::handle_arg_subset`. Their results are placed into `eval_env`,
  # so by default e.g. your `subset` object is replaced by output of
  # `stabli::handle_arg_subset`.
  # @codedoc_comment_block stabli::handle_arg_by_et_subset_et_by_style_inplace
  #' @param handle_arg_nms `[NULL, character]` (default `NULL`)
  #'
  #' Names of arguments to handle.
  #'
  #' - `NULL`: Handle all, i.e. `c("by", "by_style", "subset")`.
  #' - `character`: Handle only these, subset of
  #'   `c("by", "by_style", "subset")`.
  dbc::assert_is_one_of(
    handle_arg_nms,
    funs = list(dbc::report_is_NULL,
                dbc::report_vector_elems_are_in_set),
    arg_list = list(set = c("by", "by_style", "subset"))
  )
  if (is.null(handle_arg_nms)) {
    handle_arg_nms <- c("by", "by_style", "subset")
  }
  if (is.null(eval_env)) {
    eval_env <- parent.frame(1L)
  }
  if (is.null(calling_env)) {
    calling_env <- parent.frame(2L)
  }
  if (is.null(call)) {
    call <- eval(quote(match.call()), eval_env)
  }

  if ("by_style" %in% handle_arg_nms) {
    #' @param arg_by_style_nm `[character]` (default `"by_style"`)
    #'
    #' Name of your stratification style argument.
    eval_env[[arg_by_style_nm]] <- handle_arg_by_style(
      by_style = eval_env[[arg_by_style_nm]],
      assertion_type = assertion_type
    )
  }
  if ("subset" %in% handle_arg_nms) {
    eval_env[[arg_subset_nm]] <- handle_arg_subset(
      arg_subset_nm = arg_subset_nm,
      dataset_nm = dataset_nm,
      eval_env = eval_env,
      calling_env = calling_env,
      call = call,
      assertion_type = assertion_type
    )
  }
  if ("by" %in% handle_arg_nms) {
    #' @param arg_by_nm `[character]` (default `"by"`)
    #'
    #' Name of your stratification argument.
    eval_env[[arg_by_nm]] <- handle_arg_by(
      by = eval_env[[arg_by_nm]],
      dataset = eval_env[[dataset_nm]],
      # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.4.0")
      # `stabli::handle_arg_by_et_subset_et_by_style_inplace` gains argument
      # `stratification_vame`.
      # @codedoc_comment_block news("stabli::handle_arg_by_et_subset_et_by_style_inplace", "2025-07-08", "0.4.0")
      stratification_vame = stratification_vame,
      assertion_type = assertion_type
    )
  }
}
