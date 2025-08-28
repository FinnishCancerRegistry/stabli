call_with_arg_list <- function(
  fun,
  arg_list,
  envir = NULL
) {
  if (is.null(envir)) {
    envir <- parent.frame(1L)
  }
  fun_nm <- deparse1(substitute(fun))
  dbc::assert_has_one_of_classes(
    x = fun,
    classes = c("function", "character")
  )
  if (is.character(fun)) {
    fun_nm <- fun
  }
  is_anonymous <- grepl("function[(]", fun_nm)
  # uses_double_colon <- grepl("::", fun_nm)
  is_parseable <- !inherits(tryCatch(
    parse(text = fun_nm),
    error = function(e) e
  ), "error")
  if (is_anonymous) {
    # in case an anonymous function is supplied, we don't want to use that as
    # its name in the call.
    fun_nm <- "fun"
  } else if (!is_parseable) {
    # some special such as :, %in%, etc.
    fun_nm <- paste0("`", fun_nm, "`")
  }
  if (is.function(fun)) {
    fun_obj <- fun
  } else if (is.character(fun)) {
    fun_obj <- tryCatch(
      eval(parse(text = fun_nm), envir = envir),
      error = function(e) e
    )
    if (inherits(fun_obj, "error")) {
      fun_obj <- tryCatch(
        get(x = fun_nm, envir = envir, mode = "function"),
        error = function(e) e
      )
      # e.g. fun = ":", fun = "%in%"
      fun_nm <- paste0("`", fun_nm, "`")
    }
    if (inherits(fun_obj, "error")) {
      stop("Could not find a function based on string-valued argument ",
           sprintf("`fun = %s`", fun_nm))
    }
  } else {
    stop("Internal error: No handling defined for `fun` with class(es) ",
         deparse1(class(fun)))
  }
  dbc::assert_is_list(arg_list)
  dbc::assert_is_environment(envir)
  if (is.null(names(arg_list))) {
    # @codedoc_comment_block news("fcrcore::call_with_arg_list", "2025-03-12", "1.0.0")
    # `[fcrcore::call_with_arg_list]` argument `arg_list` handling robustified,
    # now tolerates a list of length zero.
    # @codedoc_comment_block news("fcrcore::call_with_arg_list", "2025-03-12", "1.0.0")
    names(arg_list) <- rep("", length(arg_list))
  }
  arg_lines <- vapply(seq_along(arg_list), function(i) {
    arg_nm <- names(arg_list)[i]
    if (arg_nm %in% c("", NA_character_)) {
      paste0("arg_list[[", i, "]]")
    } else {
      paste0(arg_nm, " = arg_list[[\"", names(arg_list)[i], "\"]]")
    }
  }, character(1L))
  n_rep <- max(length(arg_lines) - 1L, 0L)
  expr_lines <- c(
    "%s(",
    paste0("  ", arg_lines, c(rep(",", n_rep), "")[seq_along(arg_lines)]),
    ")"
  )
  expr_lines <- sprintf(expr_lines, fun_nm)
  eval_expr <- tryCatch(
    parse(text = expr_lines)[[1L]],
    error = function(e) e
  )
  if (inherits(eval_expr, "error")) {
    stop(
      "Internal error: Parsing string ",
      "into R expression failed. Inform the maintainer. This was the string:\n",
      paste0(expr_lines, collapse = "\n")
    )
  }
  eval_envir <- new.env(parent = envir)
  eval_envir[["arg_list"]] <- arg_list
  is_ticked <- grepl("^`", fun_nm) && grepl("`$", fun_nm)
  if (!is_ticked) {
    eval_envir[[fun_nm]] <- fun_obj
  }
  out <- eval(eval_expr, envir = eval_envir)
  return(out)
}
