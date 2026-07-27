#' Reconstruct source references inside a function
#'
#' A function keeps a `srcref` for itself, but not for the expressions in its
#' body. This recovers those from the parse data of the function's source file,
#' so that coverage and tracing can attribute results to source locations.
#'
#' @param fun the function to annotate. Must have been parsed with
#'   `keep.source` enabled, otherwise it is returned unchanged.
#' @return `fun` with `srcref` attributes imputed on the expressions of its
#'   formals and body.
#' @export
impute_fun_srcref <- function(fun) {
  srcref <- attr(fun, "srcref")
  if (is.null(srcref)) return(fun)

  pd <- get_parse_data(attr(srcref, "srcfile"))

  call <- call("function", formals(fun), body(fun))
  imputed_call <- impute_srcref(call, srcref, pd)

  formals(fun) <- imputed_call[[2]]
  body(fun) <- imputed_call[[3]]
  fun
}

impute_srcref <- function(x, parent_ref, pd) {
  if (!is.call(x)) return(x)
  if (is.null(parent_ref)) return(x)
  if (is.null(pd)) return(x)

  pd_expr <-
    (
      (pd$line1 == parent_ref[[1L]] & pd$line2 == parent_ref[[3L]]) |
      (pd$line1 == parent_ref[[7L]] & pd$line2 == parent_ref[[8L]])
    ) &
    pd$col1 == parent_ref[[2L]] &
    pd$col2 == parent_ref[[4L]] &
    pd$token %in% c("expr", "equal_assign", "expr_or_assign_or_help")

  pd_expr_idx <- which(pd_expr)

  if (length(pd_expr_idx) == 0L) return(x) # srcref not found in parse data

  if (length(pd_expr_idx) > 1) pd_expr_idx <- pd_expr_idx[[1]]

  expr_id <- pd$id[pd_expr_idx]

  pd_child <- pd[pd$parent == expr_id, ]
  pd_child <- pd_child[order(pd_child$line1, pd_child$col1), ]

  # exclude comments
  pd_child <- pd_child[pd_child$token != "COMMENT", ]

  if (pd$line1[pd_expr_idx] == parent_ref[[7L]] & pd$line2[pd_expr_idx] == parent_ref[[8L]]) {
    line_offset <- parent_ref[[7L]] - parent_ref[[1L]]
  } else {
    line_offset <- 0
  }

  make_srcref <- function(from, to = from, pd=pd_child) {
    if (length(from) == 0) {
      return(NULL)
    }

    srcref(
      attr(parent_ref, "srcfile"),
      c(pd$line1[from] - line_offset,
        pd$col1[from],
        pd$line2[to] - line_offset,
        pd$col2[to],
        pd$col1[from],
        pd$col2[to],
        pd$line1[from],
        pd$line2[to]
      )
    )
  }

  # return early on the following keywords
  if (nrow(pd_child) == 1 && pd_child$token %in% c("NEXT", "BREAK")) {
    return(x)
  } else if (nrow(pd_child) == 3 && pd_child$token[2] %in% c("NS_GET", "NS_GET_INT")) {
    return(x)
  }

  fun <- as.character(x[[1]])[1]

  if ((fun %in% c("<-", "<<-", "=", "!", "~", "~", "+", "-", "*", "/", "^", "<", ">", ":=", "<=",
                  ">=", "==", "!=", "&", "&&", "|", "||", "$", "[", "[[", ":")) ||
        (startsWith(fun, "%") && endsWith(fun, "%"))) {
    ref <- if (length(x) == 3) {
      if (pd_child$token[2] == "RIGHT_ASSIGN") {
        lhs <- 3
        rhs <- 1
      } else {
        lhs <- 1
        rhs <- 3
      }

      x[[2]] <- impute_srcref(x[[2]], make_srcref(lhs), pd)
      x[[3]] <- impute_srcref(x[[3]], make_srcref(rhs), pd)
    } else {
      x[[2]] <- impute_srcref(x[[2]], make_srcref(2), pd)
    }
  } else if (fun == "if") {
    # expression:
    # IF cond then_branch else_branch
    # parse_data:
    # IF ( cond ) then_branch ELSE else_branch

    # if-condition is never NULL
    cond_srcref <- make_srcref(3)
    then_srcref <- NULL
    else_srcref <- NULL

    x[[2]] <- impute_srcref(x[[2]], cond_srcref, pd)

    if (!is.null(x[[3]])) {
      then_srcref <- make_srcref(5)
      x[[3]] <- impute_srcref(x[[3]], then_srcref, pd)
    }

    if (length(x) == 4) {
      else_srcref <- make_srcref(7)
      x[[4]] <- impute_srcref(x[[4]], else_srcref, pd)
    }

    attr(x, "srcref") <- list(NULL, cond_srcref, then_srcref, else_srcref)[seq_along(x)]
  } else if (fun == "for") {
    # for parsing data contain 3 elements: FOR, forcond and expr for body
    # the forconf contains: symbol, in and the actual expression which
    # want to include for the coverage
    cond_srcref <- {
      forcond_id <- pd_child$id[2]
      pd_expr <- pd[pd$parent==forcond_id & pd$token=="expr", ]

      stopifnot(nrow(pd_expr) == 1)

      make_srcref(1, pd=pd_expr)
    }

    x[[3]] <- impute_srcref(x[[3]], cond_srcref, pd)
    x[[4]] <- impute_srcref(x[[4]], make_srcref(3), pd)
  } else if (fun == "while") {
    # x:
    # WHILE cond body
    # pd_child:
    # WHILE ( cond ) body
    x[[2]] <- impute_srcref(x[[2]], make_srcref(3), pd)
    x[[3]] <- impute_srcref(x[[3]], make_srcref(5), pd)
  } else if (fun == "repeat" && pd_child$token[1] == "REPEAT") {
    # x:
    # REPEAT body
    # pd_child:
    # REPEAT expr
    # repeat always executes
    x[[2]] <- impute_srcref(x[[2]], make_srcref(2), pd)
  } else if (fun == "switch") {
    # from `?switch`:
    # switch works in two distinct ways depending whether the first
    # argument evaluates to a character string or a number.
    #
    # x:
    # SWITCH cond case1 case2 ... caseM
    # pd_child:
    # expr ( expr1 , expr2 , ... , exprN)
    #  or
    # expr ( expr, SYMBOL_SUB EQ_SUB expr1 , ... )
  } else if (fun == "function") {
    # first update formals
    params <- x[[2]]
    if (!is.null(params)) {
      params_exprs_pos <- if (!is.null(params)) {
        which(sapply(params, function(y) !is.symbol(y) || as.character(y) != ""))
      } else {
        integer(0)
      }
      # the last expr is function body
      params_exprs <- head(which(pd_child$token %in% c("expr", "equal_assign")), -1)

      stopifnot(length(params_exprs_pos) == length(params_exprs))

      for (i in seq_along(params_exprs_pos)) {
        param_pos <- params_exprs_pos[i]

        param_srcref <- make_srcref(params_exprs[i])
        if (!is.null(params[[param_pos]])) {
          params[[param_pos]] <- impute_srcref(params[[param_pos]], param_srcref, pd)

          if (is.null(attr(params[[param_pos]], "srcref")) &&
                is.call(params[[param_pos]]) &&
                !is_conditional_loop_or_block(params[[param_pos]])) {
            attr(params[[param_pos]], "srcref") <- param_srcref
          }
        }
      }
    }

    # then update body
    body_srcref <- make_srcref(nrow(pd_child))
    if (!is.null(x[[3]])) {
      x[[3]] <- impute_srcref(x[[3]], body_srcref, pd)
    }
  } else if (fun == "{") {
    refs <- attr(x, "srcref")
    stopifnot(length(x) == length(refs))

    for (i in seq_along(x)[-1]) {
      if (!is.null(x[[i]])) {
        x[[i]] <- impute_srcref(x[[i]], refs[[i]], pd)
        if (is_conditional_loop_or_block(x[[i]])) refs[i] <- list(NULL)
      }
    }
    ## browser()
    attr(x, "srcref") <- c(NULL, refs[-1])
    # prevent multiple `{` nesting
    # this could happen if the only expression in within the current `{`
    # is one of the control structure for which we impute source references
    if (length(x) == 2 && length(x[[2]]) > 1 && identical(x[[2]][[1]], as.name("{"))) {
      x <- x[[2]]
    }
  } else if (fun == "(") {
    body_srcref <- make_srcref(2)
    if (!is.null(x[[2]])) {
      x[[2]] <- impute_srcref(x[[2]], body_srcref, pd)
    }
  } else {
     # take care about the function name explicitly
    if (is.call(x[[1]])) {
      x[[1]] <- impute_srcref(x[[1]], make_srcref(1), pd)
    }

    # do this only if there are any arguments
    if (nrow(pd_child) > 3) {
      # the arguments must be done manually as some can be empty, e.g. f(.=)
      arg_idx <- 2
      for (i in seq(3, nrow(pd_child)-1)) {
        token <- pd_child$token[i]
        # TODO equal_assign
        if (token == "expr") {
          if (!is.null(x[[arg_idx]])) {
            x[[arg_idx]] <- impute_srcref(x[[arg_idx]], make_srcref(i), pd)
          }
        } else if (token == "','") {
          arg_idx <- arg_idx + 1
        }
      }
    }

    attr(x, "srcref") <- parent_ref
  }

  x
}


# TODO: move to separate file

package_parse_data <- new.env(parent=emptyenv())

get_parse_data <- function(srcfile) {
  filename <- srcfile[["filename"]]
  keep <- !is.null(filename) && nchar(filename) > 0

  if (!keep || length(package_parse_data) == 0) {
    lines <- getSrcLines(srcfile, 1L, Inf)
    lines_split <- split_on_line_directives(lines)
    if (is.null(names(lines_split)) && length(lines_split) == 1) {
      # there was no split - all lines come from the same file
      names(lines_split) <- filename
    }
    res <- lapply(lines_split,
                  function(x) getParseData(parse(text = x, keep.source = TRUE), includeText = TRUE))

    if (keep) {
      for (i in seq_along(res)) {
        package_parse_data[[names(res)[[i]]]] <- res[[i]]
      }
    }
  }

  if (keep) {
    package_parse_data[[filename]]
  } else {
    res
  }
}

clean_parse_data <- function() {
  rm(list = ls(package_parse_data), envir = package_parse_data)
}

# Split lines into a list based on the line directives in the file.
split_on_line_directives <- function(lines) {
  matches <- rex::re_matches(lines,
    rex::rex(start, any_spaces, "#line", spaces,
      capture(name = "line_number", digit), spaces,
      quotes, capture(name = "filename", anything), quotes))
  directive_lines <- which(!is.na(matches$line_number))
  if (length(directive_lines) > 0) {
    file_starts <- directive_lines + 1
    file_ends <- c(directive_lines[-1] - 1, length(lines))
    res <- mapply(function(start, end) lines[start:end], file_starts, file_ends)
    names(res) <- na.omit(matches$filename)
  } else {
    res <- list(lines)
  }
  res
}

is_conditional_loop_or_block <- function(x) {
  is.call(x) &&
  (identical(x[[1L]], as.name("{")) ||
    (is.symbol(x[[1L]]) && as.character(x[[1L]]) %in% c("if", "for", "switch", "while")))
}
