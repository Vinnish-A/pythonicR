
#' cat_red
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_red = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[31m", text_, "\033[0m\n"))

}

#' cat_green
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_green = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[32m", text_, "\033[0m\n"))

}

#' cat_yellow
#'
#' @description
#' Work like `cat`.
#'
#' @export
cat_yellow = function(..., sep = ' ') {

  text_ = paste(..., sep = sep)
  cat(paste0("\033[33m", text_, "\033[0m\n"))

}

`+` = function(e1, e2) {
  UseMethod("+")
}

#' `+.character`
#'
#' @export
`+.character` = function(a, b) {
  str_c(a, b)
}

#' `+.default`
#'
#' @export
`+.default` = function(a, b) {
  base::`+`(a, b)
}

select = function (.data, ...) {
  UseMethod("select")
}

#' select.list
#'
#' @export
select.list = function(.data, fun_) {

  is_logic_ = \(x_) is.logical(x_) & length(x_) == 1

  logics_ = lapply(.data, fun_)

  if (all(sapply(logics_, is_logic_))) {
    .data[logic_]
  } else {
    stop('Fun_ not suitable')
  }

}

#' vec2copy
#'
#' @export
vec2copy = function(vec_) {

  if (is.numeric(vec_)) {
    cat(sprintf('c(%s)', paste(vec_, collapse = ', ')), '\n')
  } else if (is.character(vec_)) {
    cat(sprintf('c(\'%s\')', paste(vec_, collapse = "', '")), '\n')
  }

}

#' docal
#'
#' @export
docal = function(x_, fun_, ...) {

  params_ = lapply(as.list(match.call())[-c(1:3)], \(x__) eval(x__, envir = x_))
  # browser()
  # if (is.null(names(params_)) | '' %in% names(params_)) names(params_) = names(formals(fun_))
  res_ = do.call(fun_, params_)

  return(res_)

}

