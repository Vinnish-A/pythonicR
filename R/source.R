
.rule = function(x_) {
  str_detect(x_, '#') & str_count(x_, '-') > 3
}

#' bodyOf
#'
#' @description
#' Build a nested list recursively by rule.
#'
#' @param vec_ File readed as vectors.
#' @param rule_ rule.
#'
#' @import stringr
#'
#' @return Character vector, the shortest path of a certain name in a list
#'
#' @keywords internal
bodyOf = function(vec_, rule_) {

  gen_ = rule_(vec_)

  while (T %in% rule_(vec_) & all(gen_ == 0)) {

    vec_[rule_(vec_)] = vec_[rule_(vec_)] |> str_sub(2)

    if (all(rule_(vec_)) == F) stop()

  }

  if (1 %in% gen_) {

    if (gen_[[1]] != 1) {
      head_ = vec_[seq(1, min(which(gen_ == 1))-1)]
    } else {
      head_ = NA
    }

    vec_ = vec_[min(which(gen_ == 1)):length(vec_)]
    gen_ = as.numeric(str_count(vec_, '#') == 1 & str_count(vec_, '-') > 3)

  }

  for (i_ in seq_along(gen_)) {

    if (i_ != 1) {

      gen_[[i_]] = gen_[[i_ - 1]] + gen_[[i_]]

    }

  }
  res_ = split(vec_, gen_)
  names(res_) = vec_[!duplicated(gen_)] |> str_extract("^# [^#]*?-") |> str_sub(2, -2) |> trimws()
  for (i_ in seq_along(res_)) {

    res_[[i_]] = res_[[i_]][-1]
    condition_ = rule_(res_[[i_]])
    res_[[i_]][condition_] = res_[[i_]][condition_] |> str_sub(2)

    if (T %in% rule_(res_[[i_]])) {
      res_[[i_]] = bodyOf(res_[[i_]], rule_)
    }

  }

  res_[['head']] = head_

  return(res_)


}

#' checkFormat
#'
#' @description
#' Check if the document title levels comply with the rule.
#'
#' @param vec_ File readed as vectors.
#' @param rule_ rule.
#'
#' @keywords internal
checkFormat = function(vec_, rule_, checkType_) {

  titles_ = vec_[sapply(vec_, rule_)]

  if (length(titles_) == 0) stop('No title row matches the `rule`. ')

  if (checkType_ == 'strict') {

    # 1. 检查是否符合rule
    # 2. 检查是否存在一级标题
    # 3. 检查同一级内的同级标题
    # 4. 检查是否存在越级标题

    len_ = c()
    for (i_ in seq_along(titles_)) {

      if (i_ == 1) {
        headN_ = str_count(titles_[[1]], '#')
        if (headN_ != 1) stop('File doesn\' t start with a level one heading.')
        len_[[1]] = headN_
      } else {
        len_[[i_]] = str_count(titles_[[i_]], '#')
        con1_ = len_[[i_]] - len_[[i_-1]] > 1
        if (con1_) stop('Title Overleveling, which:\n', titles_[[i_]])
      }

    }

  } else if (checkType_ == 'lenient') {

    # 1. 检查是否符合rule
    # 2. 检查是否存在一级标题
    # 3. 检查同一级内的同级标题


  }

}

#' buildFile
#'
#' @description
#' Build a nested list recursively by rule_.
#'
#' @param filename_ File to source.
#'
#' @import stringr
#'
#' @return Builded nested list containing codes' head and body.
#'
#' @keywords internal
buildFile = function(filename_, rule_ = NULL, checkType_ = 'strict') {

  if (is.null(rule_)) rule_ = .rule

  vec_ = suppressWarnings(readLines(filename_))

  vec_ = vec_[!str_detect(vec_, '^\\s*$')]

  withAssume(checkFormat(vec_, rule_, checkType_), 'Format check passed! ', 'Format check failed...')

  return(bodyOf(vec_, rule_)[[1]])

}

#' findWhere
#'
#' @description
#' Flatten the shortest path of a certain name in a list.
#'
#' @param lst_
#' @param selected_
#' @param path_
#'
#' @return Character vector, the shortest path of a certain name in a list
#'
#' @keywords internal
findWhere = function(lst_, selected_, path_ = NULL) {

  for (name_ in names(lst_)) {

    path_new_ = c(path_, name_)

    if (identical(name_, selected_)) return(path_new_)

    if (is.list(lst_[[name_]])) {

      res_ = findWhere(lst_[[name_]], selected_, path_new_)
      if (!is.null(res_)) return(res_)

    }

  }

  return(NULL)

}

#' bulidContent
#'
#' @description
#' Return the specified module of the project file that complies with the "Vinnish" standard.
#'
#' @param filename_ file to source.
#' @param selected_ part designated to source.
#' @param root_ Return to the previous node or not.
#'
#' @return content
#'
#' @export
buildContent = function(selected_, filename_, root_ = F, checkType_ = 'strict') {

  builded_ = buildFile(filename_, checkType_ = checkType_)

  lst_ = builded_
  path_ = c()
  for (ele_ in selected_) {

    where_ = findWhere(lst_, ele_)
    lst_ = lst_[[where_]]
    path_ = c(path_, where_)

  }
  where_ = path_

  if (root_) where_ = where_[-length(where_)]

  if (is.null(where_)) stop()

  heads_ = c()
  lst_ = builded_
  for (ele_ in append(as.list(where_[-length(where_)]), 1)) {
    heads_ = append(heads_, lst_[['head']] |> paste0(collapse = '\n'))
    lst_ = lst_[[ele_]]
  }
  head_ = paste0(heads_, collapse = '\n')

  text_ = Reduce(function(x__, idx__) x__[[idx__]], where_, init = builded_)
  if (is.list(text_)) text_ = text_[['head']]
  text_ = paste0(text_, collapse = '\n')

  content_ = paste(head_, text_, sep = '\n')

  return(content_)

}

#' moveHead
#'
#' @param lst_
#'
#' @keywords internal
moveHead = function(lst_) {

  if (!is.list(lst_)) return(lst_)

  if ("head" %in% names(lst_)) {
    lst_ = lst_[c("head", setdiff(names(lst_), "head"))]
  }

  for (i in seq_along(lst_)) {
    if (is.list(lst_[[i]])) {
      lst_[[i]] = moveHead(lst_[[i]])
    }
  }

  return(lst_)

}

#' buildAllContent
#'
#' @description
#' Return the specified module of the project file that complies with the "Vinnish" standard.
#'
#' @param filename_ file to source.
#' @param selected_ part designated to source.
#'
#' @return content
#'
#' @export
buildAllContent = function(selected_, filename_, checkType_ = 'strict') {

  builded_ = moveHead(buildFile(filename_, checkType_ = checkType_))

  where_ = findWhere(builded_, selected_)

  if (is.null(where_)) stop()

  heads_ = c()
  lst_ = builded_
  for (ele_ in append(as.list(where_[-length(where_)]), 1)) {
    heads_ = append(heads_, lst_[['head']] |> paste0(collapse = '\n'))
    lst_ = lst_[[ele_]]
  }
  head_ = paste0(heads_, collapse = '\n')

  text_ = Reduce(function(x__, idx__) x__[[idx__]], where_, init = builded_) |>
    unlist() |> paste0(collapse = '\n')

  content_ = paste(head_, text_, sep = '\n')

  return(content_)

}

#' runThis
#'
#' @description
#' Run the specified module of the project file that complies with the "Vinnish" standard.
#'
#' @param filename_ file to source
#' @param ... part designated to source
#'
#' @return Null
#'
#' @export
runThis = function(..., filename_ = rstudioapi::getSourceEditorContext()$path, checkType_ = 'lenient') {

  selected_ = unlist(list(...))

  content_ = buildContent(selected_, filename_, F, checkType_)

  file_ = paste0(tempfile(), '.R')
  cat(content_, file = file_)

  withAssume(source(file_))

  invisible(file.remove(file_))

}

#' prepareThis
#'
#' @description
#' Prepare the specified module of the project file that complies with the "Vinnish" standard.
#'
#' @param filename_ file to source
#' @param ... part designated to source
#'
#' @return Null
#'
#' @export
prepareThis = function(..., filename_ = rstudioapi::getSourceEditorContext()$path) {

  selected_ = unlist(list(...))

  content_ = buildContent(selected_, filename_, T)

  file_ = paste0(tempfile(), '.R')
  cat(content_, file = file_)

  withAssume(source(file_))

  invisible(file.remove(file_))

}

#' runThese
#'
#' @description
#' Run the specified module of the project file that complies with the "Vinnish" standard.
#'
#' @param filename_ file to source
#' @param ... part designated to source
#'
#' @return Null
#'
#' @export
runThese = function(..., filename_ = rstudioapi::getSourceEditorContext()$path) {

  selected_ = unlist(list(...))

  content_ = buildAllContent(selected_, filename_)

  file_ = paste0(tempfile(), '.R')
  cat(content_, file = file_)

  withAssume(source(file_))

  invisible(file.remove(file_))

}

#' fileWhenTest
#'
#' @importFrom pkgload is_dev_package
#'
#' @export
fileWhenTest = function(file_, path_ = '../../inst/extdata/') {

  if (is_dev_package('pythonicR')) {
    filename_ = paste0(path_, file_)
  } else {
    filename_ = system.file('extdata', file_, package = 'pythonicR')
  }

  return(filename_)

}
