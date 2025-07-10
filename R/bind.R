
bind_left <- function(x, y){
  .bind_dictionaries(x, y, prefer = 'left')
}

bind_right <- function(x, y){
  .bind_dictionaries(x, y, prefer = 'left')
}

.bind_dictionaries <- function(dict1, dict2, prefer = c("left", "right")) {

  prefer <- match.arg(prefer)

  stopifnot(inherits(dict1, "DataDictionary"))
  stopifnot(inherits(dict2, "DataDictionary"))

  vars1 <- dict1$variables
  vars2 <- dict2$variables

  dupes <- intersect(names(vars1), names(vars2))

  if (!purrr::is_empty(dupes)) {
    warning("Duplicated variable names found: ",
            paste(dupes, collapse = ", "),
            ". Using definitions from the ", prefer, "-hand dictionary.")
  }

  combined_vars <- switch(prefer,
                          left  = c(vars1, vars2[setdiff(names(vars2), names(vars1))]),
                          right = c(vars1[setdiff(names(vars1), names(vars2))], vars2)
  )

  DataDictionary$new(combined_vars)

}
