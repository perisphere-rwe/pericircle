
`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}

throw_check <- function(x,
                        pre_text = NULL,
                        post_text = NULL){

  if(typeof(x) != 'logical'){
    stop(pre_text, x, post_text, call. = FALSE)
  }

}
