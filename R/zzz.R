.onAttach <- function(...) {
  attached <- pericircle_attach()

  inform_startup(pericircle_attach_message(attached))

  return(
    invisible()
  )
}
