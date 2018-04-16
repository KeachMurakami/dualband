#' Determine affine coefficient interatcively
#'
#' @description
#' Opens shiny application.
#'
#' @export
#' @return this function does not return any object.
shiny_affine <-
  function(){
    runGitHub("KeachMurakami/shiny_apps", subdir = "affine_transform")
    }
