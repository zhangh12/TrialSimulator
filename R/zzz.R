
.onAttach <- function(libname, pkgname) {

  white_text_blue_bg <- "\033[37;44m"
  reset <- "\033[0m"  # Reset to default color
  logo <- '⚕'

  packageStartupMessage(white_text_blue_bg, logo, logo,
                        ' Welcome to TrialSimulator version ',
                        utils::packageVersion(pkgname), '. ',
                        logo, logo, reset)

}
