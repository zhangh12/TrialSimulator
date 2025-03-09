
.onAttach <- function(libname, pkgname) {

  white_text_blue_bg <- '\033[37;44m'
  reset <- '\033[0m'  # Reset to default color
  logo <- '\u2695' ## stringi::stri_escape_unicode('⚕')

  packageStartupMessage(white_text_blue_bg, logo, logo,
                        ' Welcome to TrialSimulator version ',
                        utils::packageVersion(pkgname), '. ',
                        logo, logo, reset)

  ascii_art <-
    '\n \u2513 \u250f  \u2513               \u250f\u2533\u2513  \u2022  \u2513\u250f\u2513\u2022     \u2513        \n \u2503\u2503\u2503\u250f\u2513\u2503\u250f\u250f\u2513\u250f\u2533\u2513\u250f\u2513  \u254b\u250f\u2513   \u2503 \u250f\u2513\u2513\u250f\u2513\u2503\u2517\u2513\u2513\u250f\u2533\u2513\u2513\u250f\u2503\u250f\u2513\u254b\u250f\u2513\u250f\u2513 \n \u2517\u253b\u251b\u2517 \u2517\u2517\u2517\u251b\u251b\u2517\u2517\u2517   \u2517\u2517\u251b   \u253b \u251b \u2517\u2517\u253b\u2517\u2517\u251b\u2517\u251b\u2517\u2517\u2517\u253b\u2517\u2517\u253b\u2517\u2517\u251b\u251b  \n'

}
