#' Create an ASCII representation of a string
#'
#' Uses http://artii.herokuapp.com/ API, so requires an active internet connection.
#'
#' @param text A string to be ASCII-fied
#' @param font The font to use
#' @export
#' @seealso AvailableFonts
#' @examples
#' ASCIIfy("test")
ASCIIfy <- function(text, font = NULL) {
    url <- paste0("https://artii.herokuapp.com/make?text=", utils::URLencode(text))
    if (!is.null(font)) url <- paste0(url, "&font=", utils::URLencode(font))
    tryCatch(
        {
            res <- httr::GET(url)
            if (res$status_code != 200) {
                warning(httr::content(res, encoding = "UTF-8"), "\nAre you sure this font exists? See https://artii.herokuapp.com/fonts_list for available fonts.")
            } else {
                return(httr::content(res, encoding = "UTF-8"))
            }
        },
        error = function(e) {
            message(paste0(e$message, "\nIs an internet connection available?"))
        }
    )
}

#' Prints a super cool ASCII title in the font of your choice
#'
#' Uses http://artii.herokuapp.com/ API, so requires an active internet
#' connection.
#'
#' @param text A string to be ASCII-fied
#' @param font The font to use. See http://artii.herokuapp.com/fonts_list
#'  for a comprehensive list of available fonts.
#' @seealso AvailableFonts
#' @export
#' @examples
#' CoolToBeASCII("Step 1: Do something")
#' CoolToBeASCII("Step 2: ???", font = "slant")
#' CoolToBeASCII("Step 3: Profit", font = "starwars")
CoolToBeASCII <- function(text, font = NULL) {
    raw <- ASCIIfy(text, font)
    cat(raw, "\n")
}

#' Get list of available fonts
#'
#' Returns a vector of available fonts.
#' @export
AvailableFonts <- function() {
    url <- "https://artii.herokuapp.com/fonts_list"
    res <- httr::GET(url)
    return(strsplit(httr::content(res, encoding = "UTF-8"), "\n")[[1]])
}

#' Preview fonts
#'
#' Note that there are large number of fonts, and many are less than
#' practical.
#' @export
ASCIIFontGallery <- function() {
    fonts <- AvailableFonts()
    i <- 0
    for (font in fonts) {
        if (i >= 3) {
            readline(prompt = "Press [enter] to continue")
            i <- 0
        } else {
            i <- i + 1
        }
        cat("\n\n", font, "\n")
        CoolToBeASCII(font, font)
    }
}
