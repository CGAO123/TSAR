#' Download Well Information Template File
#'
#' Downloads Well Information Template File to the current working directory.
#'
#' @return None
#' @examples
#' \dontrun{
#' download_file()
#' }
#' @export
download_template <- function() {
    download.file(Well_Information_Template, method = "auto")

}
