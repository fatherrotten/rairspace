#' Import FAA Special Use Airspace Order text
#'
#' @param filepath A URL or full filepath to a .txt or .pdf SUA file
#'
#' @return A character vector of text from a SUA order
#' @export
#'
sua_import <-
  function(filepath =
             "http://www.faa.gov/documentLibrary/media/Order/7400.10c_2021-02-09.pdf") {
    # is there a valid file extension?

    if (grepl("\\.[Tt][Xx][Tt]$", filepath) == TRUE) {
      # text file?
      extn <- "txt"
    } else if (grepl("\\.[Pp][Dd][Ff]$", filepath) == TRUE) {
      # pdf file?
      extn <- "pdf"
    } else {
      stop('path does not point to a valid file type (pdf/txt)')
    }

    # does filepath point to a local file or a url?

    if (grepl("[Hh][Tt][Tt][Pp]", filepath) == TRUE) {
      # path is a url, attempt to download file

      filedest <-
        paste0("~/sua_order.", extn) # download file with extension
        utils::download.file(filepath, filedest, mode = "wb")
        filepath <- filedest
    }

    # does the file exist in a directory?

    if (extn == "txt") {
      text <- readtext::readtext(filepath)
      if (length(text$text) == 1) {
        head <- stringr::str_sub(text$text, 1, regexpr("\n", text$text) - 1)
        line <- gregexpr(head, text$text)[[1]]
        temp <- NULL
        for (i in 1:(length(line) - 1)) {
          temp[i] <- stringr::str_sub(text$text, line[i], line[i + 1] - 1)
        }
        text <- temp
      }
    }

    if (extn == "pdf") {
      text <- pdftools::pdf_text(filepath)
    }

    return(text)

  }
