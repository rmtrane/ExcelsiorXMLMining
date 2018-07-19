#' Create tibble from xml files
#'
#' Takes a path to a folder and returns a tibble with all grading reports from all .xml files at
#' the specified folder. If n_head is NA (default), all gradingreports are included. If n_head is a positive number,
#' only n_head gradingreports from each .xml file in folder 'path' is included.
#'
#' @param folder specify folder form which all .xml files are used
#' @param file specify single file to load
#' @param n_head limit output to first n_head grading reports (passed to get_data_from_xml)
#' @param primary logical; are we reading primary grading results? (primary = FALSE == variability reports. primary = TRUE == primary grading reports.)
#'
#' @export

xml_to_tibble <- function(folder = NULL, file = NULL, n_head = NA, primary = FALSE){

  if(is.null(folder) + is.null(file) != 1){
    warning("Only one of 'folder' and 'file' should be provided")
  }

  if(is.null(file)){
    tib0 <- as.list(list.files(path = folder, pattern = ".xml", full.names = TRUE)) %>%
      map_df(~read_xml(.x) %>%
               get_data_from_xml(n_head = n_head, primary = primary)
      )
  } else {
    tib0 <- read_xml(x = file) %>% get_data_from_xml(n = n_head, primary = primary)
  }

  return(tib0)
}

