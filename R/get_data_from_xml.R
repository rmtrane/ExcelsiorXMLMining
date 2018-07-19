#' Extract data from xml document
#'
#' From an xml document, get relevant characteristics and a list of results. Designed to work on the output of 'xml_to_tibble'.
#'
#' @param XML_document the xml_document part of a read_xml output from which data is to be extracted
#' @param n_head limit output to first n_head grading reports
#' @param primary logical; are we reading primary grading results?
#'
#' @export

get_data_from_xml <- function(XML_document, primary = FALSE, n_head = NA){

  ## First, find all XML documents.
  tib <- tibble(XML = xml_find_all(XML_document, "//GradingReport"))

  ## If n_head is specified, we only take the first n_head rows
  if(!is.na(n_head)){
    tib <- head(tib, n = n_head)
  }

  if(!primary){
    tib <- tib %>%
      mutate(Study               = XML %>% xml_child('Study') %>% xml_child('StudyName') %>% xml_text,
             Sponsors            = XML %>% xml_child('Sponsors') %>% xml_child('Sponsor') %>% xml_text,
             RandomizedSiteId    = XML %>% xml_child('Site') %>% xml_child('RandomizedSiteId') %>% xml_text,
             RandomizedSubjectId = XML %>% xml_child('Subject') %>% xml_child('RandomizedSubjectId') %>% xml_text,
             TimePoint           = XML %>% xml_child('TimePoint') %>% xml_text,
             StudyDate           = XML %>% xml_child('StudyDate') %>% xml_text,
             Procedure           = XML %>% xml_child('Procedure') %>% xml_text,
             Results             = XML %>% xml_child('Results'),
             PerformedBy         = Results %>% xml_attr(attr = 'PerformedBy'),
             PerformedDate       = Results %>% xml_attr(attr = 'PerformedDate')) %>%
      select(-XML)
  }

  if(primary){
    tib <- tib %>%
      mutate(Study               = XML %>% xml_child('Study') %>% xml_child('StudyName') %>% xml_text,
             Sponsors            = XML %>% xml_child('Sponsors') %>% xml_child('Sponsor') %>% xml_text,
             RandomizedSiteId    = XML %>% xml_child('Site') %>% xml_child('RandomizedSiteId') %>% xml_text,
             RandomizedSubjectId = XML %>% xml_child('Subject') %>% xml_child('RandomizedSubjectId') %>% xml_text,
             TimePoint           = XML %>% xml_child('TimePoint') %>% xml_text,
             StudyDate           = XML %>% xml_child('StudyDate') %>% xml_text,
             Procedure           = XML %>% xml_child('Procedure') %>% xml_text,
             Results             = XML %>% xml_child('Results'),
             PerformedBy         = Results %>% xml_child('PerformedBy') %>% xml_text,
             PerformedDate       = Results %>% xml_child('PerformedDate') %>% xml_text) %>%
      select(-XML)
  }

  return(tib)

}

