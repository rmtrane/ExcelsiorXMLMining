#' Unfold answers from data
#'
#' The output from 'get_data_from_xml' includes a list column with xml_nodesets called 'Results'. This
#' column has all the information about questions and answers.
#' 'xml_get_answers' extracts the 'Question' and 'Answer' information from said column,
#' using the function 'get_answer_w_units'.
#'
#' @param tib0 result of get_data_from_xml
#' @param show_progress logical; if FALSE (default), progress bar is not shown
#'
#' @export

xml_get_answers <- function(tib0,
                            show_progress = FALSE){

  ## Print "Total number of reports".
  cat(paste("Total number of reports:", nrow(tib0), '\n'))


  ## If show_progress, initiate progress bar.
  if(show_progress)
    pb <- progress::progress_bar$new(total = nrow(tib0))


  ## This will be the output.
  tib <- tib0 %>%
    mutate(Results = map(Results,
                         function(x = .x){
                           ## We first create a tibble with all children in 'results'
                           tmp0 <- tibble(results = xml_children(x),
                                          name = xml_name(results)) %>%
                             ## To account for the structure of primary results (where 'PerformedBy' and 'PerformedDate' are in the results),
                             ## we filter so we only look at 'Results'.
                             filter(name == 'Result') %>%
                             ## Get laterality, Question, and Question group from the 'results' entry
                             mutate(Laterality = xml_child(results, "Laterality") %>% xml_text,
                                    Question = xml_child(results, "Question") %>% xml_text,
                                    Q_Group = xml_child(results, "Group") %>% xml_text,
                                    ## Get answers and units using the 'get_answer_w_units' function
                                    Answers = map(results, get_answer_w_units)
                             ) %>%
                             select(-results) %>%
                             nest(-Laterality, .key = 'QA')

                           if(show_progress)
                             pb$tick()

                           return(tmp0)
                         }))
  return(tib)
}

