#' Extract answers and corresponding units
#'
#' Gets answers and units from results part of xml grading reports
#'
#' @param results element from the 'results' list from the output of xml_get_answers
#'
#' @export

get_answer_w_units <- function(results){
  ## The fourth xml child of the xml nodeset
  tmp <- results %>% xml_child(4)

  ## If the name of this child is 'Answer', the question is categorical.
  ## For these questions we find the 'Measurement' as the text of the 'Question' child, the 'Name' (which in this case is for bookkeeping) is
  ## the same as the 'Measurement', and the 'Value' is the text of the tmp xml child.
  if(xml_name(tmp) == 'Answer'){
    tmp_tib <- tibble(Name = xml_child(results, 'Question') %>% xml_text,
                      Answer = list(tibble(Measurement = Name,
                                           Value = xml_text(tmp))),
                      Unit = '')
  } else {
    ## If the name of the child is NOT 'Answer', the question is numerical.
    ## We then proceed to get values for all subquestions, and units.
    tmp_tib <- tibble(children = tmp %>% xml_children) %>%
      mutate(children_children = map(children, xml_children),
             Name = map_chr(children, xml_name),
             value = map2(children_children, children,
                          function(x, y){
                            if(!length(xml_name(x))){
                              tibble(Measurement = xml_name(y), Value = xml_text(y))
                            } else {
                              tibble(Measurement = xml_name(x), Value = xml_text(x))
                            }
                          }),
             type = case_when(str_detect(Name, 'Unit') ~ 'Unit',
                              TRUE ~ 'Answer'),
             Name = str_replace(Name, 'Unit|Length', '')) %>%
      select(Name:type) %>%
      spread(type, value) %>%
      mutate(Unit = map_chr(Unit, ~ifelse(is.null(.x), '', pull(.x, Value))))
  }

  return(tmp_tib)
}
