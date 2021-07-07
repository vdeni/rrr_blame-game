library(here)
library(dplyr)
library(magrittr)
library(readxl)

# read in data. rename columns and remove unnecessary and potentially
# identifiable.
d_sm_1 <- readxl::read_xlsx(here::here('data',
                                       'raw',
                                       'survey',
                                       'study_1a_data_pt1.xlsx')) %>%
    dplyr::slice(.,
                 -1) %>%
    magrittr::set_colnames(.,
                           c('respondent_id',
                             'collector_id',
                             'start_date',
                             'end_date',
                             'remove_1',
                             'remove_2',
                             'remove_3',
                             'remove_4',
                             'remove_5',
                             'informed_consent',
                             'prolific_id',
                             'experimental_situation',
                             'assess_blame',
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender')) %>%
    dplyr::select(.,
                  -matches('remove_'))

# recode informed consent to logical
d_sm_1 %<>%
    dplyr::mutate(.,
                  across(.cols = 'informed_consent',
                         .fns = ~dplyr::case_when(stringr::
                                                  str_detect(.x,
                                                             'I have read.*') ~
                                                      T,
                                                  TRUE ~ F)))

# recode experimental situations
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across('experimental_situation',
                                ~dplyr::case_when(stringr::
                                                  str_detect(.x,
                                                             'she flopped') ~
                                                      'agentive',
                                                  stringr::
                                                  str_detect(.x,
                                                             'napkin flopped') ~
                                                       'nonagentive',
                                                  TRUE ~ .x)))

# recode assessments to pure numeric
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across(matches('blame|active_role'),
                                ~stringr::str_replace(string = .x,
                                                      pattern = ' \\(.*\\)',
                                                      replacement = ''))) %>%
    dplyr::mutate(.,
                  dplyr::across(matches('blame|active_role'),
                                as.integer))

# recode age to integer
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across('age',
                                as.integer))
