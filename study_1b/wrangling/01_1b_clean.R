library(here)
library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(readr)

# read in data from survey monkey, part 1. rename columns and remove unnecessary
# and potentially identifiable.
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

# recode verbal financial fine descriptions to dollars
d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Nothing, it was clearly an accident. She did',
                          ' not intend to light the table on fire. The',
                          ' restaurant should avoid using candles from',
                          ' now on.') & !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Having candles at the table is an assumed risk.',
                          ' Zero $.') & !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'nothing. it was an accident.' &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("What kind of insurance policy does not cover minor",
                          " fires?! Ew. But I'd say most if not all of it.",
                          " Accidents happen absolutely but her accident could",
                          " have cost them a lot more than a table cloth.") &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'None' &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == paste0('I think she should only',
                                                ' have to pay half of that') &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('The full $1,500, the fire was her fault even',
                          ' though it was an accident.') &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('I feel she needs to pay little to none',
                          ' so I would say $200') &
                       !is.na(d_sm_1$assess_fine)] <- '200'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('That seems like something the restaurant skills ',
                          'be prepared for, especially if they have candles. ',
                          'She should only pay for half') &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine == '90%' &
                       !is.na(d_sm_1$assess_fine)] <- '1350'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("Half since they shouldn't have open flames ",
                          "on the tables without insurance in the first ",
                          "place.") &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'Half' &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("That depends on Mrs. Smith's financial state. ",
                          "It seems perfectly reasonable to me that the ",
                          "restaurant repects her to help pay for the damages, ",
                          "even though the fire was started entirely by ",
                          "accident, but it wouldn't be reasonable for them ",
                          "to expect her to pay the entire amount if it ",
                          "wasn't within her financial means.") &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine == 'Full amount.' &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine == '60dcb2daf4fb4762bb1bfb0e' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("I don't believe she should be responsible ",
                          "for the cost.") &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'the cost of the tablecloth' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine == 'All' &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("None, there was no malice. That's against fire ",
                          "code, they should have made the flame less ",
                          "accessible or had the flame on non ",
                          "flammable things.") &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   'Pay it all up front or payments each month' &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Nothing--if her insurance covers it, that ',
                          'would be great but there should  be some blame on ',
                          'the restaurant as well.') &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   'the bill is very hig' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   'If found legally responsible then 100%' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('It was not intentional. And the restaurant ',
                          'chose to have candles on their tables knowing ',
                          'the inherent risk. Therefore, cost should be ',
                          'divided in half by Mrs. Smith and the restaurant.') &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Mrs. Smith should have to Pay only for damages ',
                          'to the carpet.') &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('None, the restaurant should not have open ',
                          'flames on the table.') &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == '1' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine == 'NO' &
                       !is.na(d_sm_1$assess_fine)] <- NA

# clean financial fines
d_sm_1$assess_fine %<>%
    stringr::str_replace_all(.,
                             pattern = '\\$',
                             replacement = '') %>%
    stringr::str_trim(.,
                      side = 'both') %>%
    stringr::str_replace_all(.,
                             pattern = ',',
                             replacement = '') %>%
    stringr::str_replace_all(.,
                             pattern = '\\.\\d+',
                             replacement = '') %>%
    as.numeric(.)

# read in data from survey monkey, part 2. rename columns and remove unnecessary
# and potentially identifiable.
d_sm_2 <- readxl::read_xlsx(here::here('data',
                                       'raw',
                                       'survey',
                                       'study_1a_data_pt2.xlsx')) %>%
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
                             'assess_blame',
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender')) %>%
    dplyr::select(.,
                  -matches('remove_')) %>%
    mutate(.,
           'experimental_situation' = 'nonagentive')

# recode informed consent to logical
d_sm_2 %<>%
    dplyr::mutate(.,
                  across(.cols = 'informed_consent',
                         .fns = ~dplyr::case_when(stringr::
                                                  str_detect(.x,
                                                             'I have read.*') ~
                                                      T,
                                                  TRUE ~ F)))

# recode assessments to pure numeric
d_sm_2 %<>%
    dplyr::mutate(.,
                  dplyr::across(matches('blame|active_role'),
                                ~stringr::str_replace(string = .x,
                                                      pattern = ' \\(.*\\)',
                                                      replacement = ''))) %>%
    dplyr::mutate(.,
                  dplyr::across(matches('blame|active_role'),
                                as.integer))

# recode age to integer
d_sm_2 %<>%
    dplyr::mutate(.,
                  dplyr::across('age',
                                as.integer))

# recode verbal financial fine descriptions to dollars
d_sm_2$assess_fine[d_sm_2$assess_fine == "None, maybe don't use candles..." &
                   !is.na(d_sm_2$assess_fine)] <- '0'

# clean financial fines
d_sm_2$assess_fine %<>%
    stringr::str_replace_all(.,
                             pattern = '\\$',
                             replacement = '') %>%
    stringr::str_trim(.,
                      side = 'both') %>%
    stringr::str_replace_all(.,
                             pattern = ',',
                             replacement = '') %>%
    stringr::str_replace_all(.,
                             pattern = '\\.\\d+',
                             replacement = '') %>%
    as.numeric(.)

# read in prolific data, part 1. remove unnecessary columns.
d_prolific_1 <- readr::read_csv(here::here('data',
                                           'raw',
                                           'prolific',
                                           'study_1a_prolific_pt1.csv')) %>%
    janitor::clean_names(.) %>%
    dplyr::select(.,
                  'prolific_id' = participant_id,
                  status)

# read in prolific data, part 2. remove unnecessary columns.
d_prolific_2 <- readr::read_csv(here::here('data',
                                           'raw',
                                           'prolific',
                                           'study_1a_prolific_pt2.csv')) %>%
    janitor::clean_names(.) %>%
    dplyr::select(.,
                  'prolific_id' = participant_id,
                  status)
