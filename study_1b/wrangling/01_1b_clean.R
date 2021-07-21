# load survey data and prolific data, clean it
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
                                       'study_1b_data_pt1.xlsx')) %>%
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
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender')) %>%
    dplyr::select(.,
                  -matches('remove_'))

# recode informed consent to logical
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across(.cols = 'informed_consent',
                                .fns =
                                    ~dplyr::
                                    case_when(stringr::
                                              str_detect(.x,
                                                         'I have read.*') ~ T,
                                                          TRUE ~ F)))

# recode assessments to pure numeric
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across('assess_active_role',
                                as.integer))

# recode age to integer
d_sm_1 %<>%
    dplyr::mutate(.,
                  dplyr::across('age',
                                as.integer))

# recode verbal financial fine descriptions to dollars
d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Mrs. Smith should not be required to pay for any ',
                          'of the damages. The fire was completely accidental ',
                          'and a result of the restaurant having open ',
                          'flames near flammable materials.') &
                         !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('1/8 of $1500.00') & !is.na(d_sm_1$assess_fine)] <-
                       '187.5'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("I'm not entirely sure in my opinion she should ",
                          "be required to pay anything.  In my opinion it was ",
                          "a very unfortunate accident.  If they would like ",
                          "to avoid minor fires that they are not covered ",
                          "by insurance, then they should not use candles ",
                          "in their dining setting.  But, if I were to put ",
                          "a figure as required by the question.  I'd say ",
                          "half of the 1,500 dollars since she got a 4 out ",
                          "of 8 on the insurance assessment.") &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine == '70% maybe' &
                       !is.na(d_sm_1$assess_fine)] <- '1050'

d_sm_1$assess_fine[d_sm_1$assess_fine == paste0('1500 she should not have ',
                                                'put it on the candle') &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   'She should be required to pay for half of it $7,500' &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0('Mrs. Smith should be required to pay ',
                          'everything considering it was her wrongdoing.') &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine == '1% of the total cost.' &
                       !is.na(d_sm_1$assess_fine)] <- '15'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'Half of the 1,500' &
                       !is.na(d_sm_1$assess_fine)] <- '750'

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("I would think Mrs. Smith would have to pay ",
                          "the entire fee. The restaurant didn’t have ",
                          "the insurance to cover the incident; therefore, ",
                          "she should have to cover the costs.") &
                       !is.na(d_sm_1$assess_fine)] <- '1500'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'o' &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'None' &
                       !is.na(d_sm_1$assess_fine)] <- '0'

d_sm_1$assess_fine[d_sm_1$assess_fine == 'At least a thousand' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine ==
                   paste0("According to the math of the problem, the ",
                          "restaurant wants her to pay 750.00 however ",
                          "I believe it’s the restaurant’s fault for ",
                          "having such a hazard with no forethought or ",
                          "insurance coverage for their dining table props.") &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine == '750-1000' &
                       !is.na(d_sm_1$assess_fine)] <- NA

d_sm_1$assess_fine[d_sm_1$assess_fine == '50%' &
                       !is.na(d_sm_1$assess_fine)] <- '750'

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
    as.numeric(.)

# read in data from survey monkey, part 2. rename columns and remove unnecessary
# and potentially identifiable.
d_sm_2 <- readxl::read_xlsx(here::here('data',
                                       'raw',
                                       'survey',
                                       'study_1b_data_pt2.xlsx')) %>%
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
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender')) %>%
    dplyr::select(.,
                  -matches('remove_'))

# recode informed consent to logical
d_sm_2 %<>%
    dplyr::mutate(.,
                  dplyr::
                  across(.cols = 'informed_consent',
                         .fns = ~dplyr::case_when(stringr::
                                                  str_detect(.x,
                                                             'I have read.*') ~
                                                      T,
                                                  TRUE ~ F)))

# recode assessments to pure numeric
d_sm_2 %<>%
    dplyr::mutate(.,
                  dplyr::across('assess_active_role',
                                as.integer))

# recode age to integer
d_sm_2 %<>%
    dplyr::mutate(.,
                  dplyr::across('age',
                                as.integer))

unique(d_sm_2$assess_fine)

# recode verbal financial fine descriptions to dollars
d_sm_2$assess_fine[d_sm_2$assess_fine ==
                   paste0('Because the review panel gave her a 7, she ',
                          'should be required to pay the full amount') &
                   !is.na(d_sm_2$assess_fine)] <- '1500'

# recode verbal financial fine descriptions to dollars
d_sm_2$assess_fine[d_sm_2$assess_fine ==
                   paste0('i think 1500 sounds about right. i mean it ',
                          'wasnt anyone elses fault') &
                   !is.na(d_sm_2$assess_fine)] <- '1500'

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
    as.numeric(.)

# read in prolific data, part 1. remove unnecessary columns.
d_prolific_1 <- readr::read_csv(here::here('data',
                                           'raw',
                                           'prolific',
                                           'study_1b_prolific_pt1.csv')) %>%
    janitor::clean_names(.) %>%
    dplyr::select(.,
                  'prolific_id' = participant_id,
                  status)

# read in prolific data, part 2. remove unnecessary columns.
d_prolific_2 <- readr::read_csv(here::here('data',
                                           'raw',
                                           'prolific',
                                           'study_1b_prolific_pt2.csv')) %>%
    janitor::clean_names(.) %>%
    dplyr::select(.,
                  'prolific_id' = participant_id,
                  status)
