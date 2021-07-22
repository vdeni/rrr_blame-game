library(here)

source(here::here('wrangling',
                  '01_2b_clean.R'))

# merge to single tibble
d_sm <- dplyr::bind_rows(d_sm_1,
                         d_sm_2)

d_prolific <- dplyr::bind_rows(d_prolific_1,
                               d_prolific_2)

d <- dplyr::full_join(d_sm,
                      d_prolific,
                      by = 'prolific_id')

# exclude 'RETURNED' and 'TIMED OUT' participants, remove prolific id
d %<>%
    dplyr::filter(.,
                  !(status %in% c('TIMED-OUT', 'RETURNED') | is.na(status)) &
                      informed_consent == T) %>%
    dplyr::select(.,
                  -c('status',
                     'prolific_id'))

readr::write_csv(d,
                 here::here('data',
                            'clean',
                            'study_2b.csv'))
