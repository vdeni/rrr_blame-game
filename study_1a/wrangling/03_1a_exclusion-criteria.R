library(here)

source(here::here('wrangling',
                  '02_merge.R'))

d %<>%
    dplyr::filter(.,
                  assess_fine <= 4500 | is.na(assess_fine))

readr::write_csv(d,
                 here::here('data',
                            'clean',
                            'study_1a.csv'))
