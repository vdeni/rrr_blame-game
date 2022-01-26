library(here)
library(readr)
library(magrittr)
library(dplyr)

.fine_lim_hrk <- 10200

d_s2a <- readr::read_csv(here('analyses',
                              'study_2a',
                              'data',
                              'clean',
                              'study_2a.csv'))

.d_s2a_nstart <- nrow(d_s2a)

# filter out paricipants who have entered values above the exclusion criterion
d_s2a %<>%
    dplyr::filter(.,
                  assess_fine < .fine_lim_hrk | is.na(assess_fine))

.d_s2a_ngthan <- .d_s2a_nstart - nrow(d_s2a)

# filter out participants who haven't stated a fine
d_s2a %<>%
    filter(.,
           !is.na(assess_fine))

.d_s2a_nas <- .d_s2a_nstart - .d_s2a_ngthan - nrow(d_s2a)

d_s2a %<>%
    filter(.,
           !recently_participated_similar %in% c(NA, 2))

.d_s2a_partic_similar <- .d_s2a_nstart - .d_s2a_ngthan - .d_s2a_nas - nrow(d_s2a)

d_s2a %<>%
    dplyr::mutate(.,
                  dplyr::across('blame_level',
                                as.factor))
