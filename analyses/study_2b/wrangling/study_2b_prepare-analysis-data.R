library(here)
library(readr)
library(magrittr)
library(dplyr)

.fine_lim_usd <- 4500

d_s2b <- readr::read_csv(here('analyses',
                              'study_2b',
                              'data',
                              'clean',
                              'study_2b.csv'))

.d_s2b_nstart <- nrow(d_s2b)

# filter out paricipants who have entered values above the exclusion criterion
d_s2b %<>%
    dplyr::filter(.,
                  assess_fine < .fine_lim_usd | is.na(assess_fine))

.d_s2b_ngthan <- .d_s2b_nstart - nrow(d_s2b)

# filter out participants who haven't stated a fine
d_s2b %<>%
    filter(.,
           !is.na(assess_fine))

.d_s2b_nas <- .d_s2b_nstart - .d_s2b_ngthan - nrow(d_s2b)
