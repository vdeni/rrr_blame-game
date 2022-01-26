library(here)
library(readr)
library(magrittr)
library(dplyr)

.fine_lim_usd <- 4500

d_s1b <- readr::read_csv(here::here('analyses',
                                    'study_1b',
                                    'data',
                                    'clean',
                                    'study_1b.csv'))

.d_s1b_nstart <- nrow(d_s1b)

# filter out paricipants who have entered values above the exclusion criterion
d_s1b %<>%
    dplyr::filter(.,
                  assess_fine < .fine_lim_usd | is.na(assess_fine))

.d_s1b_ngthan <- .d_s1b_nstart - nrow(d_s1b)

# filter out participants who haven't entered one of the assessments
d_s1b %<>%
    filter(.,
           !is.na(assess_fine) & !is.na(assess_blame))

.d_s1b_nas <- .d_s1b_nstart - .d_s1b_ngthan - nrow(d_s1b)

d_s1b %<>%
    dplyr::mutate(.,
                  dplyr::across('experimental_situation',
                                as.factor))
