library(here)
library(readr)
library(magrittr)
library(dplyr)

.fine_lim_hrk <- 10200

d_s1a <- readr::read_csv(here::here('analyses',
                                    'study_1a',
                                    'data',
                                    'clean',
                                    'study_1a.csv'))

.d_s1a_nstart <- nrow(d_s1a)

# filter out paricipants who have entered values above the exclusion criterion
d_s1a %<>%
    dplyr::filter(.,
                  assess_fine < .fine_lim_hrk | is.na(assess_fine))

.d_s1a_ngthan <- .d_s1a_nstart - nrow(d_s1a)

# filter out participants who haven't entered one of the assessments
d_s1a %<>%
    filter(.,
           !is.na(assess_fine) & !is.na(assess_blame))

.d_s1a_nas <- .d_s1a_nstart - .d_s1a_ngthan - nrow(d_s1a)

d_s1a %<>%
    dplyr::mutate(.,
                  dplyr::across('experimental_situation',
                                as.factor))
