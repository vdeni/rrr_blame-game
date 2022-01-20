# load survey data and prolific data, clean it
library(here)
library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(readr)
library(tidyr)
library(haven)

# read in data from survey monkey. rename columns and remove unnecessary
# and potentially identifiable.
d_sm <- haven::read_sav(here::here('study_2a',
                                   'data',
                                   'raw',
                                   'survey',
                                   'study_2a_data.sav')) %>%
    magrittr::set_colnames(.,
                           c('collector_number',
                             'respondent_id',
                             'collector_id',
                             'start_date',
                             'end_date',
                             'remove_1',
                             'informed_consent',
                             'experimental_situation',
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender',
                             'recently_participated_similar',
                             'remove_2')) %>%
    dplyr::select(.,
                  -matches('remove_'))

# recode informed consent to logical
d_sm %<>%
    dplyr::mutate(.,
                  dplyr::across(.cols = 'informed_consent',
                                .fns =
                                    ~dplyr::
                                    case_when(.x == 1 ~ T,
                                              TRUE ~ F)))

# recode age to integer
d_sm %<>%
    dplyr::mutate(.,
                  dplyr::across('age',
                                as.integer))

# recode gender to strings
d_sm %<>%
    dplyr::mutate(.,
                  dplyr::
                  across('gender',
                         ~dplyr::
                         case_when(.x == 1 ~ 'male',
                                   .x == 2 ~ 'female',
                                   .x == 3 ~ 'other',
                                   .x == 4 ~ 'I prefer not to answer',
                                   TRUE ~ NA_character_)))

# recode experimental situation to strings
d_sm %<>%
    dplyr::mutate(.,
                  dplyr::
                  across('experimental_situation',
                         ~dplyr::
                         case_when(.x == 1 ~ 'agentive_blame1',
                                   .x == 2 ~ 'agentive_blame4',
                                   .x == 3 ~ 'agentive_blame7',
                                   .x == 4 ~ 'nonagentive_blame1',
                                   .x == 5 ~ 'nonagentive_blame4',
                                   .x == 6 ~ 'nonagentive_blame7')))

# split experimental situation into two columns
d_sm %<>%
    tidyr::separate(.,
                    col = experimental_situation,
                    into = c('agency', 'blame_level'),
                    sep = '_',
                    remove = T) %>%
    dplyr::mutate(.,
                  dplyr::across(blame_level,
                                str_replace,
                                pattern = 'blame',
                                replacement = '')) %>%
    dplyr::mutate(.,
                  dplyr::across(c(blame_level, agency),
                                as.factor))

# recode verbal financial fine descriptions to dollars
.full <- 3400

d_sm$assess_fine[d_sm$assess_fine == '' & !is.na(d_sm$assess_fine)] <- NA

# 1-that is 3400 HRK
d_sm$assess_fine[d_sm$assess_fine == '1-odnosno 3400kn'
                 & !is.na(d_sm$assess_fine)] <- 3400

# meaningless
d_sm$assess_fine[d_sm$assess_fine == 'idk'
                 & !is.na(d_sm$assess_fine)] <- NA

# filters out responses which seem to be assessing the blame instead of the fine
d_sm$assess_fine[d_sm$assess_fine %in% as.character(1:8)
                 & !is.na(d_sm$assess_fine)] <- NA

# max 500 HRK
d_sm$assess_fine[d_sm$assess_fine == '500kn maksimalno'
                 & !is.na(d_sm$assess_fine)] <- NA

# she should pay her tab, apologize and not pay anything for the damage
d_sm$assess_fine[d_sm$assess_fine == paste0('Treba platiti svoj račun, ',
                                            'ispričati se i ne platiti odštetu.')
                 & !is.na(d_sm$assess_fine)] <- NA

# around 370-380 HRK
d_sm$assess_fine[d_sm$assess_fine == 'oko 370-380 kn'
                 & !is.na(d_sm$assess_fine)] <- 375

# she shouldn't
d_sm$assess_fine[d_sm$assess_fine == 'Nebi trebala'
                 & !is.na(d_sm$assess_fine)] <- 0

# 0 because it was an accident
d_sm$assess_fine[d_sm$assess_fine == '0 jer je bila nesreća'
                 & !is.na(d_sm$assess_fine)] <- 0

# split it based on the number of friends
d_sm$assess_fine[d_sm$assess_fine == 'Podijelit ovisno o broju prijatelja'
                 & !is.na(d_sm$assess_fine)] <- NA

# she shold pay at least half, she did almost destroy the restaurant
d_sm$assess_fine[d_sm$assess_fine == paste0('Ma nek plati barem pola ipak je ',
                                            'ljudima skoro uništila restoran')
                 & !is.na(d_sm$assess_fine)] <- NA

# not sure
d_sm$assess_fine[d_sm$assess_fine == 'Nisam sigurna'
                 & !is.na(d_sm$assess_fine)] <- NA

# the amount of damage made
d_sm$assess_fine[d_sm$assess_fine == 'Iznos nastale štete'
                 & !is.na(d_sm$assess_fine)] <- .full

# the full amount
d_sm$assess_fine[d_sm$assess_fine == 'Puni iznos'
                 & !is.na(d_sm$assess_fine)] <- .full

# as she was found not guilty (1 on the scale), she shouldn't pay anything
d_sm$assess_fine[d_sm$assess_fine == paste0('Budući da je procjenjeno kako ',
                                            'osoba nije kriva (broj 1 na ',
                                            'ljestvici), gospođa ne bi ',
                                            'trebala plaćat nikakvu naknadu ',
                                            'štete')
                 & !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == '35%'
                 & !is.na(d_sm$assess_fine)] <- .full * .35

d_sm$assess_fine[d_sm$assess_fine == '¸1700'
                 & !is.na(d_sm$assess_fine)] <- 1700

# she should pay the full amount because she endangered the lives of other
# people and damaged the restaurant's property
d_sm$assess_fine[d_sm$assess_fine == paste0('Smatram da bi gospoda Kovač ',
                                            'trebala platiti puni iznos jer ',
                                            'je ugrozila život drugih i ',
                                            'oštetila imovinu restorana.')
                 & !is.na(d_sm$assess_fine)] <- .full

# i think it would be fair if she paid half of the amount, that is 1700 HRK,
# because the restaurant put the candles on the table at their own risk
d_sm$assess_fine[d_sm$assess_fine == paste0('Mislim da bi bilo pravedno da ',
                                            'gospođa Kovač plati pola tog ',
                                            'iznosa, dakle 1700kn iz ',
                                            'razloga što restoran na svoj ',
                                            'rizik stavlja zapaljene svijeće ',
                                            'na stol.')
                 & !is.na(d_sm$assess_fine)] <- .full * .5

# full amount
d_sm$assess_fine[d_sm$assess_fine == 'Potpuni'
                 & !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'H'
                 & !is.na(d_sm$assess_fine)] <- NA

# not the full amount, but a part
d_sm$assess_fine[d_sm$assess_fine == 'ne puni iznos, ali dio'
                 & !is.na(d_sm$assess_fine)] <- NA

# as she was assessed with 7 out of 8, she should pay most of the amount, almost
# the whole amount, that is around 3000 HRK
d_sm$assess_fine[d_sm$assess_fine == paste0('Budući da su je ocijenili sa 7 ',
                                            'od 8 trebala bi platiti ',
                                            'većinu, skoro cijeli iznos ',
                                            'tj. oko 3000kn')
                 & !is.na(d_sm$assess_fine)] <- 3000

# the amount determined after the assessment
d_sm$assess_fine[d_sm$assess_fine == 'Onaj koji je određen nakon procjene'
                 & !is.na(d_sm$assess_fine)] <- .full

# the amount of damage done
d_sm$assess_fine[d_sm$assess_fine == 'iznos štete koju je pocinila'
                 & !is.na(d_sm$assess_fine)] <- .full

# nothing, as she's not guilty according to the scale
d_sm$assess_fine[d_sm$assess_fine == paste0('Ništa jer po toj ljestvici ne ',
                                            'ispada kriva...')
                 & !is.na(d_sm$assess_fine)] <- 0

# half
d_sm$assess_fine[d_sm$assess_fine == 'Pola'
                 & !is.na(d_sm$assess_fine)] <- .full * .5

# i think she was fined too much. she should pay for some damage, but not 3400
# HRK. personally, i'd rate her guilt with a 4 on the scale
d_sm$assess_fine[d_sm$assess_fine == paste0('Smatram da su gospođu Kovač ',
                                            'previše kaznili. Trebala bi ',
                                            'platit neku odštetu, ali ne u ',
                                            'iznosu od 3400 kuna. Osobno bi ',
                                            'njenu krivnju označila sa ',
                                            'brojem 4 na ljestvici procjenje ',
                                            'krivnje.')
                 & !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '7/8 * 3400'
                 & !is.na(d_sm$assess_fine)] <- .full * 7 / 8

d_sm$assess_fine[d_sm$assess_fine == 'h'
                 & !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'u'
                 & !is.na(d_sm$assess_fine)] <- NA

# someone writing obscenities
d_sm$assess_fine[d_sm$assess_fine == 'Sto osamdesed milijuna kuraca'
                 & !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '1.'
                 & !is.na(d_sm$assess_fine)] <- NA

# at least 50%
d_sm$assess_fine[d_sm$assess_fine == 'Barem 50%'
                 & !is.na(d_sm$assess_fine)] <- NA

# the price of the dinner
d_sm$assess_fine[d_sm$assess_fine == 'iznos vecere'
                 & !is.na(d_sm$assess_fine)] <- NA

# some smaller amount since it wasn't done on purpose. if a person who breaks a
# glass doesn't pay for the glass, i think mrs. Kovac also shouldn't have to pay
# for the carpet, the napking, the tablecloth or the candle. or, if she has to
# pay something, let it be the cost of the things she set on fire
d_sm$assess_fine[d_sm$assess_fine == paste0('Neki manji iznos s obzirom da ',
                                            'nije namjerno to napravila. ',
                                            'Mislim da ako osoba koja ',
                                            'razbije čašu ne plaća tu čašu ',
                                            'da nebi ni gospođa Kovač ',
                                            'trebala platiti tepih, salvetu, ',
                                            'stoljnjak ni svijeću. Ili ako ',
                                            'već mora platiti neka se ',
                                            'izračuna trošak tog šta je ',
                                            'zapalila.')
                 & !is.na(d_sm$assess_fine)] <- NA

# twice
d_sm$assess_fine[d_sm$assess_fine == 'Duplo'
                 & !is.na(d_sm$assess_fine)] <- .full * 2

# nothing
d_sm$assess_fine[d_sm$assess_fine == 'Ništa'
                 & !is.na(d_sm$assess_fine)] <- 0

# the full amount
d_sm$assess_fine[d_sm$assess_fine == 'Cijeli iznos'
                 & !is.na(d_sm$assess_fine)] <- .full

# dunno
d_sm$assess_fine[d_sm$assess_fine == 'Nez'
                 & !is.na(d_sm$assess_fine)] <- NA

# 20% of the price
d_sm$assess_fine[d_sm$assess_fine == '20% od cijene'
                 & !is.na(d_sm$assess_fine)] <- .full * .2

d_sm$assess_fine[d_sm$assess_fine == '20%,'
                 & !is.na(d_sm$assess_fine)] <- .full * .2

# meaningless
d_sm$assess_fine[d_sm$assess_fine == 'Nsjd'
                 & !is.na(d_sm$assess_fine)] <- NA

# unclear which number this should be
d_sm$assess_fine[d_sm$assess_fine == '29,750'
                 & !is.na(d_sm$assess_fine)] <- NA

# clean financial fines
d_sm$assess_fine %<>%
    stringr::str_replace_all(.,
                             pattern = stringr::regex('kuna|kn|hrk',
                                                      ignore_case = T),
                             replacement = '') %>%
    str_replace_all(.,
                    pattern = ',(?=\\d\\d)',
                    replacement = '.') %>%
    str_replace_all(.,
                    pattern = '\\s+',
                    replacement = '') %>%
    str_replace(.,
                pattern = '(?<=^\\d)\\.',
                replacement = '') %>%
    as.numeric(.)

readr::write_csv(d_sm,
                 here('study_2a',
                      'data',
                      'clean',
                      'study_2a.csv'))
