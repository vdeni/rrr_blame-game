library(here)
library(dplyr)
library(magrittr)
library(readxl)
library(stringr)
library(readr)

# read in data from survey monkey, part 1. rename columns and remove unnecessary
# and potentially identifiable.
d_sm <- readxl::read_xlsx(here::here('study_1a',
                                     'data',
                                     'raw',
                                     'survey',
                                     'study_1a_data.xlsx')) %>%
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
                             'experimental_situation',
                             'assess_blame',
                             'assess_fine',
                             'assess_active_role',
                             'age',
                             'gender')) %>%
    dplyr::select(.,
                  -matches('remove_'))

# recode informed consent to logical
d_sm %<>%
    dplyr::mutate(.,
                  dplyr::across(.cols = 'informed_consent',
                                .fns = ~ifelse(.x == 1,
                                               T,
                                               F)))

# recode experimental situations
d_sm %<>%
    mutate(.,
           across(.cols = 'experimental_situation',
                  .fns = ~dplyr::case_when(.x == 1 ~ 'agentive',
                                           .x == 2 ~ 'nonagentive',
                                           is.na(.x) ~ NA_character_)))

# cast blame and active role assessment to integer
d_sm %<>%
    mutate(.,
           across(.cols = dplyr::matches('blame|active_role'),
                  .fns = as.integer))

# cast age as integer
d_sm %<>%
    mutate(.,
           across(.cols = 'age',
                  .fns = as.integer))

# recode gender to character
d_sm %<>%
    mutate(.,
           across(.cols = 'gender',
                  .fns = ~case_when(.x == '1' ~ 'man',
                                    .x == '2' ~ 'woman',
                                    .x == '3' ~ 'other',
                                    .x == '4' ~ 'do not want to disclose',
                                    is.na(.x) ~ NA_character_)))

# recode verbal financial fine descriptions to dollars
.full <- 3400

# i don't know
d_sm$assess_fine[d_sm$assess_fine == 'Ne znam' &
                     !is.na(d_sm$assess_fine)] <- NA

# 500 HRK, symbolically
d_sm$assess_fine[d_sm$assess_fine == '500 kn simbolicno' &
                     !is.na(d_sm$assess_fine)] <- 500

# if she's in a good financial situation, she could offer to pay for the damage,
# otherwise, the restaurant should pay for the damage because it could have
# happened to anyone
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ukoliko je gospođa u dobroj financijskoj situaciji ',
                        'mogla bi se ponuditi da plati štetu, ali u protivnom ',
                        'bi restoran trebao snositi štetu jer se to moglo ',
                        'dogoditi bilo kome.') &
                  !is.na(d_sm$assess_fine)] <- NA

# the full amount
d_sm$assess_fine[d_sm$assess_fine == 'Cijeli iznos' &
                     !is.na(d_sm$assess_fine)] <- .full

# full
d_sm$assess_fine[d_sm$assess_fine == 'Puni' &
                     !is.na(d_sm$assess_fine)] <- .full

# zero
d_sm$assess_fine[d_sm$assess_fine == 'Nula' &
                     !is.na(d_sm$assess_fine)] <- 0

# she should pay the full amonut. even though the fire was caused accidentally,
# she has some blame while the restaurant has none
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Trebala bi platiti puni iznos. Iako je požar izazvan ',
                        'slučajnošću, ona ima neku krivicu, dok restoran ima ',
                        'nikakvu.') &
                  !is.na(d_sm$assess_fine)] <- .full

# half
d_sm$assess_fine[d_sm$assess_fine == 'Pola' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# the full amount
d_sm$assess_fine[d_sm$assess_fine == 'Puni iznos.' &
                     !is.na(d_sm$assess_fine)] <- .full

# nothing
d_sm$assess_fine[d_sm$assess_fine == 'Ništa' &
                     !is.na(d_sm$assess_fine)] <- 0

# half of the amount
d_sm$assess_fine[d_sm$assess_fine == 'Pola iznosa' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# half
d_sm$assess_fine[d_sm$assess_fine == 'Polovicu' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == '100%' &
                     !is.na(d_sm$assess_fine)] <- .full

# she shouldn't pay anything, that's the restaurants responsiblity - to take
# care of the damages caused by unforseen situations
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ne bi trebala ništa platiti, to je odgovornost ',
                        'restorana - da sanira štete nastale nepredviđenim ',
                        'situacijama.') &
                  !is.na(d_sm$assess_fine)] <- 0

# full, that is 3400 HRK
d_sm$assess_fine[d_sm$assess_fine == 'puni, dakle 3400kn' &
                     !is.na(d_sm$assess_fine)] <- .full

# make a deal to pay for half or less
d_sm$assess_fine[d_sm$assess_fine == 'Dogovoriti se za pola ili manje' &
                     !is.na(d_sm$assess_fine)] <- NA

# less than 3400
d_sm$assess_fine[d_sm$assess_fine == 'Manji od 3400' &
                     !is.na(d_sm$assess_fine)] <- NA

# half of the amount, 1700 HRK
d_sm$assess_fine[d_sm$assess_fine == 'Polovicu iznosa, 1700 kn' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# some smaller amount
d_sm$assess_fine[d_sm$assess_fine == 'neku manju cifru' &
                     !is.na(d_sm$assess_fine)] <- NA

# full
d_sm$assess_fine[d_sm$assess_fine == 'Cijeli' &
                     !is.na(d_sm$assess_fine)] <- .full

# at least half
d_sm$assess_fine[d_sm$assess_fine == 'minimalno pola' &
                     !is.na(d_sm$assess_fine)] <- NA

# nothing
d_sm$assess_fine[d_sm$assess_fine == 'Nista' &
                     !is.na(d_sm$assess_fine)] <- 0

# complete
d_sm$assess_fine[d_sm$assess_fine == 'Potpuni' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == '1700 max' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '50%' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# a third
d_sm$assess_fine[d_sm$assess_fine == 'Trecinu' &
                     !is.na(d_sm$assess_fine)] <- .full * 1 / 3

# the whole price
d_sm$assess_fine[d_sm$assess_fine == 'Punu cijenu.' &
                     !is.na(d_sm$assess_fine)] <- .full

# nothing
d_sm$assess_fine[d_sm$assess_fine == 'Ništa.' &
                     !is.na(d_sm$assess_fine)] <- 0

# half of the amount
d_sm$assess_fine[d_sm$assess_fine == 'Polovicu iznosa' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# 3400 + legal fees
d_sm$assess_fine[d_sm$assess_fine == '3400 + sudski troškovi' &
                     !is.na(d_sm$assess_fine)] <- .full

# some small amount to cover the damage
d_sm$assess_fine[d_sm$assess_fine == 'Neki sitan iznos za nadoknadu štete' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '55%' &
                     !is.na(d_sm$assess_fine)] <- .full * .55

# she shold only pay for the damage
d_sm$assess_fine[d_sm$assess_fine == 'Trebala bi platit samo za tu štetu' &
                     !is.na(d_sm$assess_fine)] <- .full

# she should not pay for the expense
d_sm$assess_fine[d_sm$assess_fine == 'Nebi trebala platiti trošak' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == '65%' &
                     !is.na(d_sm$assess_fine)] <- .full * .65

# 1450 HRK (half)
d_sm$assess_fine[d_sm$assess_fine == '1450 kuna (pola)' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# at most 400 HRK, but se shouldn't pay anything
d_sm$assess_fine[d_sm$assess_fine ==
                 'Najviše 400 kn ali ne bi ni trebala ista platiti' &
                     !is.na(d_sm$assess_fine)] <- NA

# at least 80% of the total amount, that is 2720 HRK
d_sm$assess_fine[d_sm$assess_fine ==
                 'Barem 80% ukupnog iznosa, dakle 2720 kuna' &
                     !is.na(d_sm$assess_fine)] <- .full * .8

# half
d_sm$assess_fine[d_sm$assess_fine == 'pola' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == '1000-1600' &
                     !is.na(d_sm$assess_fine)] <- NA

# the full amount
d_sm$assess_fine[d_sm$assess_fine == 'Ukupan iznos' &
                     !is.na(d_sm$assess_fine)] <- .full

# 0 HRK, if the insurance policy of the restaurant doesn't cover minor fires,
# the owner shouldn't have put candles on the tables
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('0 kn, ako poloca osiguranja restorana ne pokriva ',
                        'manje požare, vlasnik (menađer) nije smio ostaviti ',
                        'svijeće na stolovima.') &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == '40%' &
                     !is.na(d_sm$assess_fine)] <- .full * .4

# half the amount
d_sm$assess_fine[d_sm$assess_fine == 'Polovičan iznos' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

# less
d_sm$assess_fine[d_sm$assess_fine == 'Manje.' &
                     !is.na(d_sm$assess_fine)] <- NA

# 0. the woman is obviously clumsy and those things happen. it's the owner's
# fault that the fire happend, because of a poor choice of decoration. no flame
# = no fire. i understand that the owner is upset and that the damage is large.
# but, it's not the lady's fault.
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('0. Zena je ocito \"seprtlja\" i takve stvari se ',
                        'dogadaju. Krivnja je vlasnika sto je doslo do ',
                        'pozara sa losim izborom \'dekoracije\'. Nema plamena = ',
                        'nema pozara. Shvacam uzrujanost vlasnika i da je ',
                        'steta velika. Ali gospoda nije kriva, koliko god bi ',
                        'on to \'htjeo\'.') &
                     !is.na(d_sm$assess_fine)] <- 0

# how much she can, also depending on how much the restaurant can, how fancy it
# is
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Koliko može, ovisno koliko i restoran može, koliko ',
                        'je \"fancy\"') &
                     !is.na(d_sm$assess_fine)] <- NA

# at least 2500 HRK
d_sm$assess_fine[d_sm$assess_fine == 'Minimalno 2500 kuna.' &
                     !is.na(d_sm$assess_fine)] <- NA

# depending on her financial situation, this would be the appropriate price,
# maybe a little on the more expensive side
d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ovisno o njenom financinskom stanju ovo bi bila ',
                        'prikladna cijena možda malo na skuplju stranu') &
                     !is.na(d_sm$assess_fine)] <- NA

# it depends
d_sm$assess_fine[d_sm$assess_fine == 'ovisi' &
                     !is.na(d_sm$assess_fine)] <- NA

# clean fines
d_sm$assess_fine %<>%
    stringr::str_replace_all(.,
                             '\\s',
                             '') %>%
    str_replace(.,
                'kn|kuna',
                '') %>%
    str_replace(.,
                '4,000',
                '4000') %>%
    str_replace(.,
                '971,43',
                '971.43') %>%
    as.numeric(.)

readr::write_csv(d_sm,
                 here('study_1a',
                      'data',
                      'clean',
                      'study_1a.csv'))
