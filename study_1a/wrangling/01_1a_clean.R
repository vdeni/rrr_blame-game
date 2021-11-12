library(here)
library(dplyr)
library(magrittr)
library(readxl)
library(stringr)

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

d_sm$assess_fine[d_sm$assess_fine == 'Ne znam' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '500 kn simbolicno' &
                     !is.na(d_sm$assess_fine)] <- 500

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ukoliko je gospođa u dobroj financijskoj situaciji ',
                        'mogla bi se ponuditi da plati štetu, ali u protivnom ',
                        'bi restoran trebao snositi štetu jer se to moglo ',
                        'dogoditi bilo kome.') &
                  !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Cijeli iznos' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Puni' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Nula' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Trebala bi platiti puni iznos. Iako je požar izazvan ',
                        'slučajnošću, ona ima neku krivicu, dok restoran ima ',
                        'nikakvu.') &
                  !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Pola' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == 'Puni iznos.' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Ništa' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == 'Pola iznosa' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == 'Polovicu' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == '100%' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ne bi trebala ništa platiti, to je odgovornost ',
                        'restorana - da sanira štete nastale nepredviđenim ',
                        'situacijama.') &
                  !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == 'puni, dakle 3400kn' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Dogovoriti se za pola ili manje' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Manji od 3400' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Polovicu iznosa, 1700 kn' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == 'neku manju cifru' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Cijeli' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'minimalno pola' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Nista' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == 'Potpuni' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == '1700 max' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '50%' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == 'Trecinu' &
                     !is.na(d_sm$assess_fine)] <- .full * 1 / 3

d_sm$assess_fine[d_sm$assess_fine == 'Punu cijenu.' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Ništa.' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == 'Polovicu iznosa' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == '3400 + sudski troškovi' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Neki sitan iznos za nadoknadu štete' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == '55%' &
                     !is.na(d_sm$assess_fine)] <- .full * .55

d_sm$assess_fine[d_sm$assess_fine == 'Trebala bi platit samo za tu štetu' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine == 'Nebi trebala platiti trošak' &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == '65%' &
                     !is.na(d_sm$assess_fine)] <- .full * .65

d_sm$assess_fine[d_sm$assess_fine == '1450 kuna (pola)' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine ==
                 'Najviše 400 kn ali ne bi ni trebala ista platiti' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine ==
                 'Barem 80% ukupnog iznosa, dakle 2720 kuna' &
                     !is.na(d_sm$assess_fine)] <- .full * .8

d_sm$assess_fine[d_sm$assess_fine == 'pola' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == '1000-1600' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Ukupan iznos' &
                     !is.na(d_sm$assess_fine)] <- .full

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('0 kn, ako poloca osiguranja restorana ne pokriva ',
                        'manje požare, vlasnik (menađer) nije smio ostaviti ',
                        'svijeće na stolovima.') &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine == '40%' &
                     !is.na(d_sm$assess_fine)] <- .full * .4

d_sm$assess_fine[d_sm$assess_fine == 'Polovičan iznos' &
                     !is.na(d_sm$assess_fine)] <- .full * .5

d_sm$assess_fine[d_sm$assess_fine == 'Manje.' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('0. Zena je ocito \"seprtlja\" i takve stvari se ',
                        'dogadaju. Krivnja je vlasnika sto je doslo do ',
                        'pozara sa losim izborom \'dekoracije\'. Nema plamena = ',
                        'nema pozara. Shvacam uzrujanost vlasnika i da je ',
                        'steta velika. Ali gospoda nije kriva, koliko god bi ',
                        'on to \'htjeo\'.') &
                     !is.na(d_sm$assess_fine)] <- 0

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Koliko može, ovisno koliko i restoran može, koliko ',
                        'je \"fancy\"') &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'Minimalno 2500 kuna.' &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine ==
                 paste0('Ovisno o njenom financinskom stanju ovo bi bila ',
                        'prikladna cijena možda malo na skuplju stranu') &
                     !is.na(d_sm$assess_fine)] <- NA

d_sm$assess_fine[d_sm$assess_fine == 'ovisi' &
                     !is.na(d_sm$assess_fine)] <- NA

# clean fines
