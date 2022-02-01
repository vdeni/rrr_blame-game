s1Table <- function(.d_summary,
                    .d_fine,
                    .d_blame,
                    .t_fine,
                    .t_blame) {

    .d_fine %<>%
        round(.,
              3)

    .d_blame %<>%
        round(.,
              3)

    .round <- 3

    .table <- matrix('x',
                     nrow = 2,
                     ncol = 3,
                     dimnames = list(c('Fine',
                                       'Blame'),
                                     c('Agentive',
                                       'Nonagentive',
                                       '')))

    .dimnames <- dimnames(.table)

    for (i in 1:nrow(.table)) {
        for (j in 1:(ncol(.table) - 1)) {
            .rowname <- .dimnames[[1]][i]
            .colname <- .dimnames[[2]][j]

            .write <- .d_summary %>%
                dplyr::filter(.,
                              assessment == tolower(.rowname) &
                                  experimental_situation == tolower(.colname)) %>%
                glue::glue_data(.,
                                '{round(m, .round)} ({round(stdev, .round)})')

            .table[i, j] <- .write
        }
    }

    .table['Fine', 3] <- glue::glue('t({round(.t_fine$parameter, .round)}) = ',
                                    '{round(.t_fine$statistic, .round)}, ',
                                    'p = {round(.t_fine$p.value, .round)}, ',
                                    'd = {.d_fine}')

    .table['Blame', 3] <- glue::glue('t({round(.t_blame$parameter, .round)}) = ',
                                    '{round(.t_blame$statistic, .round)}, ',
                                    'p = {round(.t_blame$p.value, .round)}, ',
                                    'd = {.d_blame}')

    return(knitr::kable(.table))
}

s2Table <- function(.d_summary) {
    .digits <- 3

    # browser()

    .d_summary <- dplyr::mutate(.d_summary,
                                dplyr::across(matches('stdev'),
                                              ~paste0('(',
                                                      round(.x,
                                                            .digits),
                                                      ')')))

    .d_summary <- mutate(.d_summary,
                         across(matches('^m'),
                                round,
                                .digits))

    .d_summary$activity <- glue::glue_data(.d_summary,
                                           '{m_activity} {stdev_activity}')

    .d_summary %<>%
        dplyr::select(.,
                      -c('m_activity',
                         'stdev_activity'))

    .d_summary$fine <- glue::glue_data(.d_summary,
                                       '{m_fine} {stdev_fine}')

    .d_summary %<>%
        dplyr::select(.,
                      -c('m_fine',
                         'stdev_fine'))

    .d_summary <- dplyr::select(.d_summary,
                                agency,
                                blame_level,
                                activity,
                                fine)

    colnames(.d_summary) <- c('Agency',
                              'Assigned blame',
                              'Mean activity assessment (SD)',
                              'Mean fine (SD)')

    return(knitr::kable(.d_summary))
}
