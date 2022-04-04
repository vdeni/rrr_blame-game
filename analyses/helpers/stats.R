extractCohenDSamples <- function(.data,
                                 .dependent) {

    group1 <- .data %>%
        dplyr::filter(.,
                      experimental_situation == 'agentive') %>%
        dplyr::pull(.,
                    paste0('assess_',
                           .dependent))

    group2 <- .data %>%
        dplyr::filter(.,
                      experimental_situation == 'nonagentive') %>%
        dplyr::pull(.,
                    paste0('assess_',
                           .dependent))

    out <- data.frame('response' = c(group1,
                                      group2),
                      'group' = c(rep('agentive',
                                      length(group1)),
                                  rep('nonagentive',
                                      length(group2))))

    out$idx <- 1:nrow(out)

    return(out)
}

getCohenD <- function(.data,
                      .idx) {
        .data <- .data[.idx, ] 

        m_agentive <- .data %>%
            filter(.,
                   group == 'agentive') %>%
            pull(.,
                 response) %>%
            mean(.,
                 na.rm = T)

        sd_agentive <- .data %>%
            filter(.,
                   group == 'agentive') %>%
            pull(.,
                 response) %>%
            sd(.,
               na.rm = T)

        n_agentive <- .data %>%
            filter(.,
                   group == 'agentive') %>%
            pull(.,
                 response) %>%
            na.omit(.) %>%
            length(.)

        m_nonagentive <- .data %>%
            filter(.,
                   group == 'nonagentive') %>%
            pull(.,
                 response) %>%
            mean(.,
                 na.rm = T)

        sd_nonagentive <- .data %>%
            filter(.,
                   group == 'nonagentive') %>%
            pull(.,
                 response) %>%
            sd(.,
               na.rm = T)

        n_nonagentive <- .data %>%
            filter(.,
                   group == 'nonagentive') %>%
            pull(.,
                 response) %>%
            na.omit(.) %>%
            length(.)

        sd_pooled <- sqrt(((n_agentive - 1) * sd_agentive^2 +
                           (n_nonagentive - 1) * sd_nonagentive^2) /
                          (n_agentive + n_nonagentive - 2))

        return((m_agentive - m_nonagentive) / sd_pooled)
}

getEtaSquared <- function(.anova_table,
                          .effect) {
        ss_between <- .anova_table[.effect, 'Sum Sq']

        ss_total <- sum(.anova_table[, 'Sum Sq']) -
            .anova_table['(Intercept)', 'Sum Sq']

        return(ss_between / ss_total)
}
