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

    return(list(group1,
                group2))
}

getCohenD <- function(group1,
                      group2) {
        m1 <- mean(group1)
        sd1 <- sd(group1)
        n1 <- length(group1)

        m2 <- mean(group2)
        sd2 <- sd(group2)
        n2 <- length(group2)

        sd_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) /
                          (n1 + n2 - 2))

        return((m1 - m2) / sd_pooled)
}

getEtaSquared <- function(.anova_table,
                          .effect) {
        ss_between <- .anova_table[.effect, 'Sum Sq']

        ss_total <- sum(.anova_table[, 'Sum Sq'])

        return(ss_between / ss_total)
}
