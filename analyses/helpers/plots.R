# helpers for plotting
setLimitsUSD <- function(x) {
    if (max(x) > 7) {
        .upper <- ceiling(max(x) / 500) * 500

        return(c(0,
                 .upper))
    } else {
        return(c(1,
                 7))
    }
}

setLimitsHRK <- function(x) {
    if (max(x) > 7) {
        .upper <- ceiling(max(x) / 500) * 500

        return(c(0,
                 .upper))
    } else {
        return(c(1,
                 7))
    }
}

setBreaksMajor <- function (x) {
    if (max(x) > 8) {
        return(seq(0,
                   max(x),
                   by = 500))
    } else {
        return(seq(1,
                   7,
                   by = 1))
    }
}

setBreaksMinor <- function(x) {
    if (max(x) > 8) {
        return(seq(0,
                   max(x),
                   by = 250))
    } else {
        return(NULL)
    }
}

# plotting functions

# s1
s1Plot <- function(raw_data,
                   summary_data) {
    s_p1 <- dplyr::filter(summary_data,
                          assessment == 'fine')

    d_p1 <- filter(raw_data,
                   assessment == 'fine')

    p_1 <- ggplot2::ggplot(d_p1,
                           aes(x = experimental_situation,
                               y = value)) +
        ggdist::stat_halfeye(geom = 'slab',
                             width = .25,
                             justification = -.5,
                             adjust = .5, n = 1e4) +
        ggplot2::geom_boxplot(width = .10) +
        ggplot2::geom_point(size = 1.5,
                            alpha = .5,
                            shape = 1,
                            position = ggplot2::position_jitter(width = .10,
                                                                height = 0,
                                                                seed = 1)) +
        ggplot2::scale_x_discrete(labels = c('agentive' = 'Agentive',
                                             'nonagentive' = 'Nonagentive')) +
        ggplot2::labs(x = 'Experimental situation',
                      y = '') +
        ggplot2::theme(panel.grid.major.x = element_blank())
        ggplot2::scale_y_continuous(limits = setLimitsHRK,
                                    breaks = setBreaksMajor,
                                    minor_breaks = setBreaksMinor)

    return(p_1)

    # ggplot2::ggplot(raw_data,
    #                 aes(x = experimental_situation,
    #                     y = value)) +
    #     ggplot2::facet_wrap('assessment',
    #                         strip.position = 'left',
    #                         scales = 'free_y',
    #                         nrow = 2,
    #                         labeller = ggplot2::as_labeller(c('blame' = 'Blame',
    #                                                           'fine' = 'Fine'))) +
    #     geom_point(inherit.aes = F,
    #                data = summary_data,
    #                aes(x = experimental_situation,
    #                    y = m),
    #                shape = 4,
    #                stroke = 1,
    #                size = 3) +
    #     ggplot2::scale_y_continuous(limits = setLimitsHRK,
    #                                 breaks = setBreaksMajor,
    #                                 minor_breaks = setBreaksMinor) +
    #     ggplot2::scale_x_discrete(labels = c('agentive' = 'Agentive',
    #                                          'nonagentive' = 'Nonagentive')) +
    #     ggplot2::labs(x = 'Experimental situation',
    #                   y = '') +
    #     ggplot2::theme(panel.grid.major.x = element_blank())
}

# s2
s2Plot <- function(raw_data,
                   summary_data) {
    ggplot2::ggplot(raw_data,
                    aes(x = blame_level,
                        y = assess_fine,
                        shape = agency)) +
        ggplot2::geom_point(size = 1.5,
                            alpha = 1,
                            position = ggplot2::position_jitter(width = .35,
                                                                height = 0)) +
        geom_point(inherit.aes = F,
                   data = summary_data,
                   aes(x = blame_level,
                       y = m,
                       shape = agency),
                   stroke = 1,
                   size = 3,
                   show.legend = F) +
        ggplot2::scale_shape_manual(values = c('agentive' = 0,
                                               'nonagentive' = 2),
                                    labels = c('agentive' = 'Agentive',
                                               'nonagentive' = 'Nonagentive'),
                                    name = 'Agency') +
        scale_y_continuous(breaks = setBreaksMajor) +
        labs(x = 'Assigned level of blame',
             y = 'Fine') +
        ggplot2::theme(panel.grid.major.x = element_blank())
}
