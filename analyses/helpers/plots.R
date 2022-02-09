# helpers for plotting
setLimitsFine <- function(x) {
    if (max(x) > 7) {
        .upper <- ceiling(max(x) / 500) * 500

        return(c(0,
                 .upper))
    } else {
        return(c(1,
                 7))
    }
}

setLimitsCount <- function(x) {
    .upper <- ceiling(max(x) / 50) * 50
    
    return(c(0,
             .upper))
}

setBreaksMajorDV <- function (x) {
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

setBreaksMinorDV <- function(x) {
    if (max(x) > 8) {
        return(seq(0,
                   max(x),
                   by = 250))
    } else {
        return(NULL)
    }
}

setBreaksMajorCount <- function(x) {
    return(seq(0,
               max(x),
               by = 50))
}

setBreaksMinorCount <- function(x) {
    return(seq(0,
               max(x),
               by = 25))
}

# plotting functions

# s1
s1Plot <- function(raw_data,
                   summary_data) {
    .labs <- c('agentive' = 'Agentive',
               'nonagentive' = 'Nonagentive')

    s_p1 <- dplyr::filter(summary_data,
                          assessment == 'fine')

    d_p1 <- filter(raw_data,
                   assessment == 'fine')

    p1 <- ggplot2::ggplot(d_p1,
                          aes(x = experimental_situation,
                              y = value)) +
        ggdist::stat_halfeye(geom = 'slab',
                             width = .25,
                             justification = -.5,
                             adjust = .5, n = 3e4,
                             alpha = .7,
                             slab_color = 'black',
                             size = 1) +
        ggplot2::geom_boxplot(width = .10) +
        ggplot2::geom_point(size = 1.5,
                            alpha = .5,
                            shape = 1,
                            position = ggplot2::position_jitter(width = .10,
                                                                height = 0,
                                                                seed = 1)) +
        ggplot2::scale_x_discrete(labels = .labs) +
        ggplot2::labs(x = 'Experimental situation',
                      y = 'Fine') +
        ggplot2::theme(panel.grid.major.x = element_blank())
        ggplot2::scale_y_continuous(limits = setLimitsFine,
                                    breaks = setBreaksMajorDV,
                                    minor_breaks = setBreaksMinorDV)

    s_p2 <- dplyr::filter(summary_data,
                          assessment == 'blame')

    d_p2 <- filter(raw_data,
                   assessment == 'blame')

    p2 <- ggplot2::ggplot(d_p2,
                          aes(fill = experimental_situation,
                              x = value)) +
        ggplot2::geom_bar(position = 'dodge') +
        ggplot2::scale_x_continuous(breaks = setBreaksMajorDV,
                                    minor_breaks = setBreaksMinorDV) +
        scale_y_continuous(limits = setLimitsCount,
                           breaks = setBreaksMajorCount,
                           minor_breaks = setBreaksMinorCount) +
        ggplot2::labs(x = 'Assessed level of blame',
                      y = 'Count') +
        ggplot2::geom_point(data = s_p2,
                            aes(x = m,
                                y = n + 5,
                                color = experimental_situation),
                            size = 4,
                            shape = 23,
                            show.legend = F) +
        ggplot2::scale_fill_grey(name = 'Experimental situation',
                                 labels = .labs) +
        ggplot2::scale_color_grey() +
        ggplot2::theme(legend.position = c(.9,
                                           .8))

    p_out <- ggpubr::ggarrange(p1,
                               p2,
                               nrow = 2)

    return(p_out)
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
        scale_y_continuous(breaks = setBreaksMajorDV) +
        labs(x = 'Assigned level of blame',
             y = 'Fine') +
        ggplot2::theme(panel.grid.major.x = element_blank())
}
