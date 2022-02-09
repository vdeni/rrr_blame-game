# helpers for plotting
setLimitsDV <- function(x) {
    if (max(x) > 7) {
        .upper <- ceiling(max(x) / 500) * 500

        return(c(0,
                 .upper))
    } else {
        return(c(1,
                 7))
    }
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
                             adjust = .5,
                             limits = c(NA, NA),
                             n = 3e4,
                             alpha = .7,
                             slab_color = 'black',
                             size = 1) +
        ggplot2::geom_pointrange(inherit.aes = F,
                                 data = s_p1,
                                 aes(x = experimental_situation,
                                     y = m,
                                     ymin = q1,
                                     ymax = q3),
                                 shape = 23,
                                 fatten = 4,
                                 size = .6,
                                 fill = 'black') +
        ggplot2::geom_point(size = 1.5,
                            alpha = .5,
                            shape = 1,
                            position = ggplot2::position_jitter(width = .10,
                                                                height = 0,
                                                                seed = 1)) +
        ggplot2::scale_x_discrete(labels = .labs) +
        ggplot2::labs(x = 'Experimental situation',
                      y = 'Fine') +
        ggplot2::theme(panel.grid.major.x = element_blank()) +
        ggplot2::scale_y_continuous(limits = setLimitsDV,
                                    breaks = setBreaksMajorDV,
                                    minor_breaks = setBreaksMinorDV)

    s_p2 <- dplyr::filter(summary_data,
                          assessment == 'blame')

    d_p2 <- filter(raw_data,
                   assessment == 'blame')

    p2 <- ggplot2::ggplot(d_p2,
                          aes(x = experimental_situation,
                              y = value)) +
        ggdist::stat_halfeye(geom = 'slab',
                             width = .25,
                             justification = -.5,
                             adjust = 1,
                             limits = c(NA, NA),
                             n = 3e4,
                             alpha = .7,
                             slab_color = 'black',
                             size = 1) +
        ggplot2::geom_pointrange(inherit.aes = F,
                                 data = s_p2,
                                 aes(x = experimental_situation,
                                     y = m,
                                     ymin = q1,
                                     ymax = q3),
                                 shape = 23,
                                 fatten = 4,
                                 size = .6,
                                 fill = 'black') +
        ggplot2::geom_point(size = 1.5,
                            alpha = .5,
                            shape = 1,
                            position = ggplot2::position_jitter(width = .10,
                                                                height = 0,
                                                                seed = 1)) +
        ggplot2::labs(x = '',
                      y = 'Blame') +
        ggplot2::theme(panel.grid.major.x = element_blank()) +
        ggplot2::scale_x_discrete(labels = .labs) +
        ggplot2::scale_y_continuous(breaks = setBreaksMajorDV,
                                    limits = setLimitsDV,
                                    minor_breaks = setBreaksMinorDV)

    p_out <- ggpubr::ggarrange(p2,
                               p1,
                               nrow = 2)

    return(p_out)
}

# s2
s2Plot <- function(raw_data,
                   summary_data) {
    .labs <- c('agentive' = 'Agentive',
               'nonagentive' = 'Nonagentive')

    ggplot2::ggplot(raw_data,
                    aes(x = blame_level,
                        y = assess_fine,
                        fill = agency,
                        color = agency)) +
        ggdist::stat_halfeye(geom = 'slab',
                             width = .70,
                             justification = -.3,
                             adjust = 1,
                             limits = c(NA, NA),
                             n = 3e4,
                             slab_alpha = .5,
                             size = 1) +
        ggplot2::geom_pointrange(inherit.aes = F,
                                 data = summary_data,
                                 aes(x = blame_level,
                                     y = m,
                                     ymin = q1,
                                     ymax = q3,
                                     fill = agency,
                                     color = agency),
                                 show.legend = F,
                                 shape = 23,
                                 fatten = 4,
                                 size = .6,
                                 position = position_dodge(width = .15)
                                 ) +
        ggplot2::geom_point(size = 1.5,
                            alpha = .6,
                            show.legend = F,
                            position = position_jitter(seed = 2,
                                                       height = 0,
                                                       width = .10)
                            ) +
        ggplot2::scale_color_grey(start = .1,
                                  end = .5,
                                  guide = 'none') +
        ggplot2::scale_fill_grey(start = .1,
                                 end = .5,
                                 name = 'Description',
                                 labels = .labs) +
        scale_y_continuous(breaks = setBreaksMajorDV) +
            labs(x = 'Assigned level of blame',
                 y = 'Fine') +
        ggplot2::theme(panel.grid.major.x = element_blank(),
                       legend.position = c(.9,
                                           .9))
    }
