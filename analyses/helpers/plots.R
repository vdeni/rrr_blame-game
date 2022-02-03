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

# plotting functions

# s1
s1Plot <- function(raw_data,
                    summary_data) {
    ggplot2::ggplot(raw_data,
                    aes(x = experimental_situation,
                        y = value)) +
        ggplot2::facet_wrap('assessment',
                            strip.position = 'left',
                            scales = 'free_y',
                            nrow = 2) +
        ggplot2::geom_point(size = 1.5,
                            alpha = .4,
                            position = ggplot2::position_jitter(width = .25,
                                                                height = 0)) +
        geom_point(inherit.aes = F,
                   data = summary_data,
                   aes(x = experimental_situation,
                       y = m),
                   shape = 4,
                   stroke = 1,
                   size = 3) +
        ggplot2::scale_y_continuous(limits = setLimitsHRK,
                                    breaks = setBreaksMajor) +
        labs(x = 'Experimental situation',
             y = '')
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
                                               'nonagentive' = 2))
}
