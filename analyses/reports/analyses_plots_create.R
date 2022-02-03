## ----setup, echo=F, message=F, warning=F, results='hide'----------------------
renv::activate(here::here('analyses'))

library(here)
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(knitr)
library(glue)

knitr::opts_chunk$set(device = 'png',
                      dpi = 300,
                      echo = F,
                      message = F,
                      fig.width = 8,
                      fig.height = 8 * 9 / 16)

options(scipen = 5,
        digits = 3)

.fine_lim_hrk <- 10200
.fine_lim_usd <- 4500

ggplot2::theme_set(theme_minimal())

ggplot2::theme_update(strip.placement = 'outside',
                      panel.spacing.y = unit(x = 20,
                                             units = 'pt'))

source(here('analyses',
            'helpers',
            'plots.R'))

source(here('analyses',
            'helpers',
            'stats.R'))

source(here('analyses',
            'helpers',
            'tables.R'))


## ----s1a-load-data------------------------------------------------------------
source(here::here('analyses',
                  'study_1a',
                  'wrangling',
                  'study_1a_prepare-analysis-data.R'))


## ----s1a-plot-assessments, fig.cap='Individual responses to the proposed fine and stated level of blame (jittered horizontally). The X-es represent each group\'s mean value.'----
.p_data <- d_s1a %>%
    tidyr::pivot_longer(.,
                        cols = c('assess_fine',
                                 'assess_blame'),
                        names_pattern = 'assess_(\\w+)',
                        names_to = 'assessment',
                        values_to = 'value')


.summary <- dplyr::group_by(.p_data,
                            assessment,
                            experimental_situation) %>%
    dplyr::summarise(.,
                     m = mean(value),
                     stdev = sd(value))

s1Plot(.p_data,
       .summary)
ggsave(filename = "reports/plots/s1a_comparison.png",
                                     device = "png",
                                     dpi = 300,
                                     bg = '#ffffff')
                          

## ----s1a-t-test---------------------------------------------------------------
## ----s1b-load-data------------------------------------------------------------
source(here::here('analyses',
                  'study_1b',
                  'wrangling',
                  'study_1b_prepare-analysis-data.R'))


## ----s1b-plot-assessments, fig.cap='Individual responses to the proposed fine and stated level of blame (jittered horizontally). The X-es represent each group\'s mean value.'----
.p_data <- d_s1b %>%
    tidyr::pivot_longer(.,
                        cols = c('assess_fine',
                                 'assess_blame'),
                        names_pattern = 'assess_(\\w+)',
                        names_to = 'assessment',
                        values_to = 'value')


.summary <- dplyr::group_by(.p_data,
                            assessment,
                            experimental_situation) %>%
    dplyr::summarise(.,
                     m = mean(value),
                     stdev = sd(value))

s1Plot(.p_data,
       .summary)
ggsave(filename = "reports/plots/s1b_comparison.png",
                                     device = "png",
                                     dpi = 300,
                                     bg = '#ffffff')
                          

## ----s1b-t-test---------------------------------------------------------------
## ----s2a-load-data------------------------------------------------------------
source(here::here('analyses',
                  'study_2a',
                  'wrangling',
                  'study_2a_prepare-analysis-data.R'))


## ----s2a-plot-assessments, fig.cap='Individual responses to the proposed fine and stated level of blame (jittered horizontally). The large markers represent each group\'s mean value.'----
.summary <- dplyr::group_by(d_s2a,
                            agency,
                            blame_level) %>%
    dplyr::summarise(.,
                     m = mean(assess_fine),
                     stdev = sd(assess_fine))

s2Plot(d_s2a,
       .summary)
ggsave(filename = "reports/plots/s2a_comparison.png",
                                     device = "png",
                                     dpi = 300,
                                     bg = '#ffffff')
                          

## ----s2a-anova----------------------------------------------------------------
## ----s2b-load-data------------------------------------------------------------
source(here::here('analyses',
                  'study_2b',
                  'wrangling',
                  'study_2b_prepare-analysis-data.R'))


## ----s2b-plot-assessments, fig.cap='Individual responses to the proposed fine and stated level of blame (jittered horizontally). The large markers represent each group\'s mean value.'----
.summary <- dplyr::group_by(d_s2b,
                            agency,
                            blame_level) %>%
    dplyr::summarise(.,
                     m = mean(assess_fine),
                     stdev = sd(assess_fine))

s2Plot(d_s2b,
       .summary)
ggsave(filename = "reports/plots/s2b_comparison.png",
                                     device = "png",
                                     dpi = 300,
                                     bg = '#ffffff')
                          

## ----s2b-anova----------------------------------------------------------------
