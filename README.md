# A preregistered direct replication of the linguistic frame effect on perceived blame and financial liability

## Table of contents

- [General information](##general-information)
- [Data metadata](##data-metadata)
- [Directory structure](##directory-structure)

## General information

![](http://creativecommons.org/licenses/by/4.0/)

This repository contains the scripts and cleaned data files used in a
replication attempt of:
Fausey, C. M., Boroditsky, L. (2010). Subtle linguistic cues influence
perceived blame and financial liability. *Psychonomic Bulletin & Review 17*,
644–650. https://doi.org/10.3758/PBR.17.5.644

The associated Open Science Framework project is: https://osf.io/jmknw/

## Data metadata

Variable names are shared between files:
- `respondent_id`: SurveyMonkey's internal participant ID
- `collector_id`: SurveyMonkey's internal "data collector" ID
- `start_date`: the datetime at which the questionnaire was accessed
- `end_date`: the datetime at which the questionnaire was finished
- `informed_consent`: has the participant given an informed consent to
participation. {`TRUE`, `FALSE`}. Should be `TRUE` for everyone.
- `experimental_situation`: specific to **study_1X**. whether the participant
read the agentive or the nonagentive description of the event. {`agentive`,
`nonagentive`, `NA` if an experimental situation wasn't assigned, i.e. if the
participant quit}
- `assess_blame`: specific to **study_1X**. the participants' assessment of the
blame of the subject of the described event. should be {1, 2, ..., 7, `NA`}
- `assess_fine`: the fine that the participants' assigned to the subject of the
described event. lower bound should be 0, upper bound is defined by exclusion
criteria outlined in the paper; `NA` if participant did not respond
- `assess_active_role`: the participants' assessment of the extent to which the
subject of the described event played an active role in starting the fire. this
is a manipulation check. should be {1, 2, ..., 7, `NA`}
- `age`: the participants stated age. lower bound is 0, upper bound was not
defined
- `gender`: the participants stated gender. differs between studies (sorry!)
    - study_1a: {`woman`, `man`, `I do not want to disclose`, `other`, `NA`}
    - study_1b: {`Female`, `Male`, `Other`, `NA`}
    - study_2a & study_2b: {`female`, `male`, `I prefer not to answer`, `other`,
    `NA`}
- `agency`: specific to **study_2X**. has the participant read the agentive or
the nonagentive description of the event. {`nonagentive`, `agentive`, `NA` if
they quit}
- `blame_level`: specific to **study_2X**. the level of blame set for the subject
of the described event. {`1`, `4`, `7`, `NA` if they quit}
- `collector_number`: specific to **study_2a**. SurveyMonkey's internal
collector number.
- `recently_participated_similar`: specific to **study_2a**. whether the
participant participated in a similar study recently. {`1` is no, `2` is yes,
`NA` if unanswered}

## Directory structure

The directory structure is displayed below.

All files related to the analyses are in the `analyses` folder.

`renv/` contains files needed by the R package `{renv}`.

`helpers/` contains R scripts with helpers functions used for generating certain
statistics, plots, and tables.

`reports/` contains the `.Rmd` files for the methods and analyses sections of
the paper.

`study_XX/` folders contain the cleaned data files (files used in the analyses;
`data/clean/*.csv`) and wrangling scripts (`wrangling/*.R`) for each of the four
studies.

```
.
├── analyses
│   ├── helpers
│   │   ├── plots.R
│   │   ├── stats.R
│   │   └── tables.R
│   ├── Makefile
│   ├── renv
│   │   ├── activate.R
│   │   └── settings.dcf
│   ├── renv.lock
│   ├── reports
│   │   ├── analyses_plots_create.R
│   │   ├── analyses_plots_extract.sed
│   │   ├── analyses_plots_save.py
│   │   ├── analyses.Rmd
│   │   └── methods.Rmd
│   ├── study_1a
│   │   ├── data
│   │   │   ├── clean
│   │   │   │   └── study_1a.csv
│   │   │   └── raw
│   │   │       └── survey
│   │   │           └── study_1a_data.xlsx
│   │   └── wrangling
│   │       ├── study_1a_clean.R
│   │       └── study_1a_prepare-analysis-data.R
│   ├── study_1b
│   │   ├── data
│   │   │   ├── clean
│   │   │   │   └── study_1b.csv
│   │   │   └── raw
│   │   │       ├── prolific
│   │   │       │   ├── study_1b_prolific_pt1.csv
│   │   │       │   └── study_1b_prolific_pt2.csv
│   │   │       └── survey
│   │   │           ├── study_1b_data_pt1.xlsx
│   │   │           └── study_1b_data_pt2.xlsx
│   │   └── wrangling
│   │       ├── study_1b_clean.R
│   │       ├── study_1b_merge.R
│   │       └── study_1b_prepare-analysis-data.R
│   ├── study_2a
│   │   ├── data
│   │   │   ├── clean
│   │   │   │   └── study_2a.csv
│   │   │   └── raw
│   │   │       └── survey
│   │   │           └── study_2a_data.sav
│   │   └── wrangling
│   │       ├── study_2a_clean.R
│   │       └── study_2a_prepare-analysis-data.R
│   └── study_2b
│       ├── data
│       │   ├── clean
│       │   │   └── study_2b.csv
│       │   └── raw
│       │       ├── prolific
│       │       │   ├── study_2b_prolific_pt1.csv
│       │       │   └── study_2b_prolific_pt2.csv
│       │       └── survey
│       │           ├── study_2b_data_pt1.xlsx
│       │           └── study_2b_data_pt2.xlsx
│       └── wrangling
│           ├── study_2b_clean.R
│           ├── study_2b_merge.R
│           └── study_2b_prepare-analysis-data.R
└── README.md
```