# >>>>> variables
S1A_DATA=study_1a/data/
S1A_WRAN=study_1a/wrangling/

S1B_DATA=study_1b/data/
S1B_WRAN=study_1b/wrangling/

S2A_DATA=study_2a/data/
S2A_WRAN=study_2a/wrangling/

S2B_DATA=study_2b/data/
S2B_WRAN=study_2b/wrangling/

# >>>>> recipes

## >>>>> abstract

.PHONY:\
	all\
	data\
	reports

all:\
	data\
	reports

data:\
	$(S1A_DATA)clean/study_1a.csv\
	$(S1B_DATA)clean/study_1b.csv\
	$(S2A_DATA)clean/study_2a.csv\
	$(S2B_DATA)clean/study_2b.csv

reports:\
	reports/methods.docx\
	reports/analyses.docx

## >>>>> concrete
$(S1A_DATA)clean/study_1a.csv:\
	$(S1A_WRAN)study_1a_clean.R\
	$(S1A_DATA)raw/survey/study_1a_data.xlsx\
	renv.lock
	Rscript $<

$(S1B_DATA)clean/study_1b.csv:\
	$(S1B_WRAN)study_1b_merge.R\
	$(S1B_WRAN)study_1b_clean.R\
	$(S1B_DATA)raw/survey/study_1b_data_pt1.xlsx\
	$(S1B_DATA)raw/survey/study_1b_data_pt2.xlsx\
	$(S1B_DATA)raw/prolific/study_1b_prolific_pt1.csv\
	$(S1B_DATA)raw/prolific/study_1b_prolific_pt2.csv\
	renv.lock
	Rscript $<

$(S2A_DATA)clean/study_2a.csv:\
	$(S2A_WRAN)study_2a_clean.R\
	$(S2A_DATA)raw/survey/study_2a_data.sav\
	renv.lock
	Rscript $<

$(S2B_DATA)clean/study_2b.csv:\
	$(S2B_WRAN)study_2b_merge.R\
	$(S2B_WRAN)study_2b_clean.R\
	$(S2B_DATA)raw/survey/study_2b_data_pt1.xlsx\
	$(S2B_DATA)raw/survey/study_2b_data_pt2.xlsx\
	$(S2B_DATA)raw/prolific/study_2b_prolific_pt1.csv\
	$(S2B_DATA)raw/prolific/study_2b_prolific_pt2.csv\
	renv.lock
	Rscript $<

reports/%.docx:\
	reports/%.Rmd\
	$(S1A_DATA)clean/study_1a.csv\
	$(S1B_DATA)clean/study_1b.csv\
	$(S2A_DATA)clean/study_2a.csv\
	$(S2B_DATA)clean/study_2b.csv\
	$(S1A_WRAN)study_1a_prepare-analysis-data.R\
	$(S1B_WRAN)study_1b_prepare-analysis-data.R\
	$(S2A_WRAN)study_2a_prepare-analysis-data.R\
	$(S2B_WRAN)study_2b_prepare-analysis-data.R
	Rscript -e 'rmarkdown::render("$<")'
