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
	study_1a_data\
	study_1b_data\
	reports

study_1a_data:\
	$(S1A_DATA)clean/study_1a.csv

study_1b_data:\
	$(S1B_DATA)clean/study_1b.csv

reports:\
	reports/methods.docx

## >>>>> concrete
$(S1A_DATA)clean/study_1a.csv: $(S1A_WRAN)study_1a_clean.R\
	$(S1A_DATA)raw/survey/study_1a_data.xlsx\
	renv.lock
	Rscript $<

$(S1B_DATA)clean/study_1b.csv: $(S1B_WRAN)study_1b_clean.R\
	$(S1B_DATA)raw/survey/study_1b_data_pt1.xlsx\
	$(S1B_DATA)raw/survey/study_1b_data_pt2.xlsx\
	$(S1B_DATA)raw/prolific/study_1b_prolific_pt1.csv\
	$(S1B_DATA)raw/prolific/study_1b_prolific_pt2.csv\
	renv.lock
	Rscript $<

reports/%.docx: reports/%.Rmd\
	study_1a_data\
	$(S1A_WRAN)study_1a_prepare-analysis-data.R\
	$(S1B_WRAN)study_1b_prepare-analysis-data.R
	Rscript -e 'rmarkdown::render("$<")'
