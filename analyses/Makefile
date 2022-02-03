# >>>>> variables
S1A_DATA=study_1a/data/
S1A_WRAN=study_1a/wrangling/

S1B_DATA=study_1b/data/
S1B_WRAN=study_1b/wrangling/

S2A_DATA=study_2a/data/
S2A_WRAN=study_2a/wrangling/

S2B_DATA=study_2b/data/
S2B_WRAN=study_2b/wrangling/

PLOTS=reports/plots/

PLOTS_TARGETS= $(PLOTS)s1a_comparison.png $(PLOTS)s1b_comparison.png\
	$(PLOTS)s2a_comparison.png $(PLOTS)s2b_comparison.png

# >>>>> recipes

## >>>>> abstract

.PHONY:\
	all\
	data\
	reports

all:\
	data\
	reports\
	plots

data:\
	$(S1A_DATA)clean/study_1a.csv\
	$(S1B_DATA)clean/study_1b.csv\
	$(S2A_DATA)clean/study_2a.csv\
	$(S2B_DATA)clean/study_2b.csv

reports:\
	reports/methods.docx\
	reports/analyses.docx\
	reports/analyses_plots_create.R

plots:\
	$(PLOTS_TARGETS)

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
	$(S2B_WRAN)study_2b_prepare-analysis-data.R\
	helpers/plots.R\
	helpers/stats.R\
	helpers/tables.R
	Rscript -e 'rmarkdown::render("$<")'

reports/analyses_plots_create.R:\
	reports/analyses.Rmd\
	reports/analyses_plots_extract.sed\
	reports/analyses_plots_save.py
	Rscript -e 'knitr::purl("$<", output = "$@")'
	sed -i -f reports/analyses_plots_extract.sed -n $@
	python reports/analyses_plots_save.py
	mv tmp.R $@

$(PLOTS_TARGETS):\
	reports/analyses_plots_create.R
	mkdir -p reports/plots
	Rscript $<
