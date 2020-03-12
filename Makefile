datasrc := data/Akhmatova_I4AbAb.xlsx data/MandelshtamO_I4AbAb.xlsx
ikts := $(patsubst %.xlsx,%.ikts.csv,$(datasrc))

%.ikts.csv: %.xlsx scripts/derive_ikts.R
	scripts/derive_ikts.R -i $< -o $@

results/%.form-stance.csv: data/%.ikts.csv
	Rscript scripts/summary-tables.R -i $< -o results

add-ikts: $(ikts)

summary: $(patsubst data/%.ikts.csv,results/%.form-stance.csv,$(ikts))
