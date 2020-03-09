datasrc := data/Akhmatova_I4AbAb_AllData.xlsx

%.ikts.csv: %.xlsx scripts/derive_ikts.R
	scripts/derive_ikts.R -i $< -o $@

add-ikts: $(patsubst %.xlsx,%.ikts.csv,$(datasrc))
