.PHONY: all clean
all: final_report.html

clean:
	rm -f data/clean/wastewater_clean.rds output/table_one.rds final_report.html
data/clean/wastewater_clean.rds: code/00_clean_data.R data/raw/CDC_Wastewater_Data_for_SARS-CoV-2_GA.csv
	Rscript code/00_clean_data.R

output/table_one.rds: code/01_tables.R data/clean/wastewater_clean.rds
	Rscript code/01_tables.R

final_report.html: final_report.Rmd data/clean/wastewater_clean.rds output/table_one.rds
	Rscript -e "rmarkdown::render('final_report.Rmd')"