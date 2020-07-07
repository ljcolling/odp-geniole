# Download the data
download: ./code/download.R docker-compose.yml 
	make made/geniole_download_info.Rdata 

made/geniole_download_info.Rdata: ./code/download.R
	docker-compose run --rm download

download-docker: ./code/download.R
	R -e "source('./code/download.R')"

# Analyse the data
analysis: code/analysis.R docker-compose.yml 
	make made/geniole.Rdata

made/geniole.Rdata: ./code/analysis.R data/
	docker-compose run --rm analysis

analysis-docker: ./code/analysis.R
	echo "building geniole"
	R -e "source('./code/analysis.R')"

# Create the data
document: made/geniole.Rdata made/geniole_download_info.Rdata
	docker-compose run --rm document

document-docker: 
	R -e "xfun::)
	R -e "rmarkdown::render('docs/geniole-LJC.Rmd')"

clean:
	rm -rf data
	rm -rf made
	rm -f docs/*.log
	rm -f docs/*.pdf
	rm -f docs/geniole.bib

 
