I am trying to keep straight what order I need to run r files in

For synoptic data:

1. Synoptic: these scripts combine synoptic measuremnts of CO2 concentration and flux collected in the field data by site
	Synoptic_ANTE_script_2022-03-24.R
		WriteOut: "Synoptic/ANTE_2022-03-23.csv"
	Synoptic_GAVI_script_2022-03-16.R
		WriteOut: "Synoptic/GAVI_2022-03-24.csv"
	Synoptic_COLMILLO_script_2022-03-17

2. Geomorphology: these script use the data collected with handheld GPS to map the stream lat, lon, and elevation. Script interpolates in between gaps calculates distance between points. Finally, it combines the synoptic data compiled in previous script. also calculates slope
	ANTE_Geomorphology_2022-02-16.Rmd
		WriteOut: "ProcessedData/ANTE_synoptic_2022-03-23.csv"
		WriteOut: "ProcessedData/ANTE_WaterChem_synop_2022-03-23.csv"
	GAVI_Geomorphology_2022-02-16.Rmd
		WriteOut: "ProcessedData/GAVI_synoptic_2022-03-24.csv"
3. More Geomorphology: This script combines geomorphology with width and depth data. It calculates a column for surface area represented by each CO2 measurment based on distance between points and width 
	ANTE_moreGeomorph_2022-02-16.Rmd
		WriteOut: "ProcessedData/ANTE_synopticGeom_2022-03-23.csv"

4. synoptic_k600_2022-02-11: this script calculates k600 for all sites
