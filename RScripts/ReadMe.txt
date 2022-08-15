I am trying to keep straight what order I need to run r files in
Last edit: 2022-06-04
For synoptic data:

1. Synoptic: these scripts combine synoptic measuremnts of CO2 concentration and flux collected in the field data by site
	Synoptic_ANTE_script_2022-06-04.R
		WriteOut: "Synoptic/ANTE_2022-06-04.csv"
	Synoptic_GAVI_script_2022-06-04.R
		WriteOut: "Synoptic/GAVI_2022-06-04.csv"
	Synoptic_COLMILLO_script_2022-06-05
		WriteOut: "Synoptic/COLMILLO_2022-06-05.csv"

2. Geomorphology: these script use the data collected with handheld GPS to map the stream lat, lon, and elevation. Script interpolates in between gaps calculates distance between points. Finally, it combines the synoptic data compiled in previous script. also calculates slope
	ANTE_Geomorphology_2022-06-05.Rmd
		WriteOut: "ProcessedData/ANTE_synoptic_2022-06-07.csv"
		WriteOut: "ProcessedData/ANTE_WaterChem_synop_2022-03-23.csv"
	GAVI_Geomorphology_2022-06-04.Rmd
		WriteOut: "ProcessedData/GAVI_synoptic_2022-06-04.csv"
  COLM_Geomorphology_2022-06-04.Rmd
		WriteOut: "ProcessedData/COLM_synoptic_2022-06-04.csv"
	GAVItrib1_Geomorphology_2022-06-08.Rmd
		WriteOut: "ProcessedData/GAVI_Trib1_synoptic_2022-06-08.csv"
	GAVItrib2_Geomorphology_2022-06-08.Rmd
		WriteOut: "ProcessedData/GAVI_Trib2_synoptic_2022-06-08.csv"

3. More Geomorphology: This script combines geomorphology with width and distance data. It calculates a column for surface area represented by each CO2 measurment based on distance between points and width 
	ANTE_moreGeomorph_2022-06-05.Rmd
		WriteOut: "ProcessedData/ANTE_synopticGeom_2022-06-07.csv"
	GAVI_moreGeomorph_2022-06-04.Rmd
		WriteOut: "ProcessedData/GAVI_synopticGeom_2022-06-04.csv"
	COLM_moreGeomorph_2022-06-07.Rmd
		WriteOut: "ProcessedData/COLM_synopticGeom_2022-06-07.csv"

4. synoptic_k600_2022-06-08: this script calculates k600 for all sites (excluding gavi tribs)
		WriteOut: "ProcessedData/ALL_synoptic_2022-06s-24.csv"
		
5. MergeRaster.R
This script merges dataframe with the Raster Data based on flux, wetland and ave flux column
I also did some manual manipulations after merging raster data
  
