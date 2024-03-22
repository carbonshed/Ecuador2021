#slope

GaviTrib1_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviTrib1_synop_Slope.csv")
GaviTrib1_synop_Slope$Stream_name <- "GaviTrib1"
GaviTrib2_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviTrib2_synop_Slope.csv")
GaviTrib2_synop_Slope$Stream_name <- "GaviTrib2"
GaviOutlet_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviOutlet_synop_Slope.csv")
GaviOutlet_synop_Slope$Stream_name <- "GaviOutlet"
    #one to many points
GaviInlet_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/GaviInlet_synop_Slope.csv")
GaviInlet_synop_Slope$Stream_name <- "GaviInlet"
#Antenas_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/Antenas_synop_Slope.csv")
Colmillo_synop_Slope <-  read.csv("~/OneDrive - University of North Carolina at Chapel Hill/Dissertation papers/SynopPaper/DTW_data/Slope_tables/Colmillo_synop_Slope.csv")
Colmillo_synop_Slope$Location1 <- NULL
Colmillo_synop_Slope$Stream_name <- "Colmillo"


df <- rbind(GaviInlet_synop_Slope,GaviOutlet_synop_Slope,GaviTrib1_synop_Slope,GaviTrib2_synop_Slope)
