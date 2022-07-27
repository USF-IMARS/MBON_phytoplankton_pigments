# to read in all data for ctds, should do something like:
# map_dfr({var from fs::dir_ls}, ~read_csv({with some parameters}), .id = "station")

# then filter depth < 4 m, do either avg or median for parameters of interest
# then link them to HPLC data using cruise ID and stations number
library("rerddap")
mainDir <- rprojroot::find_rstudio_root_file()

# -----------------------------------------------------------------------------
# set database
database <- "https://gcoos5.geos.tamu.edu/erddap/"
Sys.setenv(RERDDAP_DEFAULT_URL = database)

# -----------------------------------------------------------------------------
# function to query and download CTD data from each cruise
ctd_downloads <- function(IDS){
    results <- ed_search_adv(query = IDS)
    
    print(results)
    
    for (j in 1:length(results$info$dataset_id)){
        
        out <- info(results$info$dataset_id[j])
        
        ctd_data <- tabledap(out, url = eurl(), store = disk())
        
        write.csv(ctd_data ,paste0(root,"//data//raw//ctd//",IDS,"/",
                                  results$info$dataset_id[j], ".csv"))
        
    }
}

# -----------------------------------------------------------------------------
# list of cruises to search for
cruiseID <- c("WS16074", "WS16130", "WS16263", "WS16319", "WS17086", "WS17170", 
              "WS17282", "WS18008", "SV18067", "WS18120", "SV18173", "WS18218",
              "WS18285", "WS18351", "WS19028", "WS19119", "WS19210", "WS19266",
              "WS19322", "WS20006", "WS20230", "WS20278", "WS20342", "WS21032",
              "WS21093")

# -----------------------------------------------------------------------------
# create folder for cruises
for (k in 1:length(cruiseID)){
    try(dir.create(paste0(root,"//data//raw//ctd//",cruiseID[k]))) # need to test
}


# -----------------------------------------------------------------------------
# run ctd_downloads function and try each cruise ID for existing data
for (i in 1:length(cruiseID)){
    
    # try each cruise and download if exist
    try(ctd_downloads(cruiseID[i]))
    
    print(paste("Finished ", cruiseID[i]))
    print("-----------------------------------------")
}

