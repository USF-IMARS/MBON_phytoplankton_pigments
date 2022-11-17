# load rerddap to aquire data


# to read in all data for ctds, should do something like:
# map_dfr({var from fs::dir_ls}, ~read_csv({with some parameters}), .id = "station")

# then filter depth < 4 m, do either avg or median for parameters of interest
# then link them to HPLC data using cruise ID and stations number

library("purrr")
library("rerddap")
library("dplyr")
root <- rprojroot::find_rstudio_root_file()


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

        filename <- results$info$dataset_id[j]
        file_path <- paste0(root,"/data/raw/ctd/",IDS,"/",filename,".csv")
        
        if (isTRUE(fs::file_exists(file_path))) {
            next
        }
        
        out <- info(filename)
        
        ctd_data <- tabledap(out, url = eurl(), store = disk())
        
        write.csv(ctd_data ,file_path)

        
    }
}

# -----------------------------------------------------------------------------
# list of cruises to search for
cruiseID <- c("WS16074", "WS16130", "WS16263", "WS16319", "WS17086", "WS17170", 
              "WS17282", "WS18008", "SAV1803", "WS18120", "SAV18173", "WS18218",
              "WS18285", "WS18351", "WS19028", "WS19119", "WS19210", "WS19266",
              "WS19322", "WS20006", "WS20231", "WS20278", "WS20342", "WS21032",
              "WS21093")

# -----------------------------------------------------------------------------
# create folder for cruises
for (k in 1:length(cruiseID)){
    try(dir.create(paste0(root,"/data/raw/ctd/", cruiseID[k]))) # need to test
}


# -----------------------------------------------------------------------------
# run ctd_downloads function and try each cruise ID for existing data
for (i in 1:length(cruiseID)){
    
    # try each cruise and download if exist
    try(ctd_downloads(cruiseID[i]))
    
    print(paste("Finished ", cruiseID[i]))
    print("-----------------------------------------")
}

# -----------------------------------------------------------------------------

data_dir <- paste0(root,"/data/raw/ctd/")
file.ctd <- 
    fs::dir_ls(data_dir,
               recurse = TRUE,
               regexp = "\\.csv$") %>% 
    stringr::str_sort()


ctd_dat <- map_dfr(file.ctd, readr::read_csv, .id = "indx")

ctd_dat2 <-ctd_dat %>% 
    mutate(indx = as.numeric(indx)) %>% 
    filter(depth <= 5) %>% 
    select(indx, time, latitude, longitude, sea_water_pressure, 
           sea_water_temperature, oxygen_saturation, dissolved_oxygen,
           photosynthetically_available_radiation, depth, sea_water_salinity)

file_names <- gsub(data_dir, "", file.ctd)
cruises <- gsub("/.*", "", file_names)
stations <- gsub(".*stn", "", file_names)
stations <- gsub(".*STN", "", stations)
stations <- gsub(".*STA", "", stations)
stations <- gsub(".*Stn", "", stations)
stations <- gsub(".*Sta", "", stations)
stations <- gsub(".csv", "", stations)
stations <- gsub("SAV1803/Savannah_SAV1803_SAV1803_", "", stations)
stations <-gsub("_", "", stations)


ctd_dat_summary <- ctd_dat2 %>% 
    group_by(indx) %>% 
    summarize(temp = mean(sea_water_temperature), o_sat = mean(oxygen_saturation), do = mean(dissolved_oxygen),
       par = mean(photosynthetically_available_radiation), sal= mean(sea_water_salinity)) %>% 
    # cbind(cruises) %>% 
    # cbind(stations) %>% 
    mutate(station = stations,
           cruise = cruises,
           station = case_when(station %in% c("21LK", "21LKb") ~ "LK",
                                station %in% c("215", "215B", "215v2") ~ "21.5",
                                station == "012" ~ "12",
                                station %in% c("018") ~ "18",
                                station == "WSb" ~ "WS",
                                station %in% c("MRredo", "MRv2", "MRa") ~ "MR",
                                station %in% c("010") ~ "10",
                                station %in% c("07", "007") ~ "7",
                                station == "016" ~ "16", 
                                station == "060" ~"60",
                                station =="003" ~ "3",
                                station == "24real" ~ "24",
                                TRUE ~ station
                                ),
           cruises = case_when(cruises == "SAV18173" ~ "SV18173",
                               cruises == "SAV1803" ~ "SV18067",
                               cruises == "WS20231" ~ "WS20230",
                               TRUE ~ cruises))

env_cmtx <- cmtx_avgs %>% 
    mutate(cruise_code = substr(sample, 1, 7), season = as.character(season)) %>% 
    left_join(ctd_dat_summary, by = c("cruise_code" = "cruises", "station" = "station")) %>% 
    mutate_all(~ifelse(is.nan(.), NA, .)) 


write.csv(env_cmtx, file = paste0(root, "//data//processed//cmtx_env_data.csv"))
