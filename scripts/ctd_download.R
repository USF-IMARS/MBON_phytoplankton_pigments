# ============================================================================ #
# 
# 
# ---- CTD Functions ----
# 
# 
# ============================================================================ #    


##%######################################################%##
#                                                          #
####        Download CTD Data from MBON Cruises         ####
#                                                          #
##%######################################################%##
#' Download CTD Data from MBON Cruises
#'
#' Function to query and download CTD data for each cruise.
#' This takes in cruise IDs and queries the ERDDAP server to find data. Next, 
#' this will download CTD files if they don't exist.
#'
#' @param IDS Cruise IDs to search
#' @param path Path to save files to
#' @param verbose `logical()`, if want more output information
#' @param overwrite `logical()`, if want to overwrite previous data
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
ctd_downloads <- function(IDS, path, verbose = TRUE, overwrite = FALSE) {
    
    # ---- check if ERDDAP is able to be connected to
    # if (!RCurl::url.exists(eurl())) {
    #     cat("\n")
    #     stop(paste("\b\b----\nCannot reach", eurl(),
    #                "\nThe server is probably down!\n---\n"))
    #     }
    
    if (verbose) message(paste("\n-------------------------",
                               "\nStarting cruise: ", IDS,
                               "\n-------------------------\n"))
    
    # create folder if doesn't exist
    here::here(path, IDS) %>%
        fs::dir_create()
    
    # query cruise ID
    results <- tryCatch({
        ed_search_adv(query = IDS)
    }, error = function(e) {
        if (verbose) message("Did not find any files!")
        invisible(return())
        break
    })
    # show results of cruise
    if (verbose) print(results)
    
    result_files <-  results$info$dataset_id
    
    # 
    dwn <- 0
    skips <- prev_dwn <- dwnld <- tibble() 
    
    # loop through each station of cruise and download if doesn't exist
    for (j in seq(result_files)) {
        
        # ---- extract filename from query
        file_name  <- result_files[j] %>%
            str_extract("(?i)((WS|SAV|WB|H)\\d{4,5}).+((stn|sta).*)",
                        c(1,3)) %>%
            str_c(collapse = "_")
        
        if (is.na(file_name)) {
            file_name <- result_files[j]
            
            if (str_detect(file_name, "SAV1803")) {
                file_name <-
                    str_extract(
                        file_name,
                        "SAV\\d{4}_[^SAV].*")
            } else if ((str_detect(file_name, "WS22215"))) {
                file_name <- 
                    str_extract(
                        file_name,
                        "CTD_(.*)", 1) %>%
                    str_c("WB22215", ., sep = "_stn")
            } else {
                skips <- bind_rows(skips, list("skip" = result_files[j]))
                # cat("Skipping file:", file_name, "\n--------\n\n")
                
                next
            }
        }
        
        # ---- create out file path
        file_path <- here(path, IDS, glue("{file_name}.csv")) 
        
        # ---- skip already downloaded files
        if (fs::file_exists(file_path) & !overwrite) {
            if (verbose) prev_dwn <- bind_rows(prev_dwn, 
                                               list("skip" = result_files[j])) 
            
            # cat("Skipping file:", file_name, "\n--------\n\n")
            next
        }
        
        if (verbose & dwn < 1) {
            cat("\n-------------------------",
                "\nDownloading:", 
                "\n-------------------------")
            dwn <- 1
        }
        
        if (verbose) {
            cat(sprintf("\n%-4.3d%35s as %s", dwn, result_files[j], file_name))
            dwn <- dwn + 1
        }
        
        # ---- get data
        out      <- info(result_files[j])
        
        suppressMessages(ctd_data <- tabledap(out, url = eurl(), store = disk()))
        
        # ---- save data
        # write.csv(ctd_data, file_path, row.names = FALSE)
        write_csv(ctd_data, file_path)
        dwnld <- bind_rows(dwnld, list("download" = result_files[j]))
    }
    
    # ---- print section
    if (verbose & dwn > 1) 
        cat("\n")
    
    # ---- previously downloaded files
    if (nrow(prev_dwn > 0)) 
        cat("\n-------------------------", 
            sprintf("\n%d Previously Downloaded:\n-------------------------",
                    nrow(prev_dwn)
            ),
            paste("\n", prev_dwn$skip),
            "\n"
        )
    
    # ---- skipped files
    if (nrow(skips > 0) & verbose) 
        cat("\n-------------------------", 
            sprintf("\n%d Skipped:\n-------------------------",
                    nrow(skips)
            ),
            paste("\n", skips$skip),
            "\n"
        )
    
    if (verbose)
        cat("-------------------------\n\n")
    
    return(
        list(download  = dwnld,
             prev_down = prev_dwn,
             skips     = skips)
    )
    
    # ---- end of function
}


##%######################################################%##
#                                                          #
####                Read CTD .csv Files                 ####
#                                                          #
##%######################################################%##
#' Read CTD .csv Files  
#'
#' FUNCTION_DESCRIPTION
#'
#' @param file CTD .csv file
#' @param cruise_id Cruise ID
#' @param station_id Station Name
#' @param depth The depth to average by
#' @param p If available, progress bar (to be used with `with_progress()`)
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
read_ctd_csv <- function(
  file,
  cruise_id,
  station_id,
  depth = 5,
  p) {
    
  out <-
    # ---- read file
    read_csv(
      file,
      show_col_types = FALSE,
      name_repair = "unique_quiet"
    ) %>%
      
    # ---- filter depth
    filter(depth <= .env$depth) %>%
    
    # ---- select cols and rename
    select(
      time,
      lat      = latitude,
      lon      = longitude, depth,
      pressure = sea_water_pressure,
      temp     = sea_water_temperature,
      o_sat    = oxygen_saturation, do = dissolved_oxygen,
      par      = any_of("photosynthetically_available_radiation"),
      sal      = sea_water_salinity
    ) %>%
      
    # ---- calculate average
    summarise(across(
      .cols = everything(),
      .fn = \(x) mean(x, na.rm = TRUE)
    ))
    
  # ---- print update
  if (!is.null(p)) {
    p(glue("Cruise {cruise_id}, Station: {station_id}"))
  } else {
    cli::cli_alert_info("Cruise {cruise_id}, Station: {station_id}")
  }
  out
}

# # load rerddap to require data
# 
# 
# # to read in all data for ctds, should do something like:
# # map_dfr({var from fs::dir_ls}, ~read_csv({with some parameters}), .id = "station")
# 
# # then filter depth < 4 m, do either avg or median for parameters of interest
# # then link them to HPLC data using cruise ID and stations number
# 
# library("purrr")
# library("rerddap")
# library("dplyr")
# root <- rprojroot::find_rstudio_root_file()
# 
# 
# # -----------------------------------------------------------------------------
# # set database
# database <- "https://gcoos5.geos.tamu.edu/erddap/"
# Sys.setenv(RERDDAP_DEFAULT_URL = database)
# 
# 
# # -----------------------------------------------------------------------------
# # function to query and download CTD data from each cruise
# ctd_downloads <- function(IDS){
#     results <- ed_search_adv(query = IDS)
# 
#     
#     print(results)
#     
#     for (j in 1:length(results$info$dataset_id)){
# 
#         filename <- results$info$dataset_id[j]
#         file_path <- paste0(root,"/data/raw/ctd/",IDS,"/",filename,".csv")
#         
#         if (isTRUE(fs::file_exists(file_path))) {
#             next
#         }
#         
#         out <- info(filename)
#         
#         ctd_data <- tabledap(out, url = eurl(), store = disk())
#         
#         write.csv(ctd_data ,file_path)
# 
#         
#     }
# }
# 
# # -----------------------------------------------------------------------------
# # list of cruises to search for
# cruiseID <- c("WS16074", "WS16130", "WS16263", "WS16319", "WS17086", "WS17170", 
#               "WS17282", "WS18008", "SAV1803", "WS18120", "SAV18173", "WS18218",
#               "WS18285", "WS18351", "WS19028", "WS19119", "WS19210", "WS19266",
#               "WS19322", "WS20006", "WS20231", "WS20278", "WS20342", "WS21032",
#               "WS21093")
# 
# # -----------------------------------------------------------------------------
# # create folder for cruises
# for (k in 1:length(cruiseID)){
#     try(dir.create(paste0(root,"/data/raw/ctd/", cruiseID[k]))) # need to test
# }
# 
# 
# # -----------------------------------------------------------------------------
# # run ctd_downloads function and try each cruise ID for existing data
# for (i in 1:length(cruiseID)){
#     
#     # try each cruise and download if exist
#     try(ctd_downloads(cruiseID[i]))
#     
#     print(paste("Finished ", cruiseID[i]))
#     print("-----------------------------------------")
# }
# 
# # -----------------------------------------------------------------------------
# 
# data_dir <- paste0(root,"/data/raw/ctd/")
# file.ctd <- 
#     fs::dir_ls(data_dir,
#                recurse = TRUE,
#                regexp = "\\.csv$") %>% 
#     stringr::str_sort()
# 
# 
# ctd_dat <- map_dfr(file.ctd, readr::read_csv, .id = "indx")
# 
# ctd_dat2 <-ctd_dat %>% 
#     mutate(indx = as.numeric(indx)) %>% 
#     filter(depth <= 5) %>% 
#     select(indx, time, latitude, longitude, sea_water_pressure, 
#            sea_water_temperature, oxygen_saturation, dissolved_oxygen,
#            photosynthetically_available_radiation, depth, sea_water_salinity)
# 
# file_names <- gsub(data_dir, "", file.ctd)
# cruises <- gsub("/.*", "", file_names)
# stations <- gsub(".*stn", "", file_names)
# stations <- gsub(".*STN", "", stations)
# stations <- gsub(".*STA", "", stations)
# stations <- gsub(".*Stn", "", stations)
# stations <- gsub(".*Sta", "", stations)
# stations <- gsub(".csv", "", stations)
# stations <- gsub("SAV1803/Savannah_SAV1803_SAV1803_", "", stations)
# stations <-gsub("_", "", stations)
# 
# 
# ctd_dat_summary <- ctd_dat2 %>% 
#     group_by(indx) %>% 
#     summarize(temp = mean(sea_water_temperature), o_sat = mean(oxygen_saturation), do = mean(dissolved_oxygen),
#        par = mean(photosynthetically_available_radiation), sal= mean(sea_water_salinity)) %>% 
#     # cbind(cruises) %>% 
#     # cbind(stations) %>% 
#     mutate(station = stations,
#            cruise = cruises,
#            station = case_when(station %in% c("21LK", "21LKb") ~ "LK",
#                                 station %in% c("215", "215B", "215v2") ~ "21.5",
#                                 station == "012" ~ "12",
#                                 station %in% c("018") ~ "18",
#                                 station == "WSb" ~ "WS",
#                                 station %in% c("MRredo", "MRv2", "MRa") ~ "MR",
#                                 station %in% c("010") ~ "10",
#                                 station %in% c("07", "007") ~ "7",
#                                 station == "016" ~ "16", 
#                                 station == "060" ~"60",
#                                 station =="003" ~ "3",
#                                 station == "24real" ~ "24",
#                                 TRUE ~ station
#                                 ),
#            cruises = case_when(cruises == "SAV18173" ~ "SV18173",
#                                cruises == "SAV1803" ~ "SV18067",
#                                cruises == "WS20231" ~ "WS20230",
#                                TRUE ~ cruises))
# 
# env_cmtx <- cmtx_avgs %>% 
#     mutate(cruise_code = substr(sample, 1, 7), season = as.character(season)) %>% 
#     left_join(ctd_dat_summary, by = c("cruise_code" = "cruises", "station" = "station")) %>% 
#     mutate_all(~ifelse(is.nan(.), NA, .)) 
# 
# 
# write.csv(env_cmtx, file = paste0(root, "//data//processed//cmtx_env_data.csv"))
