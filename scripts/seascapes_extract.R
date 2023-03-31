##%######################################################%##
#                                                          #
####              Download Seascapes Data               ####
#                                                          #
##%######################################################%##
#' Download Seascapes Data
#'
#' FUNCTION_DESCRIPTION
#'
#' @param map_loc DESCRIPTION.
#' @param ss_dataset DESCRIPTION.
#' @param ss_var DESCRIPTION.
#' @param date_beg DESCRIPTION.
#' @param date_end DESCRIPTION.
#' @param path DESCRIPTION.
#' @param del_cache DESCRIPTION.
#' @param verb DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' 
#' @author Sebastian Di Geronimo (Mar 30, 2023)
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
dwnld_seascp <- function(
        map_loc    = "fknms",
        ss_dataset = c("global_monthly", "global_8day"), 
        ss_var     = c("CLASS", "P"),         
        date_beg   = "2020-01-01",
        date_end   = "2020-03-01",
        path       = NULL,
        del_cache  = FALSE,
        verb       = FALSE
        ) {

    # ---- load libraries
    librarian::shelf(
        librarian, glue, fs, here, dplyr, stringr, cli,
        
        seascapeR
    )
    
    # ---- catch if package needs updating
    if (!str_detect(getNamespaceVersion("raster")[[1]], "3.6-21")) {
        stop(glue(
            "\n\n------------\n",
            "You need to download the development version of `raster` ",
            "before continuing!\n\n",
            "Current: {getNamespaceVersion(\"raster\")[[1]]}\n",
            "Needed:  3.6-21\n\n",
            "Restart R and run pacman::p_install_gh(\"rspatial/raster\")\n",
            "Top ribbon: Session > Restart R",
            "\n------------\n"))
        }
        
    if (!str_detect(getNamespaceVersion("seascapeR")[[1]], "0.4.1")) {
        stop(glue(
            "\n\n------------\n",
            "You need to update the version of `seascapeR` ",
            "before continuing!\n\n",
            "Current: {getNamespaceVersion(\"seascapeR\")[[1]]}\n",
            "Needed:  0.4.1\n\n",
            "Restart R and run pacman::p_install_gh(\"marinebon/seascapeR\")\n",
            "Top ribbon: Session > Restart R",
            "\n------------\n"))
        }

    # ---- inform
    cli::cli_alert_info(
        glue("Downloading Seascape data from: ", 
             "{seascapeR::nms[[which(map_loc == names(seascapeR::nms))]]}\n",
             "The resolution is {str_remove(ss_dataset, \"global_\")}\n",
             "Start: {date_beg}\n",
             "End: {date_end}\n")
        )
    
    
    ss_dataset <- match.arg(ss_dataset)
    ss_var     <- match.arg(ss_var)
    
    if (is.null(path)) stop("`path` needs to be set before continuing!")
    
    # ---- create path for data if doesn't exists
    fs::dir_create(path)
    
    # ---- get path for polygon and location dataset
    dir_ply <- here(path, "polygon")
   
    # either sanctuary or bounding box
    if (is.character(map_loc)) {
        # get sanctuary polygon
        ply <-
            get_url_ply(
                sanctuary = map_loc,
                dir_ply   = dir_ply)
        
        dir_grd <- here(path, glue("{map_loc}_{ss_dataset}"))
        
    } else if (is.list(map_loc)) {
        # list(n, s, e, w)
        ply <- 
            bbox_ply(
                lon_min = map_loc$w, 
                lat_min = map_loc$s, 
                lon_max = map_loc$e, 
                lat_max = map_loc$n)
        dir_grd <- here(path, glue("bbox_{ss_dataset}"))
        
    } else {
        cat("idk")
    }
    
    # ---- get seascape info for either global or 8 day
    ss_info <- get_ss_info(dataset = ss_dataset)    
    
    if (str_detect(date_beg, "min")) date_beg <- min(get_ss_dates(ss_info))
    if (str_detect(date_end, "max")) date_end <- max(get_ss_dates(ss_info))
    
   
    get_ss_grds(
        ss_info,
        ply,
        date_beg  = date_beg,
        date_end  = date_end,
        dir_tif   = dir_grd,
        del_cache = del_cache,
        verbose   = verb
    )
    
    if (verb) dir_tree(path)
    
    dir_ls(path, recurse = TRUE, 
           regexp = "global")
    
    here(path) %>%
        dir_ls(type = "directory", 
               regexp = ss_dataset) %>%
        dir_ls(recurse = TRUE, 
               regexp = "grd") %>%
        tibble(files = .)
    

}


##%######################################################%##
#                                                          #
####         Extract Seascapes from seascapes           ####
#                                                          #
##%######################################################%##
#' Extract Seascapes from seascapes
#'
#' FUNCTION_DESCRIPTION
#'
#' @param filename DESCRIPTION.
#' @param df DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' 
#' @author Anna Finch (2022-08-31)
#' @examples
#' # ADD_EXAMPLES_HERE
extract_seascapes <- function(filename, df) {
   
    
    # ---- load libraries
    librarian::shelf(
        librarian, tibble, tidyr, readr, purrr, dplyr, stringr,
        forcats, lubridate, glue, fs, magrittr, here,
        # broom # optional
        
        # additional
        tabularaster, raster, sp, geosphere
    )
    
    library("conflicted")
    
    conflict_prefer("filter", "dplyr")
    conflict_prefer("select", "dplyr")
    
    sea <- 
        raster(filename) %>%
        tabularaster::as_tibble(xy = TRUE) %>%
        drop_na(cellvalue)
    
    coord_df <- 
        df %>%
        distinct(station, .keep_all = TRUE) %>%
        select(lon, lat)
    
    #extract coordinates for every station
    coord <- 
        coord_df %>%
        SpatialPointsDataFrame(
            coords      = cbind(coord_df$lon,coord_df$lat),
            proj4string = CRS(SRS_string = "EPSG:4326"))
    
    # make a list of all the seascapes classes for each station in the cruise
    seascape_list <- list()
    
    for (i in 1:length(coord)) {
        
        temp_dist <- 
            geosphere::distGeo(
            p1 = sea[,3:4],
            p2 = coord[i,]
            )
        
        # temporary dataframe with distances, seascapes, coordinates
        temp_df <- 
            sea %>% 
            mutate(dist = as.numeric(temp_dist)) %>%
            
            # filter for distances less than 12 km (3-pixel radius, pixels are 4km x 4km)
            filter(temp_dist < 12000) %>%
            group_by(cellvalue) %>%
            
            # calculate average inverse distance for each seascape class
            summarise(sum1 = sum(1/dist)/n()) %>%
            
            # calculate probabilities of coordinate being the present seascape classes
            mutate(prob = sum1 / sum(sum1))
        
        #select row with the greatest probability
        greatest_prob <- filter(temp_df, prob == max(prob))
        
        #if there were no pixels at all within a 3-pixel radius, add NA to list
        if(nrow(greatest_prob) == 0){
            seascape_list[i] <- NA
        }
        #else, add the seascape of greatest probability
        else {seascape_list[i] <- greatest_prob$cellvalue
        }
    }
    
    #combine coordinates and seascapes togehter in a dataframe
    sea_coord <- coord_df %>% 
        mutate(seascape = unlist(seascape_list))
    
    return(sea_coord)
    
    # return(seascape_list)
}

