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
        stop(glue(
            "`map_loc` needs to be one of the National Marine Sanctuaries or ",
            "a bounding box.\n",
            "Choose:\n", 
            "- Search National Marine Sanctuaries acronym: `seascapeR::nms` ", 
            "(ex: \"fknms\")\n",
            "- Bounding box in the form `list(n=, s=, e=, w=)` ",
            "(ex `list(n = 29, s = 24, e = -80, w = -85)`)"))
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
    
    # ---- Function End ----
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
#' @param filename File path for seascape tiff files
#' @param df Data with lat and lon to get closest seascape class
#' @param cell_w The width of a cell in a unit. It will be converted to meters.
#'               (default: 4 km)
#' @param grd_num The expected number of cells. The radius will be calculated.
#'                (default: 6 x 6)
#' @param verb return output in the console
#'
#' @return RETURN_DESCRIPTION
#' 
#' @author Anna Finch (2022-08-31) and Sebastian Di Geronimo (2022-03-30)
#'
#' @examples
#' # ADD_EXAMPLES_HERE
extract_seascapes <- function(filename, 
                              df, 
                              cell_w  = list(dist = 4, unit = "km"), 
                              grd_num = 6,
                              verb    = TRUE) {
   
    
    # ---- load libraries
    librarian::shelf(
        librarian, tibble, tidyr, readr, purrr, dplyr, stringr,
        forcats, lubridate, glue, fs, magrittr, here,
        # broom # optional
        
        # additional
        tabularaster, raster, sp, geosphere,
        measurements
    )
    
    library("conflicted")
    
    conflict_prefer("filter", "dplyr", quiet = TRUE)
    conflict_prefer("select", "dplyr", quiet = TRUE)
    
    # check if unit is in length
    units_possible <- conv_unit_options$length
    if (!str_to_lower(cell_w$unit) %in% units_possible) {
        message(c(
            sprintf("The units in `cell_w` is not a length!\n"),
            sprintf("You gave: %s\n", cell_w$unit),
            sprintf("Avaiable options:\n---\n%s\n",
                    paste(units_possible,
                          collapse = ", ")),
            "---\nPlease change `cell_w$unit` to one of the available ",
            "options and try again!")
            )
        return(invisible(NULL))
    }
    
    if (verb) {
        cat("\n\n------\nFile Name\n------\n", basename(filename))
        cat("\n\n------\nCruise ID:\n------\n", unique(df$cruise_id), "\n")
    }
    
    # ---- calc radius to filter around each point
    rad <- conv_unit(cell_w$dist, cell_w$unit, "m") * grd_num / 2
    
    # ---- read raster file and filter for non-NA values
    sea <- 
        raster(filename) %>%
        tabularaster::as_tibble(xy = TRUE) %>%
        drop_na(cellvalue)

    # ---- extract lat and lon from df
    coord_df <- 
        df %>%
        distinct(station, .keep_all = TRUE) %>%
        select(station, lon, lat)
    
    # ---- convert coordinates to SpatialPointsDataFrame
    coord <- 
        coord_df %>%
        SpatialPointsDataFrame(
            coords      = cbind(coord_df$lon,
                                coord_df$lat),
            proj4string = CRS(SRS_string = "EPSG:4326"))
    
    # make a list of all the seascapes classes for each station in the cruise
    seascape_list <- c()
    
    if (verb) {
        cat("\n------\nCalculate Distance\n------\n", 
            "Number of stations: ", nrow(coord), "\n",
            "Station ID:\tClass Number:\n")
    }
    
    for (i in seq(coord)) {
        if (verb) cat(sprintf("%11s", coord$station[[i]]))

        # temporary dataframe with distances, seascapes, coordinates
        prop_max <- 
            sea %>%
            
            # ---- calc the distance around each point
            rowwise() %>% 
            mutate(
                dist = geosphere::distGeo(
                    p1 = c(x, y),
                    p2 = coord[i,])
                ) %>%
            ungroup() %>%
            
            # ---- filter distances less than the radius around each point
            filter(dist < rad)
        
        if (nrow(prop_max) == 0) {
            # no pixels at all within a 3-pixel radius, add NA to list
            seascape_list <- c(seascape_list, NA)
            
        } else {
            prop_max <-
                prop_max %>%
            
                # calculate average inverse distance for each seascape class
                # this give more weight to closer cells
                summarise(sum = sum(1/dist)/n(), .by = cellvalue) %>%
                
                # ---- calc proportion of seascape classes within the radius
                mutate(prop = sum / sum(sum)) %>%
                
                # --- select class with highest proportion
                filter(prop == max(prop))
            
            # add the seascape of greatest proportion
            seascape_list <- c(seascape_list, prop_max$cellvalue)
           
            
        }
        if (verb) cat(sprintf("%18s\n", seascape_list[i]))
    }

    # combine coordinates and seascapes togehter in a dataframe
    df <- 
        bind_cols(coord_df, seascape = seascape_list)  %>%
        full_join(df, ., by = join_by(station)) %>%
        relocate(seascape, .after = 1)
    
    
    return(df)

    # ---- Function End ----
}

