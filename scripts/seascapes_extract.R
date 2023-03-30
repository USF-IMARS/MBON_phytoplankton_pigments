##%######################################################%##
#                                                          #
####         Extract Seascapes from seascapes           ####
#                                                          #
##%######################################################%##
extract_seascapes <- function(filename, df) {
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
        dplyr::select(lon, lat)
    
    #extract coordinates for every station
    coord <- coord_df %>%
        sp::SpatialPointsDataFrame(coords = cbind(coord_df$lon,coord_df$lat),
                                   proj4string = CRS(SRS_string = "EPSG:4326"))
    
    # make a list of all the seascapes classes for each station in the cruise
    seascape_list <- list()
    for (i in 1:length(coord)) {
        temp_dist <- geosphere::distGeo(
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

