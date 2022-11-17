extract_seascapes <- function(filename, df) {
################################################################################
#                                                                              # 
#               Extract Seascapes from seascapes#
#                                                                              #    
################################################################################
# ---- DESCRIPTION: ------
# Description
#
# ---- INPUTS: -----------
# Input =  
#
# ---- OUTPUTS: ----------
# Output = 
#
# ---- NOTES: ------------
#
# ---- REFERENCES(s): ----
#
# ---- AUTHOR(s): --------
# Anna Finch (2022-08-31)
    sea <- raster(filename) %>%
        tabularaster::as_tibble(xy = T) %>%
        drop_na(cellvalue)
    
    coord_df <- df %>%
        distinct(station, .keep_all = T) %>%
        select(lon, lat)
    
    #extract coordinates for every station
    coord <- coord_df %>%
        sp::SpatialPointsDataFrame(coords = cbind(coord_df$lon,coord_df$lat),
                                   proj4string = CRS(SRS_string = "EPSG:4326"))
    
    #make a list of all the seascapes classes for each station in the cruise
    seascape_list <- list()
    for (i in 1:length(coord)) {
        temp_dist <- geosphere::distGeo(
            p1 = sea[,3:4],
            p2 = coord[i,]
        )
        
        #temporary dataframe with distances, seascapes, coordinates
        temp_df <- sea %>% mutate(dist = as.numeric(temp_dist)) %>%
            #filter for distances less than 12 km (3-pixel radius, pixels are 4km x 4km)
            filter(temp_dist < 12000) %>%
            group_by(cellvalue) %>%
            #calculate average inverse distance for each seascape class
            summarise(sum1 = sum(1/dist)/n()) %>%
            #calculate probabilities of coordinate being the present seascape classes
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

