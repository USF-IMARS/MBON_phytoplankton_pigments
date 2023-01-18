map_data <- function(.data,
                        pig,
                        szn,
                        resolution = 0.01, 
                        adj        = 0.1, 
                        base_map   = NULL, 
                        colr       = "data",
                        sav_loc    = here("data", "plots", "map"),
                        sav        = FALSE,
                        sv_name    = NA,
                        verbose    = FALSE) {
    set.seed(123)
    
    # ======================================================================== #
    # ---- color and contour breaks ----
    # ======================================================================== #
    if (!is.numeric(colr) && str_detect(colr, "data")) {
        breaks_fuco <- pretty(.data$ratio_avg, n = 10)
        
    } else if (is.numeric(colr) && length(colr) == 3) {
        breaks_fuco <- seq(colr[1], colr[2], by = colr[3])
        
    } else if (is.numeric(colr) && length(colr) > 3) {
        breaks_fuco <- colr 
        
    } else {
        rlang::abort(
            cli::cli_abort(
                c("{.var colr} can to be:",
                  "{.var data} to select 10 values from the data,", 
                  "a vector with 3 values for `seq()` (i.e {.var c(1,2,3)}), or",
                  "a seqence of numbers greater than 3 (i.e {.var c(1,2,3,5,6,7)})" 
                )))
    }
    
    chl_col <-
        colorRampPalette(c("honeydew2", 
                                      "darkseagreen3", 
                                      "forestgreen", 
                                      "darkslategrey"))
                                      
    cols_fuco <- chl_col(length(breaks_fuco) - 1)
    
    # ======================================================================== #
    # ---- kriging setup locations ----
    # ======================================================================== #  
    df_loc   <- select(.data, lon, lat)
    
    # creates two list: lat and lon that is bounding box for prediction surface
    # min to max +/- adjustment by resolution amount in decimal degress
    loc_grid <- 
        list(lon = seq(min(.data$lon, na.rm = TRUE) - adj,
                       max(.data$lon, na.rm = TRUE) + adj, 
                       resolution),
             
             lat = seq(min(.data$lat, na.rm = TRUE) - adj, 
                       max(.data$lat, na.rm = TRUE) + adj, 
                       resolution))
    
    # set limits for plotting as range of lat/lon
    xlims    <- range(loc_grid$lon)
    ylims    <- range(loc_grid$lat)
    
    if (verbose) print(loc_grid)
    
    
    # ======================================================================== #
    # ---- model spatial process using lon/lat and pigment:chla  ----
    # ======================================================================== #  
    krig_dat     <- spatialProcess(df_loc, 
                                   .data$ratio_avg)
    
    if (verbose) {
        summary(krig_dat)
        
        set.panel(2, 2)
        plot(krig_dat)
    }
    
    # prediction surface and standard errors
    ratio_kriged <- predictSurface(krig_dat, 
                                   loc_grid,
                                   extrap = FALSE
    )
    
    ratio_SE     <- predictSurfaceSE(krig_dat, 
                                     loc_grid, 
                                     extrap = FALSE)
    
    # corrects the kriged min and max to observed min and max observed
    max_pig <- max(.data$ratio_avg, na.rm = TRUE)
    min_pig <- min(.data$ratio_avg, na.rm = TRUE)
    
    # corrected
    ratio_kriged$z <- 
        as_tibble(ratio_kriged$z) %>%
        mutate(
            across(.fn = ~ case_when(. > max_pig ~ max_pig, 
                                     . < min_pig ~ min_pig,
                                     TRUE ~ .))
        ) %>%
        as.matrix()
    
    
    # ======================================================================== #
    # ---- extract data to map ----
    # ======================================================================== #    
    
    dat <- t(ratio_kriged$z) %>%
        c() %>%
        as_tibble() 
    
    dat_sp <-
        expand_grid(lon = ratio_kriged$x,
                    lat = ratio_kriged$y) %>%
        # dplyr::bind_cols(as.vector(ratio_kriged_fuco$z) %>%
        bind_cols(dat) %>%
        filter(!is.na(value))
    
    dat_se <- t(ratio_SE$z) %>%
        c() %>%
        as_tibble() 
    
    dat_se_sp <-
        expand_grid(lon = ratio_SE$x,
                    lat = ratio_SE$y) %>%
        # dplyr::bind_cols(as.vector(ratio_kriged_fuco$z) %>%
        bind_cols(dat) %>%
        filter(!is.na(value))
    
    # ======================================================================== #
    # ---- map data ----
    # ======================================================================== #
    
    if (rlang::is_null(base_map)) {
        cli_alert_warning(c("{.var base_map} is set to {.var NULL}. ",
                            "Skipping plotting."))
        return(dat_sp)
    }
    
    # ---- plot  ----
    plt <- base_map +
        geom_point(data = dat_sp, aes(x = lon, y = lat, color = value)) +
        geom_point(data = df_fuco, aes(x = lon, y = lat), color = "yellow") +
        labs(color = NULL,
             title = glue("{szn_sel}: {pig}:TChla")) + 
        scale_color_gradientn(breaks = breaks_fuco, colors = cols_fuco) +
        coord_sf(xlim = xlims, ylim = ylims) +
        theme(
            legend.key.height = unit(50, "pt")
        )
    
    plt_se <- base_map +
        # geom_contour_filled(data = dat_se_sp, aes(x = lon, y = lat, z = value)) +
        geom_point(data = dat_se_sp, aes(x = lon, y = lat, color = value)) +
        geom_point(data = df_fuco, aes(x = lon, y = lat), color = "yellow") +
        labs(color = NULL,
             title = glue("{szn}: {pig}:TChla")) + 
        scale_color_gradientn(breaks = breaks_fuco, colors = cols_fuco) +
        coord_sf(xlim = xlims, ylim = ylims) +
        theme(
            legend.key.height = unit(50, "pt")
        )
    
    # save location and name
    if (sav) {
        fs::dir_create(sav_loc)
        filename <- here(sav_loc,
                         glue("MBON_pigm_", sv_name, "_winter_",
                              format(Sys.time(), "%y%m%d_%H%M%S") ,
                              ".png"))
        
        cli_alert_info("Map file location: {.file {dirname(file)}}")
        cli_alert_info("Map file name: {.file {basename(file)}}")
        
        ggsave(filename, 
               plot = plt,
               width = 10,
               height = 6)
    }
    
    result <- list(
        dat_sp,
        plt,
        plt_se
    )
    
    return(result)
    
    }
