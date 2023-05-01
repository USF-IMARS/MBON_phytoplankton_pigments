##%######################################################%##
#                                                          #
####                Run Kriging on Data                 ####
#                                                          #
##%######################################################%##
#' Run Kriging on Data
#'
#' This uses the `fields` package to run `spatialProcesses` for a Krigged set of
#' data.
#'
#' @param .data Dataframe or tibble that contains columns of:
#'              - avg_ratio, 
#'              - lat, and 
#'              - lon
#' @param conc_name name of pigment or phytoplankton functional type
#'                  (ex. "Fucoxanthin")
#' @param col_name The column named used to run `spatialProces`
#' @param loc_name DESCRIPTION.
#' @param .title name added to the title (i.e. <.title>: <con_name>:TChla)
#'               (ex: "Spring: Fucoxanthin:TChla")
#' @param resolution Resolution of each pixel in degrees 
#'                   (default: 0.01)
#' @param adj Add adjustment to bounding box to make slighlty larger then the
#'            min/max of lat/lon un degrees
#'            (default: 0.1)
#' @param base_map Optional: supply a base map as a ggplot object to plot data
#'                 (default: NULL)
#' @param colr Setting the number and distance of break points in green color 
#'             palette. 
#'             Options (default: "data"): 
#'             - data:        sets the breaks at 10 locations based on the 
#'                            `avg_conc` column
#'             - vector == 3: low, high and length (i.e. c(1,5,9))
#'             - vector > 3:  sets number and distance of breaks
#' @param verbose Optional: allow the script to be more verbose with print 
#'                statements and plots 
#'                (default: FALSE)
#'
#' @return RETURN_DESCRIPTION
#' 
#' @returns A list object with three lists:
#'          - lon   = longitude of prediction grid 
#'          - lat   = latitude of prediction grid
#'          - value = predicted values
#'          
#'
#' @author Sebastian Di Geronimo (Febuary 08, 2023)
#'
#' @details
#' Optional:
#' map = Adds a ggplot map to output variable if base_map is given list(plt = plt)
#'
#' @note
#' Modified from https://github.com/eqmh/waltonsmith/blob/main/R_code/waltonsmith_hplc.R
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
map_data <- function(
                    .data,
                    conc_name,
                    col_name,
                    loc_name,
                    .title,
                    resolution = 0.01, 
                    adj        = 0.1, 
                    base_map   = NULL, 
                    colr       = "data",
                    verbose    = FALSE) {
    
    

    # TODO: fix ggplot
    # TODO: allow to use ratio or not
    set.seed(123)

    librarian::shelf(
        cli, ggplot2, rlang, fields, dplyr, 
    )
    
    cli_alert_info("Working on: {conc_name} for {loc_name}")
    
    if (mean(.data[[col_name]], na.rm = TRUE) == 0) {
        cli_alert_warning(c("Skipping: Average {conc_name} for ",
                            "{loc_name} equals 0."))
        return(invisible(NULL))
    }
    
    # ======================================================================== #
    # ---- color and contour breaks ----
    # ======================================================================== #
    if (!is.numeric(colr) && str_detect(colr, "data")) {
        # breaks set as length 10 and a gradient from data column
        breaks <- pretty(.data[[col_name]], n = 10)
        
    } else if (is.numeric(colr) && length(colr) == 3) {
        # break is high, low, and length (i.e. c(1,2,3))
        breaks <- seq(colr[1], colr[2], by = colr[3])
        
    } else if (is.numeric(colr) && length(colr) > 3) {
        # sequence of breakpoints (i.e. c(0.1,2,3.5,4,5,6,7))
        breaks <- sort(colr)
        
    } else {
        rlang::abort(
            cli::cli_abort(
                c("{.var colr} is used to create break points in the data.",
                  "This can be set to:",
                  "*" = "{.val data} to select 10 values spaced out using data", 
                  "*" = "a vector with 3 values (low, high, and length) (i.e {.var c(1,2,3)})",
                  "*" = "a vector of values with a length >3 (i.e {.var c(1,2,3,5,6,7)})" 
                ))
            )
    }
    
    chl_col <-
        colorRampPalette(c("honeydew2", 
                           "darkseagreen3", 
                           "forestgreen", 
                           "darkslategrey"))
                                      
    cols_fuco <- chl_col(length(breaks) - 1)
    
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
    krig_dat <-
      spatialProcess(
        df_loc,
        .data[[col_name]]
      )
            
    # catches ones with issues
    if (any(str_detect(class(krig_dat), "list"))) {
      cli_alert_danger(
        c(
          "Skipping: Issues with ",
          "{.fun {col_red(\"spatialProcess\")}}.",
          "\nThe returned {col_blue(\"class\")} is of type ",
          "{.var {col_green(\"list\")}} instead of ",
          "{.var {col_red(\"spatialProcess\")}} or ",
          "{.var {col_red(\"mKrig\")}}."
        )
      )

      return(invisible(NULL))
    }
    
    if (verbose) {
        summary(krig_dat)
        
        set.panel(2, 2)
        plot(krig_dat)
    }
    
    # prediction surface and standard errors
   krig_pred <-
     predictSurface(
       object = krig_dat,
       gridList = loc_grid,
       extrap = FALSE,
       verbose = FALSE
     )
    
    ratio_SE <-
    predictSurfaceSE(
      krig_dat,
      loc_grid,
      extrap = FALSE
    )
    
    
    # corrects the kriged min and max to observed min and max observed
    max_pig <- max(.data[[col_name]], na.rm = TRUE)
    min_pig <- min(.data[[col_name]], na.rm = TRUE)
    
    # corrected
    # krig_pred$z <- 
    dat <- 
        as_tibble(krig_pred$z) %>%
        mutate(
            across(.fn = ~ case_when(. > max_pig ~ max_pig, 
                                     . < min_pig ~ min_pig,
                                     TRUE ~ .))
        ) %>%
        as.matrix() %>%
        t() %>%
        c() %>%
        as_tibble() 
    
    
    # ======================================================================== #
    # ---- extract data to map ----
    # ======================================================================== #    
    
    # dat <- t(krig_pred$z) %>%
    #     c() %>%
    #     as_tibble() 
    
    dat_sp <-
        expand_grid(lon = krig_pred$x,
                    lat = krig_pred$y) %>%
        bind_cols(dat) %>%
        filter(!is.na(value))
    
    dat_se <- t(ratio_SE$z) %>%
        c() %>%
        as_tibble() 
    
    dat_se_sp <-
        expand_grid(lon = ratio_SE$x,
                    lat = ratio_SE$y) %>%
        bind_cols(dat) %>%
        filter(!is.na(value))
    
    # ======================================================================== #
    # ---- map data ----
    # ======================================================================== #
    
    if (rlang::is_null(base_map)) {
        cli_inform("\n")
        cli_alert_warning(c("{.var base_map} is set to {.var NULL}. ",
                            "Skipping plotting."))
        return(dat_sp)
    }
    
    # ---- plot  ----
    plt <- base_map +
        geom_contour_fill(data = dat_sp, 
                          aes(x = lon, 
                              y = lat, 
                              z = value), 
                          binwidth = 0.0001) +
        # geom_point(data = dat_sp, 
        #            aes(x = lon, 
        #                y = lat,  
        #                color = value)) +
        geom_point(data = .data,  
                   aes(x = lon, 
                       y = lat), 
                   color = "yellow") +
        
        labs(color = NULL,
             fill = "Concentration",
             title = .title) +
        
        scale_fill_gradientn(breaks = breaks, 
                             colors = cols_fuco) +
        
        coord_sf(xlim = xlims, 
                 ylim = ylims) +
        theme(legend.key.height = unit(50, "pt"))
    
    # maybe add back?
    # plt_se <- base_map +
    #     # geom_contour_filled(data = dat_se_sp, aes(x = lon, y = lat, z = value)) +
    #     geom_point(data = dat_se_sp, aes(x = lon, y = lat, color = value)) +
    #     geom_point(data = .data, aes(x = lon, y = lat), color = "yellow") +
    #     labs(color = NULL,
    #          title = glue("{.title}: {conc_name}:TChla")) + 
    #     scale_color_gradientn(breaks = breaks, colors = cols_fuco) +
    #     coord_sf(xlim = xlims, ylim = ylims) +
    #     theme(
    #         legend.key.height = unit(50, "pt")
    #     )
    
    # return list of data
    result <- list(
        dat = dat_sp,
        # map = list(map = plt)
        map = plt
    )
    
    
    Sys.sleep(1)
    return(result)
    # ---- end of function ----
    }


##%######################################################%##
#                                                          #
####                  Save Kriged Map                   ####
#                                                          #
##%######################################################%##
#' Save Kriged Map
#'
#' FUNCTION_DESCRIPTION
#'
#' @param maps DESCRIPTION.
#' @param save Optional: set to TRUE if want to save 
#'            (default: FALSE)
#' @param sv_name Optional: add part of name to file,
#'                (default: NULL; file - MBON_pigm_<title>_<yyymmdd_hhmmss>.png,
#'                 optional: file - MBON_pigm_<sv_name>_<title>_<yyymmdd_hhmmss>.png)
#' @param sav_loc Optional: set location to save plots as `.jpeg`, a default is set 
#'                (default: here("data", "plots", Sys.Date(), "map")
#'                               "~/data/plots/map/<current date>/")
#'
#' @return RETURN_DESCRIPTION
#' 
#' @details
#' #' External:
#' If `sav == TRUE`, will save figures to location set in sav_loc with a file 
#' name of "MBON_pigm_<sv_name>_<title>_<yyymmdd_hhmmss>.png"
#' 
#' @examples
#' # ADD_EXAMPLES_HERE
#' 
save_maps <- function(maps, sv = FALSE, sv_name = NULL,
                      sav_loc  = here("data", "plots", Sys.Date(), "map"),
                      overwrite = FALSE
                      ) {
    library("cli")
    
    overwrite
    
    if (is.null(sv_name)) {
        sv_name <- "map_plot"
    }
    
    if (is.null(maps)) {
        cli_alert_warning(c("Skipping: {.file {sv_name}} ",  
                            "is {.var {col_red(\"NULL\")}}"))
        return(invisible(NULL))
    }
    
    # save location and name
    if (sv) {
        
        filename <- here(sav_loc,
                         glue("{sv_name}_",
                              format(Sys.time(), "%y%m%d_%H%M%S") ,
                              ".png"))
        # cat("\n\n")
        cli_alert_info("Map file location: {.file {dirname(filename)}}")
        cli_alert_info("Map file name: {.file {basename(filename)}}")
        
        ggsave(filename,
               plot   = maps,
               width  = 10,
               height = 6)
    }
}
