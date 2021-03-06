#' Process raw NWIS data into format for plotting
#' 
#' Reformats raw NWIS data and combines with site metadata for use in plotting. Returns a table of reformatted data.
#' 
#'  @param fileout str, output location for file 
#'  @param site_filename str, relative file path for NWIS gage metadata 
#'  @param site_info target object

process_data <- function(fileout, site_data_file, site_meta){
  # load station data
  nwis_data <- readr::read_csv(site_data_file)
  
  # reformat raw data and select columns of interest
  nwis_data_clean <- rename(nwis_data, water_temperature = X_00010_00000) %>% 
    select(-agency_cd, -X_00010_00000_cd, -tz_cd) %>% 
    left_join(., y = site_meta, by = 'site_no') %>%
    # join clean NWIS data with gage metadata
    select(station_name = station_nm, site_no, dateTime, water_temperature,
           latitude = dec_lat_va, longitude = dec_long_va) %>%
    # reformat station names into factors for easier plotting
    mutate(station_name = as.factor(station_name))

  # return(nwis_data_clean)
  readr::write_csv(nwis_data_clean, file = fileout)
}


