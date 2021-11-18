plot_nwis_timeseries <- function(fileout, site_data_clean_file, width = 12, height = 7, units = 'in'){
  # load clean data
  site_data_styled <- readr::read_csv(site_data_clean_file)

  ggplot(data = site_data_styled, aes(x = dateTime, y = water_temperature, color = station_name)) +
    geom_line() + theme_bw()
  ggsave(fileout, width = width, height = height, units = units)
}