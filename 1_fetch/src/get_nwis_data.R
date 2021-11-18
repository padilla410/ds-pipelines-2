
download_nwis_data <- function(site_nums = c("01427207", "01432160", "01436690", "01466500")){ # removing "01435000"
  
  # create the file names that are needed for download_nwis_site_data
  # tempdir() creates a temporary directory that is wiped out when you start a new R session; 
  # replace tempdir() with "1_fetch/out" or another desired folder if you want to retain the download
  download_files <- file.path(tempdir(), paste0('nwis_', site_nums, '_data.csv'))
  data_out <- data.frame(agency_cd = c(), site_no = c(), dateTime = c(), 
                         X_00010_00000 = c(), X_00010_00000_cd = c(), tz_cd = c())
  # loop through files to download 
  for (download_file in download_files){
    download_nwis_site_data(download_file, parameterCd = '00010')
    # read the downloaded data and append it to the existing data.frame
    these_data <- read_csv(download_file, col_types = 'ccTdcc')
    data_out <- rbind(data_out, these_data)
  }
  return(data_out)
}

nwis_site_info <- function(fileout, site_data){
  site_no <- unique(site_data$site_no)
  site_info <- dataRetrieval::readNWISsite(site_no)
  write_csv(site_info, fileout)
}

#' Combine NWIS files for specified gages
#' 
#' Returns a table of NWIS data from user-specified gages
#' 
#' @param ... all upstream targets that should be combined for use in the downstream `site_data` target
#' 
combine_site_data <- function(...){
  files_in <- c(...)
  data <- lapply(files_in, readr::read_csv, col_types = 'ccTdcc') %>% bind_rows()
  
  return(data)
}

#' a helper function called internally by `download_nwis_data()`
#' 
download_nwis_site_data <- function(filepath, parameterCd = '00010', startDate="2014-05-01", endDate="2015-05-01"){
  
  # filepaths look something like directory/nwis_01432160_data.csv,
  # remove the directory with basename() and extract the 01432160 with the regular expression match
  site_num <- basename(filepath) %>% 
    stringr::str_extract(pattern = "(?:[0-9]+)")
  
  # readNWISdata is from the dataRetrieval package
  data_out <- readNWISdata(sites=site_num, service="iv", 
                           parameterCd = parameterCd, startDate = startDate, endDate = endDate)

  # -- simulating a failure-prone web-sevice here, do not edit --
  if (sample(c(T,F,F,F), 1)){
    stop(site_num, ' has failed due to connection timeout. Try scmake() again')
  }
  # -- end of do-not-edit block
  
  write_csv(data_out, file = filepath)
  return(filepath)
}

