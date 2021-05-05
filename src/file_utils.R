

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  
  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

rds_to_csv <- function(in_file, out_file){
  write_csv(readRDS(in_file), out_file)
}

reduce_metadata <- function(in_file, out_file) {
  dat <- readRDS(in_file) %>%
    select(site_id, site_type, source, original_source, longitude, latitude, seg_id_orig_match, seg_id_reassign)
  
  write_csv(dat, out_file)
  
}