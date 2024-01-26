

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()
  
  sf::st_write(st_as_sf(sf_object), dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites
  
  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)
  setwd(dsn)
  zip::zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

zip_this <- function(out_file, .object){
  
  if ('data.frame' %in% class(.object)){
    filepath <- basename(out_file) %>% tools::file_path_sans_ext() %>% paste0('.csv') %>% file.path(tempdir(), .)
    write_csv(.object, path = filepath)
    zip_this(out_file = out_file, .object = filepath)
  } else if (class(.object) == 'character' & file.exists(.object)){
    # for multiple files?
    curdir <- getwd()
    on.exit(setwd(curdir))
    setwd(dirname(.object))
    zip::zip(file.path(curdir, out_file), files = basename(.object))
  } else {
    stop("don't know how to zip ", .object)
  }
}

zip_obs <- function(out_file, in_file){
  if (grepl('csv', in_file)) {
    zip_this(out_file, .object = readr::read_csv(in_file))
    
  } else if (grepl('rds', in_file)) {
    zip_this(out_file, .object = readRDS(in_file))
  } else {
    message('There is no reader for this filetype. Please modify function zip_obs.')
  }
  
}

rds_to_csv <- function(in_file, out_file){
  
  readRDS(in_file) %>%
    readr::write_csv(file = out_file)

}

reduce_metadata <- function(in_file, out_file) {

  dat <- readRDS(in_file) %>%
    select(site_id, site_type, source, collection_agency = original_source, longitude, latitude, seg_id_orig_match, seg_id_reassign)
  
  write_csv(dat, out_file)
  
}

create_sf <- function(in_file) {
  readRDS(in_file) %>%
    select(site_id, source, longitude, latitude) %>%
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE) %>%
    #as_Spatial() %>%
    return()
    
}