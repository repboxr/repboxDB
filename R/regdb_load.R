
regdb_list_parcels = function(project_dir, metareg.dir = file.path(project_dir,"metareg"), other.dirs = paste0(project_dir,c("/art/regdb","/repbox/regdb","/map/regdb","/reports/regdb"))) {
  dirs = list.dirs(metareg.dir,full.names = TRUE,recursive = FALSE)
  parcel.dirs = c(file.path(dirs, "regdb"), other.dirs)
  parcel.files = list.files(parcel.dirs,glob2rx("*.Rds"),full.names = TRUE)
  parcels = basename(parcel.files) %>% str.left.of(".Rds")
  metaids = basename(dirname(dirname(parcel.files)))

  tibble(parcel=parcels, path = parcel.files, metaid = metaids, prio=-nchar(metaids))

}

regdb_load_parcels = function(project_dir, parcel_names, parcels=NULL) {
  restore.point("regdb_load_parcels")
  parcel_names = setdiff(parcel_names, names(parcels))
  if (length(parcel_names)==0) return(parcels)

  parcel_df = regdb_list_parcels(project_dir)
  rows = match(parcel_names, parcel_df$parcel)
  use = !is.na(rows)
  paths = parcel_df$path[rows[use]]
  new_parcels = lapply(paths, readRDS)
  names(new_parcels) = parcel_names[use]
  if (length(parcels)==0) return(new_parcels)
  c(parcels, new_parcels)
}

