# Transforms results from EJD into regdb format
regdb_save_parcels = function(parcels, dir, check=TRUE) {
  restore.point("regdb_save_parcels")
  for (name in names(parcels)) {
    parcel = parcels[[name]]
    if (check) {
      for (table in names(parcel)) {
        regdb_check_data(parcel[[table]], table)
      }
    }
    tables = names(parcel)
    norm_parcel = lapply(tables, function(table) {
      dbspec_select_fields(parcel[[table]], regdb_get_spec(table))
    })
    names(norm_parcel) = tables


    file = file.path(dir,paste0(name,".Rds"))
    if (!dir.exists(dir)) {
      cat("\nCreate regdb directory ", dir,"\n.")
      dir.create(dir,recursive = TRUE)
    }
    saveRDS(norm_parcel, file)
  }
}


regdb_save_rds = function(dat,  dir,table=c("reg","regcoef","regvar")[1],   spec = regdb_get_spec(table)) {
  restore.point("regdb_save_rds")
  file = file.path(dir, paste0(table,".Rds"))
  if (!dir.exists(dir)) {
    cat("\nCreate regdb directory ", dir,"\n.")
    dir.create(dir)
  }
  dbspec_save_rds(dat, file, spec)
}

regdb_select_fields = function(dat, table,spec = regdb_get_spec(table), ignore=NULL, null_as_empty=FALSE) {
  dbspec_select_fields(dat, spec, ignore, null_as_empty)
}

regdb_check_data = function(dat, table,spec = regdb_get_spec(table)) {
  dbspec_check_data(dat, spec)
}
