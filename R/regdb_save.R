repdb_save_parcel_table = function(dat, project_dir, parcel_name, tab_name=parcel_name, parcel_dir = file.path(project_dir,"repdb")) {
  parcel = list(parcel=dat)
  names(parcel) = tab_name
  file = file.path(parcel_dir, paste0(parcel_name, ".Rds"))
  saveRDS(parcel, file)
}

repdb_save_parcels = function(parcels, dir, check=TRUE, check_missing_spec=FALSE) {
  restore.point("repdb_save_parcels")
  for (name in names(parcels)) {
    parcel = parcels[[name]]
    if (check) {
      for (table in names(parcel)) {
        repdb_check_data(parcel[[table]], table)
      }
    }
    tables = names(parcel)
    norm_parcel = lapply(tables, function(table) {
      dbspec_select_fields(parcel[[table]], repdb_get_spec(table))
    })
    names(norm_parcel) = tables


    file = file.path(dir,paste0(name,".Rds"))
    if (!dir.exists(dir)) {
      cat("\nCreate repdb directory ", dir,"\n.")
      dir.create(dir,recursive = TRUE)
    }
    saveRDS(norm_parcel, file)
  }
}


repdb_save_rds = function(dat,  dir,table=c("reg","regcoef","regvar")[1],   spec = repdb_get_spec(table)) {
  restore.point("repdb_save_rds")
  file = file.path(dir, paste0(table,".Rds"))
  if (!dir.exists(dir)) {
    cat("\nCreate repdb directory ", dir,"\n.")
    dir.create(dir)
  }
  dbspec_save_rds(dat, file, spec)
}

repdb_select_fields = function(dat, table,spec = repdb_get_spec(table), ignore=NULL, null_as_empty=FALSE) {
  dbspec_select_fields(dat, spec, ignore, null_as_empty)
}

repdb_check_data = function(dat, table,spec = repdb_get_spec(table), check_missing_spec = TRUE) {
  dbspec_check_data(dat, spec)
}
