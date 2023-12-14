regdb_null_to_empty = function(df, table) {
  if (!is.null(df)) return(df)
  spec = spec = regdb_get_spec(table)
  dbspec_make_empty(spec)
}



regdb_field_names = function(table) {
  regdb_get_spec(table)$fields$field
}

regdb_get_spec = function(table) {
  #restore.point("regdb_get_spec")
  specs = getOption("regdb.specs")
  if (!table %in% names(specs)) regdb_load_specs()
  specs = getOption("regdb.specs")
  spec = specs[[table]]
  if (is.null(spec)) {
    stop("The specification for table ", table, " was not loaded.")
  }
  spec
}

get_lib_path = function(lib) {
  si = devtools::session_info()[[2]]
  row = match(lib, si$package)
  si$loadedpath[row]
}

regdb_load_specs = function(dir = NULL, libs = c("repboxReg", "repboxArt","repboxDB","repboxCodeText","repboxMap")) {
  restore.point("regdb_load_specs")
  if (is.null(dir)) {
    dir = sapply(libs, function(lib) {
      d = system.file("regdb",package=lib)
      # Also detect path if lib was loaded via devtools
      if (is.null(d) | is.na(d) | d=="") {
        lib_path = get_lib_path(lib)
        d = file.path(lib_path,"inst","regdb")
      }
      d
    })
  }
  spec_files = list.files(dir,glob2rx("*.yml"), full.names = TRUE)
  spec_bases = basename(spec_files)
  spec_names = tools::file_path_sans_ext(spec_bases)

  specs = lapply(spec_files, function(file) {
    res = try(dbspec_load(paste0(file)))
    if (is(res,"try-error")) {
      stop(paste0("There was a problem when loading the data base specification ", file,". Please correct the specification file."),call. = FALSE)
    }
    res
  })
  names(specs) = spec_names
  old.specs = getOption("regdb.specs")
  if (!is.null(old.specs)) {
    old.cols = setdiff(names(old.specs), names(specs))
    specs[old.cols] = old.specs[old.cols]
  }

  options(regdb.specs = specs)
  invisible(specs)
}
