# Converts the results of the original repbox reproduction
# into regdb tables

example = function() {
  project.dir = "C:/libraries/repbox/projects_reg/testsupp"
  project.dir = "~/repbox/projects_reg/aejapp_3_1_3"
  repbox_to_regdb(project.dir)

  rstudioapi::filesPaneNavigate(project.dir)
  rstudioapi::filesPaneNavigate("~/repbox/repboxDB/inst/regdb")
}

repbox_to_regdb = function(project.dir) {
  restore.point("repbox_results_to_regdb")
  regdb_load_specs(libs="repboxDB")
  regdb.dir = file.path(project.dir, "repbox","regdb")
  if (!dir.exists(regdb.dir)) dir.create(regdb.dir)
  file_df = repbox_file_to_regdb(project.dir)
  script_df = regdb_make_stata_script_parcel(project.dir, file_df)
  parcels = repbox_results_to_regdb(project.dir, script_df)
  invisible(parcels)
}

regdb_make_stata_script_parcel = function(project.dir, file_df) {
  restore.point("regdb_make_script_parcels")

  script_df = file_df %>%
    ungroup() %>%
    filter(file_type == "do") %>%
    mutate(
      sup_dir = paste0(project.dir, ifelse(in_org, "/org","/mod")),
      long_path = paste0(sup_dir,"/", file_path ),
      script_num = seq_len(n()),
      file_exists = file.exists(long_path),
      source_added = file_exists
    )
  text = rep(NA_character_, NROW(script_df))
  num_lines = rep(NA_integer_, NROW(script_df))

  for (i in seq_len(NROW(script_df))) {
    if (script_df$file_exists[i]) {
      txt = readLines(script_df$long_path[i],encoding = "UTF-8")
      text[i] = merge.lines(txt)
      num_lines[i] = length(txt)
    }
  }
  script_df$num_lines = num_lines
  script_df$text = text

  parcels = list(
    stata_file = list(script_file=script_df),
    stata_source = list(script_source = script_df)
  )
  regdb_save_parcels(parcels, dir = file.path(project.dir, "repbox", "regdb") )
  return(script_df)
}

repbox_results_to_regdb = function(project.dir, script_df) {
  restore.point("repbox_results_to_regdb")

  parcels = list()

  artid = basename(project.dir)

  results.file = file.path(project.dir, "repbox", "stata","repbox_results.Rds")
  res = readRDS.or.null(results.file)

  if (is.null(res)) {
    parcels = list()
    return(parcels)
  }

  dotab = res$dotab
  dotab$file_path = str.right.of(dotab$file, paste0("/",artid,"/mod/"))

  if (!has.col(res$tab,"in_loop")) {
    cat("\nNote there was no 'in_loop' col in repbox_results$tab\n")
    res$tab$in_loop = NA_integer_
  }

  cmd_df = res$tab %>%
    mutate(
      artid = artid,
      in_loop = in_loop==1,
      in_program = is.true(in.program>=1)
    ) %>%
    rename(
      prefix_cmd1 = colon_cmd1,
      prefix_cmd2 = colon_cmd2,
      num_runs = runs,
      num_err_runs = errruns,
      is_reg = is.regcmd
    ) %>%
    left_join(select(dotab, donum, file_path), by="donum")

  regdb_check_data(cmd_df,"stata_cmd")
  parcels$stata_cmd = list(stata_cmd = cmd_df)

  run_df = res$run.df %>%
    mutate(
      artid = artid,
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project.dir, "/", artid, "/mod/"))
    ) %>%
    rename(

      start_time = stime,
      end_time = etime,
      errcode = runerrcode,
      errmsg = runerrmsg,
      out_img_file = out.img.file
    ) %>%
    left_join(select(dotab, donum, file_path), by="donum") %>%
    left_join(select(dotab, rootdonum = donum, root_file_path = file_path), by="rootdonum")

  run_df$runid = seq_len(NROW(run_df))
  run_df = left_join(run_df, select(script_df, file_path, script_num), by="file_path")


  regdb_check_data(run_df,"stata_run_cmd")
  regdb_check_data(run_df,"stata_run_log")

  parcels$stata_run_cmd = list(stata_run_cmd = run_df)
  parcels$stata_run_log = list(stata_run_log = run_df)

  regdb_save_parcels(parcels, dir = file.path(project.dir, "repbox", "regdb") )
  invisible(parcels)
}

repbox_file_to_regdb = function(project.dir, ignore="repbox_") {
  restore.point("repbox_file_to_regdb")

  artid = basename(project.dir)
  org_files = readRDS.or.null(file.path(project.dir, "repbox","org_files.Rds"))
  mod_files = readRDS.or.null(file.path(project.dir, "repbox","mod_files.Rds"))

  file_df = bind_rows(
    mutate(org_files, type="org"),
    mutate(mod_files, type="mod") %>% filter(!startsWith(base,"repbox_"))
  )

  file_df = file_df %>%
    group_by(file) %>%
    arrange(desc(type)) %>%
    mutate(
      org_mtime = ifelse(type=="org", as.character(mtime),NA_character_),
      mod_mtime = ifelse(type=="mod", as.character(mtime),NA_character_),
      mb_org = ifelse(type=="org", size / 1e6,NA_real_),
      mb_mod = ifelse(type=="mod", size / 1e6,NA_real_),

    ) %>%
    summarize(
      artid = artid,
      file_name = first(base),
      file_type = tolower(first(ext)),
      mb = first(size) / 1e6,
      mb_org = first(size) / 1e6,
      mb_mod = first(size) / 1e6,
      mtime_org = first(na.omit(org_mtime)),
      mtime_mod = first(na.omit(mod_mtime)),
      in_org = any(type=="org"),
      in_mod = any(type=="mod")
    ) %>%
    rename(file_path = file)

  parcels = list(repbox_file = list(repbox_file=file_df))
  regdb_save_parcels(parcels,dir = file.path(project.dir, "repbox", "regdb"))
  invisible(file_df)
}


file_path_relative_to_supp = function(file_path, keep_from, wdir=NULL, supp.dir=NULL) {
  restore.point("file_path_relative_to")
  result = file_path

  empty_rows = file_path == ""
  abs_rows = is_abs_path(file_path)
  rel_rows = which(!abs_rows & !empty_rows)
  abs_rows = which(abs_rows)

  res_abs = abs_file_path_relative_to_supp(file_path[abs_rows], first(keep_from))
  result[abs_rows] = res_abs

  if (length(rel_rows)==0) {
    return(result)
  }

  res_rel = rel_file_path_relative_to_supp(file_path[rel_rows],first(keep_from), wdir[rel_rows],first(supp.dir))
  result[rel_rows] = res_rel

  result


}

rel_file_path_relative_to_supp = function(file_path, keep_from, wdir, supp.dir) {
  restore.point("rel_file_path_relative_to_supp")

  abs_wdir = abs_file_path_relative_to_supp(wdir, keep_from)

  ..rows = which(startsWith(file_path,"../"))
  while(length(..rows)>0) {
    file_path[..rows] = str.right.of(file_path[..rows],"../")
    abs_wdir[..rows] = dirname(abs_wdir[..rows])
    ..rows = which(startsWith(file_path,"../"))
  }
  paste0(abs_wdir, "/", file_path)
}


abs_file_path_relative_to_supp = function(file_path, keep_from) {
  restore.point("abs_file_path_relative_to_supp")
  res_abs = str.right.of(file_path, keep_from,not.found = rep(NA, length(file_path)))
  root.rows = which(endsWith(file_path, str.remove.ends(keep_from,0,1)) & is.na(res_abs))
  res_abs[root.rows] = "."
  if (any(is.na(res_abs))) {
    stop("Not all absolute paths could be translated.")
  }
  return(res_abs)
}

is_abs_path = function(x) {
  startsWith(x, "/") | startsWith(x,"~") | grepl(":",x, fixed=TRUE)
}
