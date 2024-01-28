example = function() {
  dat = data.frame(x =1:3, a="a",date = Sys.Date())
}

repdb_make_spec_template = function(dat, to_clipboard=TRUE) {
  field_type_fun = function(val) {
    if (is.integer(val)) return("int")
    if (is.logical(val)) return("bool")
    if (is.numeric(val)) return("num")
    if (is(val,"Date")) return("date")
    if (is(val,"POSIXct")) return("datetime")
    return("")
  }

  field_types = sapply(dat, field_type_fun)
  cols = setdiff(colnames(dat),"artid")

  field_txt = paste0("  ", cols, ":\n","    descr:\n",
                     ifelse(field_types=="","",paste0("    type: ", field_types,"\n")))

  txt =
paste0("name:
descr:

unique_index:

fields:
  artid:

", paste0(field_txt, collapse="\n"))
  cat(txt)
  #if (to_clipboard) clipr::write_clip(txt)
  invisible(txt)
}
