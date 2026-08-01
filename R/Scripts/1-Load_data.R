# Loading Input files ####
## Setting the Paths to the files
file.path(".",
          "R",
          "Inputs") %>%
  list.files(full.names = T,
             recursive = T) %>%
  set_names(nm = basename(.) %>%
              str_remove("\\..+$")) ->
  Paths[["Inputs"]][["files"]]
  
# Loading .conf files ####
Paths[["Inputs"]][["files"]] %>%
  str_subset(pattern = "\\.conf$") %>%
  map(.f = parse_enfusion_conf) ->
  Data[[".conf"]]


