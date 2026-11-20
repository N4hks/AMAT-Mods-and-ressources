library(tidyverse)
library(jsonlite)
library(fs)

# ---- 1. Register paths in `Paths` ----
path("C:",
     "Users",
     Sys.info()[["user"]],
     "Documents",
     "My Games") ->
  Paths[["User My Games"]]$path

path(Paths[["User My Games"]]$path,
     "ArmaReforger",
     "addons") ->
  Paths[["User My Games"]][["Reforger addons"]]$path

path(Paths[["User My Games"]]$path,
     "ArmaReforgerWorkbench",
     "addons") ->
  Paths[["User My Games"]][["Workbench addons"]]$path

path(Paths[["Inputs"]]$path,
     "Modlist") %>%
  list.files(
    pattern = "-modlist\\.json$",
    full.names = TRUE
  ) %>%
  {
    tibble(file_path = .) %>%
      mutate(mtime = file.info(file_path)$mtime) %>%
      arrange(desc(mtime), desc(file_path)) %>%
      slice_head(n = 1) %>%
      pull(file_path)
  } %>%
  path() ->
  Paths[["Inputs"]][["Latest modlist"]]$path

path(Paths[["Outputs"]]$path,
     "Workbench Presets") ->
  Paths[["Outputs"]][["Workbench preset"]]$path

# ---- 2. Build & store recurrently used preset data in `Data` ----
c(
  Workbench = Paths[["User My Games"]][["Workbench addons"]]$path,
  Reforger = Paths[["User My Games"]][["Reforger addons"]]$path
) %>%
  .[dir.exists(.)] %>%
  imap_dfr(function(addon_dir, source_label) {
    list.files(
      path = addon_dir,
      pattern = "\\.gproj$",
      recursive = TRUE,
      full.names = TRUE
    ) %>%
      tibble(
        gproj_path = .,
        source = source_label,
        source_priority = if_else(source_label == "Workbench", 1L, 2L)
      )
  }) %>%
  mutate(
    gproj_path = str_replace_all(gproj_path, "\\\\+", "/"),
    folder_name = basename(dirname(gproj_path)),
    folder_modId = str_to_upper(str_match(folder_name, "_([0-9A-Fa-f]+)$")[, 2]),
    gproj_guid = map_chr(gproj_path, function(p) {
      if (!file.exists(p)) return(NA_character_)
      p %>%
        read_file() %>%
        str_match('GUID\\s+"([0-9A-Fa-f]+)"') %>%
        .[1, 2] %>%
        str_to_upper()
    }),
    modId = coalesce(folder_modId, gproj_guid),
    norm_folder = str_to_lower(str_remove(folder_name, "_[0-9A-Fa-f]+$")) %>%
      str_replace_all("[^a-z0-9]+", "")
  ) %>%
  dplyr::select(-folder_modId, -gproj_guid) %>%
  arrange(source_priority, gproj_path) %>%
  (function(gproj_index) {
    id_lookup <- gproj_index %>%
      dplyr::filter(!is.na(modId)) %>%
      distinct(modId, .keep_all = TRUE) %>%
      dplyr::select(modId, id_path = gproj_path, id_source = source)
    
    name_lookup <- gproj_index %>%
      distinct(norm_folder, .keep_all = TRUE) %>%
      dplyr::select(norm_folder, fallback_path = gproj_path, fallback_source = source)
    
    Paths[["Inputs"]][["Latest modlist"]]$path %>%
      read_file() %>%
      str_trim() %>%
      {
        if (!str_starts(., "\\[")) {
          str_c("[", str_remove(., ",\\s*$"), "]")
        } else {
          .
        }
      } %>%
      fromJSON() %>%
      as_tibble() %>%
      mutate(
        modId = str_to_upper(str_trim(modId)),
        norm_name = str_to_lower(name) %>% str_replace_all("[^a-z0-9]+", "")
      ) %>%
      left_join(id_lookup, by = "modId") %>%
      left_join(name_lookup, by = c("norm_name" = "norm_folder")) %>%
      mutate(
        gproj_path = coalesce(id_path, fallback_path),
        source = coalesce(id_source, fallback_source)
      ) %>%
      dplyr::select(-norm_name, -id_path, -id_source, -fallback_path, -fallback_source)
  }) %>%
  {
    . ->> Data[["Workbench_Preset"]][["matched_mods"]]
    dplyr::filter(., is.na(gproj_path)) ->> Data[["Workbench_Preset"]][["missing_mods"]]
    .
  } %>%
  dplyr::filter(!is.na(gproj_path)) %>%
  pull(gproj_path) %>%
  str_replace(pattern = "ArmaReforgerWorkbench/addons/Radio - Les Cires Jaunes",
              replacement = "ArmaReforger/addons/Radio-LesCiresJaunes_69C8B53172851162") ->
  Data[["Workbench_Preset"]]$full_latest_preset

Data[["Workbench_Preset"]]$full_latest_preset %>%
  write_lines(path(Paths[["Outputs"]][["Workbench preset"]]$path,
                   Paths[["Inputs"]][["Latest modlist"]]$path %>%
                     basename() %>%
                     str_replace("-modlist\\.json$", "-workbench-preset.txt")),
              sep = "\r\n")

Data[["Workbench_Preset"]]$full_latest_preset %>%
  str_subset(negate = T,
             pattern = paste0("(",
                              paste(collapse = ")|(",
                                    c("C:/Users/Nahks/Documents/My Games/ArmaReforgerWorkbench/addons/No FIA in FF-DLRebalance/addon.gproj",
                                       "C:/Users/Nahks/Documents/My Games/ArmaReforgerWorkbench/addons/Dynamic Loot Rebalance/addon.gproj",
                                       "C:/Users/Nahks/Documents/My Games/ArmaReforgerWorkbench/addons/FF-Dynamic Loot Rebalance/addon.gproj")),
                              ")")) %>%
  append("C:/Users/Nahks/Documents/My Games/ArmaReforgerWorkbench/addons/CopyEntityCatalogPlugin/addon.gproj") %>%
  write_lines(path(Paths[["Outputs"]][["Workbench preset"]]$path,
                   Paths[["Inputs"]][["Latest modlist"]]$path %>%
                     basename() %>%
                     str_replace("-modlist\\.json$", "-workbench-preset_for_entity_catalog.txt")),
              sep = "\r\n")


# ---- 3. Report missing mods if any ----
if (nrow(Data[["Workbench_Preset"]][["missing_mods"]]) > 0) {
  message("Mods in JSON not found in scanned folders:")
  Data[["Workbench_Preset"]][["missing_mods"]] %>%
    dplyr::select(modId, name) %>%
    print()
}