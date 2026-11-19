# /!\ Copy/paste the loot table from Dynamic Loot ran with all mods including Dynamic Loot Reblance ? /!\
file.path(Paths$Inputs$path,
          "DL_LootSystem") %>%
  list.files(full.names = TRUE) %>%
  file.info() %>%
  rownames_to_column(var = "filepath") %>%
  dplyr::filter(str_detect(string = filepath,
                           pattern = "DL_LootSystem Loot table ")) %>%
  arrange(desc(mtime)) %>%
  slice(1) %>%
  pull(filepath) %>%
  read_lines() %>%
  str_subset("\\S") %>%
  str_squish() %>%
  tibble(raw_line = .) %>%
  extract(col = raw_line,
          into = c("Log_channel",
                   "Loot_system",
                   "Faction_key",
                   "Object_ptr",
                   "Loot_categories",
                   "Entry_kind",
                   "Prefab_path",
                   "Weight"),
          regex = paste0("^(\\S+)\\s*:\\s*(\\S+):\\s*([^\\]\\s]+)",
                         "(?:\\]?\\(([^\\s\\)]+)\\)?)?",
                         "\\s+(.+?)\\s+(\\S+)\\s+",
                         "(\\{?[0-9A-Fa-f]+\\}?Prefabs/.+?\\.et)",
                         "(?:\\s*=\\s*([0-9]+(?:\\.[0-9]+)?)\\s*weight)?$"),
          remove = FALSE) %>%
  mutate(Weight = as.numeric(Weight),
         Faction_key = str_remove(string = Faction_key,
                                  pattern = "^\\["),
         Item_mod = str_extract(string = Prefab_path,
                                pattern = "^\\{?([0-9A-Fa-f]+)\\}?") %>%
           str_remove_all("[\\{\\}]"),
         Item_type = str_remove(string = Prefab_path,
                                pattern = "^\\{?[0-9A-Fa-f]+\\}?Prefabs(?:/(?:LCJ|Modded))?/") %>%
           str_extract(pattern = "^[^/]+/[^/]+"),
         Prefab_file = basename(Prefab_path),
         Prefab_name = str_remove(string = Prefab_file,
                                  pattern = "(_CIV)?\\.et$"),
         Loot_categories = Loot_categories %>%
           str_remove_all("[\\{\\}\\[\\]\\(\\)]") %>%
           str_squish(),
         CIV_duplicate = str_detect(string = Prefab_file,
                                    pattern = "_CIV.et")) %>%
  separate(col = Item_type,
           sep = "/",
           into = c("Item_type",
                    "Item_subtype"),
           fill = "right") %>%
  mutate(Item_subtype = if_else(condition = str_detect(string = Prefab_path,
                                                       pattern = "LCJ|T33"),
                                true = case_when(str_detect(string = Prefab_path,
                                                            pattern = "Magazine") ~ "Magazines",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "T33") ~ "Handguns",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Rifle") ~ "Rifles",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "MG") ~ "MachineGuns",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Backpack") ~ "Equipment",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Launcher") ~ "Launchers",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Jacket") ~ "Uniforms",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Pants") ~ "Pants",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Vest") ~ "Vests",
                                                 str_detect(string = Prefab_path,
                                                            pattern = "Binocular") ~ "Equipment",
                                                 Item_subtype == "Ammunitions" ~ "Magazines",
                                                 .default = NA_character_),
                                false = Item_subtype)) %>% {
                                  data <- .
                                  
                                  lookup_types <- data %>%
                                    dplyr::filter(!is.na(Item_type),
                                                  Item_type != "LCJ") %>%
                                    dplyr::select(Item_type,
                                                  Item_subtype) %>%
                                    distinct()
                                  
                                  data %>%
                                    dplyr::filter(is.na(Item_type) | Item_type != "LCJ") %>%
                                    bind_rows(data %>%
                                                dplyr::filter(Item_type == "LCJ") %>%
                                                dplyr::select(-Item_type) %>%
                                                left_join(lookup_types,
                                                          by = join_by(Item_subtype)))
                                } %>%
  distinct() %>%
  separate_longer_delim(cols = Loot_categories,
                        delim = ",") %>%
  mutate(Loot_category = str_squish(Loot_categories)) %>%
  dplyr::filter(!is.na(Loot_category),
                Loot_category != "") %>%
  distinct() %>%
  mutate(active = TRUE) %>%
  pivot_wider(names_from = Loot_category,
              values_from = active,
              values_fill = FALSE) %>%
  dplyr::select(- c(raw_line,
                    Log_channel,
                    Loot_system,
                    Object_ptr,
                    Entry_kind,)) %>%
  dplyr::relocate(c(Item_type,
                    Item_subtype,
                    Prefab_name,
                    CIV_duplicate,
                    Weight,
                    Loot_categories),
                  .before = everything()) %>%
  dplyr::relocate(Faction_key,
                  Item_mod,
                  Prefab_file,
                  Prefab_path,
                  .after = everything()) %>%
  group_by(Prefab_name) %>%
  summarise(across(.cols = c(Item_type,
                             Item_subtype,
                             Weight,
                             Faction_key,
                             Loot_categories),
                   .fns = function(variable) {
                     paste(unique(variable),
                           collapse = " ; ")
                   }),
            across(.cols = c(CIV_duplicate,
                             URBAN,
                             RURAL,
                             MEDICAL,
                             COMMERCIAL,
                             INDUSTRIAL,
                             POLICE,
                             MILITARY),
                   .fns = any)) %>%
  arrange(MILITARY,
          INDUSTRIAL,
          POLICE,
          COMMERCIAL,
          MEDICAL,
          RURAL,
          URBAN,
          Item_type,
          Item_subtype,
          Prefab_name)->
  Data[["DL_LootSytem"]][["Loot tables"]][["lastest"]][["full"]]

Data[["DL_LootSytem"]][["Loot tables"]][["lastest"]][["full"]]

Data[["DL_LootSytem"]][["Loot tables"]][["lastest"]][["full"]] %>%
  write_tsv(file = file.path(Paths$Outputs$path,
                             "DL_LootSytem_Loot_table-lastest.tsv"))


