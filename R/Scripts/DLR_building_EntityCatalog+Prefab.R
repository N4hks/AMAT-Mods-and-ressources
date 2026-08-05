# -- 1. Loading Input files -- ####
## Setting the Paths to the files in the R folder ####
file.path(".",
          "R",
          "Inputs",
          "Configs",
          "EntityCatalog") %>%
  list.files(full.names = T,
             recursive = T,
             pattern = paste0("(",
                              paste("Inventory",
                                    "Ammunition",
                                    "Attachment",
                                    "Backpack",
                                    "Clothing",
                                    "Deployable",
                                    "Equipment",
                                    "Explosive",
                                    "Shops",
                                    "Vest",
                                    "Weapons",
                                    sep = "|"),
                              ").*\\.conf$")) %>%
  str_subset(pattern = paste("CIV",
                             "USSR",
                             "FIA",
                             sep = "|")) %>%
  # 1. Convert the vector to a dataframe for easier manipulation
  tibble(full_path = .) %>%
  # 2. Extract the Level 1 and Level 2 names
  mutate(level_1_folder = str_extract(string = full_path,
                                      pattern = "(?<=/Copy/)[^/]+"),
         level_2_file = basename(path = full_path)) %>%
  # 3. Split the dataframe by the Level 1 folder (creates a list of dataframes)
  split(f = .$level_1_folder) %>%
  # 4. Map over each dataframe to create the second nested level
  map(.f = \(df) {
    df %>%
      pull(var = full_path) %>%
      as.list() %>%
      set_names(nm = df$level_2_file)}) ->
  Paths[["Inputs"]][["InventoryItems_EntityCatalog"]]


## Loading .conf files ####
Paths[["Inputs"]][["InventoryItems_EntityCatalog"]] %>%
  map(.f =  ~.x %>%
        map(.f = parse_enfusion_conf)) ->
  Data[["InventoryItems_EntityCatalog"]][["references"]]


# Modifying Entity Catalogs
# -- 2. Preparing Data -- ####
## Preparing Item properties dataframes ####
Data[["InventoryItems_EntityCatalog"]][["references"]] %>%
  map(~ .x %>%
  keep(.p =  \(object) any(str_detect(colnames(object),
                                      pattern = "context"))) %>%
  keep(.p =  \(object) any(str_detect(colnames(object),
                                      pattern = "property"))) %>%
  keep(.p =  \(object) any(str_detect(object$property,
                                      pattern = "m_sEntityPrefab"))) %>%
  map(function(conf_df) conf_df %>%
        distinct() %>%
        mutate(Item_GUID =  str_extract(string = context,
                                        pattern = '(?<=SCR_EntityCatalogInventoryItem "\\{)[A-Z0-9]+(?=\\}")')) %>%
        dplyr::filter(! is.na(Item_GUID)) %>% 
        dplyr::select(file_name,
                      property,
                      value,
                      Item_GUID) %>%
        pivot_wider(names_from = property,
                    values_from = value,
                    values_fn = unique) %>%
        mutate(Item_prefab = str_extract(string = m_sEntityPrefab,
                                         pattern = '(?<=/)[A-Za-z0-9_\\-]+(?=\\.et$)')) %>%
        dplyr::select(where(~ !all(is.na(.x)))))) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]]


# -- 3. Adjusting values -- ####
## Preparing modified properties dataframes ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
  map(~ .x %>%
        keep_at(at = \(name) str_detect(string = name,
                                  pattern = paste("CIV",
                                                  "FIA",
                                                  "USSR",
                                                  sep = "|"))) %>%
  keep(.p =  \(object) any(str_detect(colnames(object),
                                      pattern = "m_iSupplyCost"))) %>%
  keep(.p =  \(object) any(str_detect(colnames(object),
                                      pattern = "m_eItemType"))) %>%
  map(function(item_property_df) {
    
    # Safety check: Initialize m_bEnabled as NA if the raw file omitted it
    if (!"m_bEnabled" %in% colnames(item_property_df)) {
      mutate(.data = item_property_df,
             m_bEnabled = NA_character_) ->
        item_property_df
    }
    
    item_property_df %>%
      mutate(m_iSupplyCost = case_when(.default = m_iSupplyCost,
                                       m_eItemType == "BACKPACK" &
                                         str_detect(file_name,
                                                    pattern = "_CIV") ~ "3",
                                       m_eItemType == "HEADWEAR" & 
                                         str_detect(m_sEntityPrefab,
                                                    pattern = "Helmet") ~ m_iSupplyCost,
                                       m_eItemType %in% c("HANDWEAR",
                                                          "HEADWEAR",
                                                          "LEGS",
                                                          "TORSO") &   
                                         str_detect(file_name,
                                                    pattern = "_CIV") ~ "1",
                                       str_detect(string =  m_sEntityPrefab, 
                                                  pattern = paste("Jerrycan",
                                                                  "RepairKit",
                                                                  sep = "|")) ~ "4",
                                       str_detect(string =  m_sEntityPrefab, 
                                                  pattern = "PersonalBelongings_CIV") ~ m_iSupplyCost,
                                       m_eItemType == "EQUIPMENT" ~ "2"),
             m_bEnabled = case_when(m_eItemType %in% c("MORTARS",
                                                       "HELICOPTER") ~ 
                                      "0",
                                    str_detect(string = m_sEntityPrefab,
                                               pattern = paste("RPG",
                                                               "M70",
                                                               paste0("PaperMap_01_folded_",
                                                                      c("FIA",
                                                                        "USSR"),
                                                                      collapse = "|"),
                                                               "CacheNote",
                                                               "PersonalBelongings",
                                                               "Mine",
                                                               "RearmingKit",
                                                               "Mortars",
                                                               "CombatBoots_US",
                                                               "JungleBoots",
                                                               "PeakedCap_USSR",
                                                               paste0("Helmet_",
                                                                      c("M1",
                                                                        "PASGT",
                                                                        "PASGT",
                                                                        "SSh68_",
                                                                        "TSh4",
                                                                        "ZSh5"),
                                                                      collapse = "|"),
                                                               "Hood",
                                                               "KZS",
                                                               "M88",
                                                               "Pilot",
                                                               "TAZ83",
                                                               "Tanker",
                                                               "RGD5",
                                                               "VOG25",
                                                               "RPK",
                                                               "RPD",
                                                               "NSV",
                                                               "UK59",
                                                               "PK",
                                                               "KPVT",
                                                               "1911",
                                                               "Rocket",
                                                               "Bandit",
                                                               "_Trench",
                                                               "KLMK",
                                                               "SovietHarness",
                                                               "Type56",
                                                               "_M16",
                                                               "_AKM",
                                                               "_G3",
                                                               "_UZI",
                                                               "_AK47",
                                                               "_AK74",
                                                               "VG40",
                                                               "_V[Zz]58",
                                                               "STG77_SD",
                                                               "Suppressor_T4AUG",
                                                               sep = "|")) ~ "0",
                                    str_detect(string = file_name,
                                               pattern = "USSR") & 
                                      str_detect(string = m_sEntityPrefab,
                                                 pattern = "Radios") ~ 
                                      "0",
                                    m_eItemType == "WEAPON_ATTACHMENT" &
                                      str_detect(string = m_sEntityPrefab,
                                                 negate = T,
                                                 pattern = paste("Bayonet_6Kh4",
                                                                 "Knife_Bayonet",
                                                                 "PSO1",
                                                                 "PBS4",
                                                                 sep = "|")) ~ 
                                      "0",
                                    .default = m_bEnabled))
  })) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]]

## Creating the Output Directory ####
file.path(Paths[["Outputs"]]$path,
          "DynamicLootReblance",
          "Configs",
          "EntityCatalog",
          "CIV_DL") ->
  Paths[["Outputs"]][["CIV_DL EntityCatalog"]]

if(! dir.exists(Paths[["Outputs"]][["CIV_DL EntityCatalog"]])) {
  dir.create(path = Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
             recursive = TRUE,
             showWarnings = FALSE)
}

## Mapping Vanilla Base File Paths & GUIDs for Override Headers ####
list("CIV" = "{9D7E5804BB2E9B28}Configs/EntityCatalog/CIV/InventoryItems_EntityCatalog_CIV.conf",
     "FIA" = "{E908001749419691}Configs/EntityCatalog/FIA/InventoryItems_EntityCatalog_FIA.conf",
     "USSR" = "{C53421647C3D0D2E}Configs/EntityCatalog/USSR/InventoryItems_EntityCatalog_USSR.conf",
     "US" = "{5F7EC52FC40A03E2}Configs/EntityCatalog/US/InventoryItems_EntityCatalog_US.conf") ->
  Data[["InventoryItems_EntityCatalog"]][["Vanilla_inherits"]]


## Re-linking Context and Finding Changed Values -- ####
### Map every existing property to its context, grouped by base faction ####
Data[["InventoryItems_EntityCatalog"]][["references"]] %>%
  map(~ .x %>%
        list_rbind()) %>%
  list_rbind(names_to = "folder_name") %>%
  mutate(Item_GUID = str_extract(string = context,
                                 pattern = '(?<=SCR_EntityCatalogInventoryItem "\\{)[A-Z0-9]+(?=\\}")'),
         is_base_file = str_detect(string = folder_name,
                                   pattern = "^CIV|FIA|US|USSR$") &
           str_detect(string = file_name,
                                   pattern = "InventoryItems_EntityCatalog_(CIV|FIA|US|USSR)\\.conf$")) %>% {
  # dplyr::filter(!is.na(Item_GUID)) %>% {
    . -> data 
    
    data %>%
      mutate(group_name = file_name %>%
               str_extract(pattern = paste0("(?<=^|_)",
                                            collapse = "|",
                                            data %>%
                                              dplyr::filter(is_base_file) %>%
                                              pull(var = file_name) %>%
                                              str_extract(pattern = "(?<=InventoryItems_EntityCatalog_)[A-Z]{2,4}(?=\\.conf$)") %>%
                                              unique())))
  } %>%
  arrange(group_name,
          desc(is_base_file),
          file_name) %>%
  mutate(vanilla_order = row_number()) ->
  Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]]

### Canonical exact context map: first occurrence across files under each group_name ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  dplyr::filter(!is.na(property)) %>%
  group_by(group_name,
           Item_GUID,
           property) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(group_name,
                Item_GUID,
                property,
                context,
                vanilla_order) ->
  Data[["InventoryItems_EntityCatalog"]][["Canonical_Exact_Context_Map"]]


### Canonical base block context map for each Item (fallback for brand-new properties) ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  group_by(group_name,
           Item_GUID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(base_context = str_extract(string = context,
                                    pattern = '.*SCR_EntityCatalogInventoryItem "\\{[A-Z0-9]+\\}"'),
         base_order = vanilla_order) %>%
  dplyr::select(group_name,
                Item_GUID,
                base_context,
                base_order) ->
  Data[["InventoryItems_EntityCatalog"]][["Canonical_Base_Context_Map"]]


### Compare raw and modified dataframes using LEFT JOIN to catch modified/added values ####
## Flatten the 'raw' nested list into a single long dataframe -- ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
  # bind_rows handles the inner lists, map_dfr handles the outer list
  purrr::map_dfr(.f = ~ dplyr::bind_rows(.x), .id = "folder_name") %>%
  tidyr::pivot_longer(cols = -c(folder_name, file_name, Item_GUID, m_sEntityPrefab, Item_prefab),
                      names_to = "property",
                      values_to = "raw_value") -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw_long"]]


## Flatten the 'modified' nested list into a single long dataframe -- ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]] %>%
  # CRITICAL: Drop the empty lists (e.g., ARMARYAK74) before binding to prevent errors
  purrr::keep(.p = ~ length(.x) > 0) %>% 
  purrr::map_dfr(.f = ~ dplyr::bind_rows(.x), .id = "folder_name") %>%
  tidyr::pivot_longer(cols = -c(folder_name, file_name, Item_GUID, m_sEntityPrefab, Item_prefab),
                      names_to = "property", 
                      values_to = "mod_value") -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["mod_long"]]


## Join, Compare, and Extract Changed Values -- ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["mod_long"]] %>%
  dplyr::left_join(y = Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw_long"]], 
                   by = c("folder_name", "file_name", "Item_GUID", "m_sEntityPrefab", "Item_prefab", "property")) %>%
  # Keep ONLY non-NA values that have actually changed from raw_value
  dplyr::filter(!is.na(mod_value) & 
                  mod_value != "NA" & 
                  (is.na(raw_value) | raw_value != mod_value)) %>%
  # FIX 1: Add m_sEntityPrefab here so we can deduplicate by it in the next step
  dplyr::select(folder_name, file_name, Item_GUID, m_sEntityPrefab, property, mod_value) -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]


# Join context and deduplicate to guarantee 1 entry per property per group ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
  mutate(group_name = file_name %>%
           str_extract(pattern = paste0("(?<=^|_)",
                                        collapse = "|",
                                        unique(Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
                                                 dplyr::filter(is_base_file) %>%
                                                 pull(var = group_name))))) %>%
  
  # FIX 2: Bring in the 'is_base_file' flag from our references
  left_join(x = .,
            y = Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
              dplyr::select(Item_GUID, is_base_file) %>%
              distinct(),
            by = "Item_GUID") %>%
  
  # FIX 3: Sort so TRUE base files are at the top. This guarantees we grab the pure Vanilla GUID!
  arrange(desc(is_base_file)) %>%
  
  # FIX 4: Deduplicate by PREFAB, not GUID. This strips out the third-party mod duplicates.
  distinct(group_name,
           m_sEntityPrefab, 
           property,
           .keep_all = TRUE) %>%
  
  left_join(x = .,
            y = Data[["InventoryItems_EntityCatalog"]][["Canonical_Exact_Context_Map"]],
            by = c("group_name",
                   "Item_GUID",
                   "property")) %>%
  left_join(x = .,
            y = Data[["InventoryItems_EntityCatalog"]][["Canonical_Base_Context_Map"]],
            by = c("group_name",
                   "Item_GUID")) %>%
  mutate(context = coalesce(context, base_context),
         vanilla_order = coalesce(vanilla_order, base_order + 0.1),
         parent_GUID = str_extract(string = context,
                                   pattern = '(?<=SCR_EntityCatalogMultiListEntry "\\{)[A-Z0-9]+(?=\\}")')) %>%
  group_by(parent_GUID) %>%
  mutate(entry_order = min(vanilla_order, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(entry_order,
          vanilla_order) %>%
  
  # Clean up our temporary columns so the rest of your script runs perfectly
  dplyr::select(! c(folder_name,
                    file_name,
                    is_base_file,
                    m_sEntityPrefab)) %>% 
  distinct() ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]


## Generating and Saving the .conf Override Files -- ####
### Create the Exact Context Map ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  dplyr::filter(!is.na(property)) %>%
  dplyr::select(file_name,
                Item_GUID,
                property,
                context,
                vanilla_order) %>%
  distinct() ->
  Data[["InventoryItems_EntityCatalog"]][["Exact_Context_Map"]]

### Loop through files and generate MINIMAL override outputs ####
for (grp in unique(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]$group_name)) {
  
  if (is.na(grp)) next
  
  #### 1. Filter ONLY the properties that actually changed for this faction/group ####
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
    dplyr::filter(group_name == grp) ->
    grp_changes
  
  # Safety check: If nothing changed, skip file generation entirely
  if (nrow(grp_changes) == 0) {
    message(paste("No changes detected for:", grp, "- Skipping file generation."))
    next
  }
  
  #### 2. Initialize a fresh, empty environment for the minimal tree ####
  Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]] <- new.env(parent = emptyenv())
  
  # 3. Populate the tree using ONLY the nodes that lead to modified values
  for (i in 1:nrow(grp_changes)) {
    # We use mod_value directly. We do not need the original vanilla values.
    # The 'context' path contains the necessary multi-list GUIDs for the engine to match.
    add_to_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
                path = str_split(grp_changes$context[i], " > ")[[1]], 
                prop = grp_changes$property[i], 
                val = grp_changes$mod_value[i])
  }
  rm(i)
  
  #### 4. Open connection to save the file ####
  file_out <- file.path(Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
                        paste0("InventoryItems_EntityCatalog_", grp, "_for_CIV_DL.conf"))
  con <- file(file_out, "w")
  
  
  #### 6. Serialize and write the file recursively ####
  write_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
             indent_level = "", 
             file_con = con, 
             is_root = TRUE, 
             # Fetch the dynamic inheritance header from our mapping
             override = Data[["InventoryItems_EntityCatalog"]][["Vanilla_inherits"]][[grp]])
  
  close(con)
  message(paste("Saved Arma Reforger MINIMAL override for:", grp))
  rm(con,
     file_out,
     grp_changes)
}
rm(grp)