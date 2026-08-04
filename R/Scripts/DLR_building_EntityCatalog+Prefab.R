# -- 1. Loading Input files -- ####
## Setting the Paths to the files in the R folder ####
file.path(".",
          "R",
          "Inputs") %>%
  list.files(full.names = T,
             recursive = T,
             pattern = "Inventory.+\\.conf$") %>%
  set_names(nm = basename(.) %>%
              str_remove("\\..+$")) ->
  Paths[["Inputs"]][["InventoryItems_EntityCatalog"]]


## Loading .conf files ####
Paths[["Inputs"]][["InventoryItems_EntityCatalog"]] %>%
  map(.f = parse_enfusion_conf) %>%
  keep_at(~ str_detect(string = .x,
                     pattern = paste("CIV",
                                     "USSR",
                                     "FIA",
                                     sep = "|")))->
  Data[["InventoryItems_EntityCatalog"]][["references"]]


# Modifying Entity Catalogs
# -- 2. Preparing Data -- ####
## Preparing Item properties dataframes ####
Data[["InventoryItems_EntityCatalog"]][["references"]] %>%
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
        dplyr::select(where(~ !all(is.na(.x))))) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]]


# -- 3. Adjusting values -- ####
## Preparing modified properties dataframes ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
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
  }) ->
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
  Data[["InventoryItems_EntityCatalog"]][["Vanilla_Overrides"]]


## Re-linking Context and Finding Changed Values -- ####

# 1. Map every existing property to its context, grouped by base faction
Data[["InventoryItems_EntityCatalog"]][["references"]] %>%
  bind_rows() %>%
  mutate(Item_GUID = str_extract(string = context,
                                 pattern = '(?<=SCR_EntityCatalogInventoryItem "\\{)[A-Z0-9]+(?=\\}")'),
         is_base_file = str_detect(string = file_name,
                                   pattern = "InventoryItems_EntityCatalog_[A-Z]{2,4}\\.conf$")) %>% {
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

# Canonical exact context map: first occurrence across files under each group_name
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


# 2. Canonical base block context map for each Item (fallback for brand-new properties)
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


# 3. Compare raw and modified dataframes using LEFT JOIN to catch modified/added values
map2_dfr(.x = Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
           keep_at(names(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]])),
         .y = Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]],
         .f = \(raw_df, mod_df) {
           
           raw_df %>%
             pivot_longer(cols = -c(file_name, Item_GUID, m_sEntityPrefab, Item_prefab),
                          names_to = "property", values_to = "raw_value") -> raw_long
           
           mod_df %>%
             pivot_longer(cols = -c(file_name, Item_GUID, m_sEntityPrefab, Item_prefab),
                          names_to = "property", values_to = "mod_value") -> mod_long
           
           mod_long %>%
             left_join(x = ., y = raw_long, 
                       by = c("file_name", "Item_GUID", "m_sEntityPrefab", "Item_prefab", "property")) %>%
             # Keep ONLY non-NA values that have actually changed from raw_value
             dplyr::filter(!is.na(mod_value) & mod_value != "NA" & (is.na(raw_value) | raw_value != mod_value)) %>%
             dplyr::select(file_name, Item_GUID, property, mod_value)
         }) -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]


# 4. Join context and deduplicate to guarantee 1 entry per property per group
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
  mutate(group_name = file_name %>%
           str_extract(pattern = paste0("(?<=^|_)",
                                        collapse = "|",
                                        unique(Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
                                                 dplyr::filter(is_base_file) %>%
                                                 pull(var = group_name))))) %>%
  distinct(group_name,
           Item_GUID,
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
          vanilla_order) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]


## Generating and Saving the .conf Override Files -- ####

### 1. Define recursive function to build tree environment (updates existing properties to prevent duplicates) ####
add_to_tree <- function(env, path, prop, val) {
  if (length(path) == 0) {
    if (!exists(".props", envir = env)) {
      list() -> env$.props
    }
    prop_names <- sapply(env$.props, function(x) x$prop)
    if (prop %in% prop_names) {
      idx <- which(prop_names == prop)
      env$.props[[idx]]$val <- val
    } else {
      append(env$.props, list(list(prop = prop, val = val))) -> env$.props
    }
    return()
  }
  
  path[1] -> node
  
  if (!exists(node, envir = env)) {
    new.env(parent = emptyenv()) -> env[[node]]
    if (!exists(".keys", envir = env)) {
      character(0) -> env$.keys
    }
    c(env$.keys, node) -> env$.keys
  }
  
  add_to_tree(env = env[[node]], path = path[-1], prop = prop, val = val)
}


### 2. Create the Exact Context Map ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  dplyr::filter(!is.na(property)) %>%
  dplyr::select(file_name,
                Item_GUID,
                property,
                context,
                vanilla_order) %>%
  distinct() ->
  Data[["InventoryItems_EntityCatalog"]][["Exact_Context_Map"]]

### 3. Define recursive function to write out the tree structure ####
write_tree <- function(env, indent_level, file_con, is_root = FALSE, override = NULL) {
  
  # Write node properties first
  if (exists(".props", envir = env)) {
    
    # Sort properties to place m_sEntityPrefab at the top if present
    env$.props[order(sapply(env$.props, function(x) {
      if (x$prop == "m_sEntityPrefab") return(1)
      if (startsWith(x$prop, "m_e")) return(2)
      if (startsWith(x$prop, "m_i")) return(3)
      if (startsWith(x$prop, "m_b")) return(4)
      return(5)
    }))] -> env$.props
    
    for (p in env$.props) {
      p$prop -> prop
      p$val -> val
      
      if (is.na(val) || val == "NA") next
      
      if (str_starts(prop, "m_s")) {
        paste0('"', val, '"') -> val
      }
      writeLines(paste0(indent_level, "  ", prop, " ", val), file_con)
    }
  }
  
  # Write nested children blocks
  if (exists(".keys", envir = env)) {
    for (k in env$.keys) {
      child_env <- env[[k]]
      has_props <- exists(".props", envir = child_env) && length(child_env$.props) > 0
      has_keys <- exists(".keys", envir = child_env) && length(child_env$.keys) > 0
      if (!has_props && !has_keys) next
      
      k -> header
      
      if (is_root && !is.null(override)) {
        paste0(header, " : \"", override, "\"") -> header
      }
      
      writeLines(paste0(indent_level, header, " {"), file_con)
      write_tree(env = child_env, indent_level = paste0(indent_level, "  "), file_con = file_con, is_root = FALSE, override = NULL)
      writeLines(paste0(indent_level, "}"), file_con)
    }
  }
}


### 4. Loop through files and generate outputs ####
for (grp in Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
     pull(var = group_name) %>%
     unique()) {
  if (is.na(grp)) next
  
  
  # CRITICAL FIX: Pull the complete original context mapping for this file group
  # so that unchanged structural blocks and their vanilla GUIDs are fully preserved.
  # Data[["InventoryItems_EntityCatalog"]][["Exact_Context_Map"]] %>%
  #   dplyr::filter(str_detect(file_name,
  #                            grp)) ->
  #   grp_full_structure
  
  # Combine or override the structure with your modified values
  # (or construct the tree using the full vanilla reference structure, injecting modified values where they match)
  file(file.path(Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
                 paste0("InventoryItems_EntityCatalog_",
                        grp, "_for_CIV_DL.conf")), "w") -> 
    con
  
  # Option A: If you want to build from the full vanilla tree framework and overwrite changed values:
  # Load all rows from the original reference file for this group, then overlay `grp_changes`
  Data[["InventoryItems_EntityCatalog"]][["references"]][[paste0("InventoryItems_EntityCatalog_",
                                                                 grp)]] %>% 
    distinct() %>%
    # Apply your modifications onto the vanilla dataframe values
    left_join(# Get changed properties for this group
      Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
        dplyr::filter(group_name == grp) 
      %>% select(# file_name,
                                     context,
                                     property,
                                     mod_value),
              by = c(#"file_name",
                     "context", "property")) %>%
    mutate(final_val = coalesce(mod_value,
                                value)) -> 
    Data[["InventoryItems_EntityCatalog"]][["combined_tree_data"]][[grp]]
  
  new.env(parent = emptyenv()) -> 
    Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]]
  
  # Populate the tree using the complete vanilla hierarchy with modified values injected
  for (i in 1:nrow(Data[["InventoryItems_EntityCatalog"]][["combined_tree_data"]][[grp]])) {
    
    add_to_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
                path = str_split(Data[["InventoryItems_EntityCatalog"]][["combined_tree_data"]][[grp]]$context[i], " > ")[[1]], 
                prop = Data[["InventoryItems_EntityCatalog"]][["combined_tree_data"]][[grp]]$property[i], 
                val = Data[["InventoryItems_EntityCatalog"]][["combined_tree_data"]][[grp]]$final_val[i])
  }
  rm(i)
  
  # 6. Serialize and write the file recursively
  write_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
             indent_level = "", 
             file_con = con, 
             is_root = TRUE, 
             override = Data[["InventoryItems_EntityCatalog"]][["Vanilla_Overrides"]][[grp]])
  
  close(con)
  message(paste("Saved Arma Reforger override for:",
                grp))
}

