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
  map(.f = parse_enfusion_conf) ->
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
  map(function(item_property_df) item_property_df %>%
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
                                                    pattern = paste(sep = "|",
                                                                    "Jerrycan",
                                                                    "RepairKit")) ~ "4",
                                         str_detect(string =  m_sEntityPrefab, 
                                                    pattern = "PersonalBelongings_CIV") ~ m_iSupplyCost,
                                         m_eItemType == "EQUIPMENT" ~ "2"),
               m_bEnabled = case_when(.default = "1",
                                      m_eItemType %in% c("MORTARS",
                                                         "HELICOPTER") ~ 
                                        "0",
                                      str_detect(string = m_sEntityPrefab,
                                                 pattern = paste("RPG",
                                                                 "M70",
                                                                 paste0("PaperMap_01_folded_",
                                                                        c("FIA",
                                                                          "USSR"),
                                                                        collapse = "|"),,
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
                                                   pattern = paste(sep = "|",
                                                                   "Bayonet_6Kh4",
                                                                   "Knife_Bayonet",
                                                                   "PSO1",
                                                                   "PBS4")) ~ 
                                        "0"))) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]]


## Check the result ####  
View(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]] %>%
       list_rbind() %>%
       # dplyr::filter(m_bEnabled == "0") %>%
       arrange(m_eItemType,
               Item_prefab,
               file_name))

# -- 4. Saving the modified propertied in .conf files -- ####
## Creating the Output Directory ####
# Setting the path
file.path(Paths[["Outputs"]]$path,
          "DynamicLootReblance",
          "Configs",
          "EntityCatalog",
          "CIV_DL") ->
  Paths[["Outputs"]][["CIV_DL EntityCatalog"]]

# Creating the directory
if(! dir.exists(Paths[["Outputs"]][["CIV_DL EntityCatalog"]])) {
  dir.create(path = Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
             recursive = TRUE,
             showWarnings = FALSE)
}

## Re-linking Context and Finding Changed Values -- ####
# Extract original context strings for each Item_GUID from the references
Data[["InventoryItems_EntityCatalog"]][["references"]] %>%
  bind_rows() %>%
  mutate(Item_GUID = str_extract(context,
                                 '(?<=SCR_EntityCatalogInventoryItem "\\{)[A-Z0-9]+(?=\\}")')) %>%
  dplyr::filter(!is.na(Item_GUID)) %>%
  dplyr::select(file_name,
                Item_GUID,
                context) %>%
  distinct() ->
Data[["InventoryItems_EntityCatalog"]][["references with Item_GUID"]]

# Compare raw and modified dataframes to find specific changed properties
map2_dfr(.x = Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
           keep_at(names(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]])),
         .y = Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]],
         .f = \(raw_df,
           mod_df) {
           
           # Pivot back to longer format for exact property-to-property comparison
           raw_df %>%
             pivot_longer(cols = -c(file_name,
                                    Item_GUID,
                                    m_sEntityPrefab,
                                    Item_prefab),
                          names_to = "property",
                          values_to = "raw_value") ->
             raw_long
           
           mod_df %>%
             pivot_longer(cols = -c(file_name,
                                    Item_GUID,
                                    m_sEntityPrefab,
                                    Item_prefab),
                          names_to = "property",
                          values_to = "mod_value") ->
             mod_long
           
           # Join and isolate the differences
           inner_join(x = raw_long,
                      y = mod_long, 
                      by = c("file_name",
                             "Item_GUID",
                             "m_sEntityPrefab",
                             "Item_prefab",
                             "property")) %>%
             dplyr::filter(raw_value != mod_value) %>%
             dplyr::select(file_name,
                           Item_GUID,
                           property,
                           mod_value)
         }) -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]

# Join the Enfusion context back to the changes and apply the grouping regex
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
  left_join(Data[["InventoryItems_EntityCatalog"]][["references with Item_GUID"]],
            by = c("file_name",
                   "Item_GUID")) %>%
  mutate(group_name = str_extract(file_name,
                                  "InventoryItems_EntityCatalog_[A-Z]+")) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]

## Mapping Vanilla GUIDs for Override Inheritance from .meta files ####
Paths[["Inputs"]]$path %>%
  list.files(full.names = T,
             recursive = T,
             pattern = "\\.conf\\.meta$") %>%
  set_names(nm = basename(.) %>%
              str_remove(pattern = "\\.conf\\.meta$")) %>%
  map(.f = \(path) {
    readLines(con = path,
              warn = FALSE) %>%
      str_subset(pattern = "Name \"\\{") %>%
      str_extract(pattern = '(?<=Name ").+(?=")') %>%
      unique()
  }) ->
  Data[["InventoryItems_EntityCatalog"]][["Vanilla_Overrides"]]


## Generating and Saving the .conf Override Files -- ####
for (grp in Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
     pull(var = group_name) %>%
     unique()) {
  if (is.na(grp)) next
  
  # Filter data for this specific regex group
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
    dplyr::filter(group_name == grp) ->
    grp_data
  
  file.path(Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
            paste0(grp,
                   "_for_CIV_DL.conf")) ->
    output_file
  
  file(output_file,
       "w") ->
    con
  
  # Get all unique contexts and sort them to maintain structural/hierarchical order
  grp_data %>%
    pull(var = context) %>%
    unique() %>%
    sort() ->
    contexts
  
  character(0) ->
    current_path
  
  "" ->
    indent
  
  for (ctx in contexts) {
    str_split(ctx, " > ")[[1]] ->
      target_path
    
    # Find common prefix length between current_path and target_path
    0 ->
      common_len
    
    if (length(current_path) > 0 && length(target_path) > 0) {
      for (i in 1:min(length(current_path), length(target_path))) {
        if (current_path[i] == target_path[i]) {
          i -> common_len
        } else {
          break
        }
      }
    }
    
    # Close blocks that are in current_path but not in target_path (climbing UP the tree)
    if (length(current_path) > common_len) {
      for (i in length(current_path):(common_len + 1)) {
        substr(indent, 1, nchar(indent) - 2) ->
          indent
        writeLines(paste0(indent, "}"),
                   con)
      }
    }
    
    # Open blocks that are in target_path but not in current_path (climbing DOWN the tree)
    if (length(target_path) > common_len) {
      for (i in (common_len + 1):length(target_path)) {
        target_path[i] ->
          block_name
        
        # Add Enfusion inheritance pointer to the absolute root block
        if (i == 1) {
          Data[["InventoryItems_EntityCatalog"]][["Vanilla_Overrides"]][[grp]] ->
            override_path
          
          if (!is.null(override_path)) {
            paste0(block_name,
                   " : \"",
                   override_path,
                   "\"") ->
              block_name
          }
        }
        
        writeLines(paste0(indent,
                          block_name,
                          " {"),
                   con)
        
        paste0(indent,
               "  ") ->
          indent
      }
    }
    
    target_path ->
      current_path
    
    # Write the modified properties for this specific depth context
    grp_data %>%
      dplyr::filter(context == ctx) ->
      item_props
    
    for (i in 1:nrow(item_props)) {
      item_props$property[i] ->
        prop
      item_props$mod_value[i] ->
        val
      
      # Enfusion string prefix handling
      if (str_starts(prop, "m_s")) {
        paste0('"', val, '"') ->
          val
      }
      
      writeLines(paste0(indent,
                        prop,
                        " ",
                        val),
                 con)
    }
  }
  
  # Close any remaining open blocks at the end of the file
  if (length(current_path) > 0) {
    for (i in length(current_path):1) {
      substr(indent, 1, nchar(indent) - 2) ->
        indent
      writeLines(paste0(indent, "}"),
                 con)
    }
  }
  
  close(con)
  message(paste("Saved Arma Reforger override:",
                output_file))
}