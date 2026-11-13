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
c("660B0207E47CE37E",
  "660B0207E47D2B31",
  "660B0207E47D2B3B",
  "69F8F1F97E225AF5",
  "69F8F1F97E22B7E0",
  "69F8F1F97E22B7B3",
  "69F90928A825E896",
  "69F8F1F97E2BE61D",
  "6594FDF053BC96BC") ->
  Data[["Problematic SCR_EntityCatalogMultiListEntry"]]

Data[["InventoryItems_EntityCatalog"]][["references"]] %>% 
  map(~ .x %>% 
        keep(.p = \(object) any(str_detect(colnames(object), pattern = "context"))) %>% 
        keep(.p = \(object) any(str_detect(colnames(object), pattern = "property"))) %>% 
        keep(.p = \(object) any(str_detect(object$property, pattern = "m_aEntities|m_sEntityPrefab|m_sEntityID"))) %>% 
        # Filtering others mods problematic references :
        keep(.p = \(object) any(str_detect(object$context,
                                           negate = T,
                                           pattern = paste(collapse = "|",
                                                           Data[["Problematic SCR_EntityCatalogMultiListEntry"]])))) %>% 
        map(function(conf_df){ 
          res <- conf_df %>% 
            distinct() %>% 
            # Generalized regex with capture group to avoid look-behind length limits & support custom Item classes
            mutate(Item_GUID = str_match(string = context, 
                                         pattern = '[A-Za-z0-9_]+Item "\\{([A-Z0-9]+)\\}\"')[, 2]) %>% 
            dplyr::filter(!is.na(Item_GUID)) %>%  
            dplyr::select(file_name, 
                          property, 
                          value, 
                          Item_GUID) %>% 
            pivot_wider(names_from = property, 
                        values_from = value, 
                        values_fn = unique)
          
          # Dynamically handle prefab vs ID extraction and unify into m_sEntityPrefab for downstream compatibility
          if ("m_sEntityPrefab" %in% colnames(res)) {
            res <- res %>% mutate(Item_prefab = str_extract(string = m_sEntityPrefab, pattern = '(?<=/)[A-Za-z0-9_\\-]+(?=\\.et$)'))
          } else if ("m_sEntityID" %in% colnames(res)) {
            res <- res %>% mutate(
              m_sEntityPrefab = m_sEntityID,
              Item_prefab = str_extract(string = m_sEntityID, pattern = '(?<=/)[A-Za-z0-9_\\-]+(?=\\.et$)')
            )
          } else {
            res <- res %>% mutate(Item_prefab = NA_character_)
          }
          
          res %>% dplyr::select(where(~ !all(is.na(.x))))
        }) 
  ) -> 
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
        # keep(.p =  \(object) any(str_detect(colnames(object),
        #                                     pattern = "m_iSupplyCost"))) %>%
        # keep(.p =  \(object) any(str_detect(colnames(object),
        #                                     pattern = "m_eItemType"))) %>%
        map(function(item_property_df) {
          
          # Safety check: Initialize m_bEnabled as NA if the raw file omitted it
          if (!"m_bEnabled" %in% colnames(item_property_df)) {
            mutate(.data = item_property_df,
                   m_bEnabled = NA_character_) ->
              item_property_df
          }
          if (!"m_iSupplyCost" %in% colnames(item_property_df)) {
            mutate(.data = item_property_df,
                   m_iSupplyCost = NA_character_) ->
              item_property_df
          }
          if (!"m_eItemType" %in% colnames(item_property_df)) {
            mutate(.data = item_property_df,
                   m_eItemType = NA_character_) ->
              item_property_df
          }
          
          item_property_df %>%
            mutate(m_iSupplyCost = case_when(.default = m_iSupplyCost,
                                             m_eItemType == "BACKPACK" &
                                               str_detect(file_name,
                                                          pattern = "_CIV") ~ 
                                               "3",
                                             m_eItemType == "HEADWEAR" & 
                                               m_iSupplyCost != "0" &
                                               str_detect(Item_prefab,
                                                          pattern = "Helmet") ~ m_iSupplyCost,
                                             # str_detect(file_name,
                                             #           pattern = "_CIV")  &
                                             m_eItemType %in% c("HANDWEAR",
                                                                "HEADWEAR",
                                                                "LEGS",
                                                                "FOOTWEAR",
                                                                "TORSO") ~
                                               "1",
                                             str_detect(string =  Item_prefab, 
                                                        pattern = paste("Jerrycan",
                                                                        "RepairKit",
                                                                        sep = "|")) ~
                                               "4",
                                             str_detect(string =  Item_prefab, 
                                                        pattern = "PersonalBelongings_CIV") ~
                                               m_iSupplyCost,
                                             m_eItemType == "EQUIPMENT" ~ "2"),
                   m_bEnabled = case_when(str_detect(string = Item_prefab,
                                                     pattern = paste("Raincoat",
                                                                     "Fisherman",
                                                                     "Kolobok",
                                                                     sep = "|")) ~ 
                                            "1",
                                          m_eItemType %in% c("MORTARS",
                                                             "HELICOPTER") ~ 
                                            "0",
                                          m_eItemType == "WEAPON_ATTACHMENT" &
                                            str_detect(string = Item_prefab,
                                                       negate = T,
                                                       pattern = paste0("(",
                                                                        paste("Bayonet(?!=_M7)",
                                                                              "GP25",
                                                                              "Mosin",
                                                                              "PSO1",
                                                                              "PBS4",
                                                                              "_PU",
                                                                              "APB",
                                                                              "Stock_VZ61",
                                                                              "Suppressor_VZ61",
                                                                              "AUG",
                                                                              "1P29",
                                                                              "PGO7V3",
                                                                              sep = ")|("),
                                                                        ")")) ~ 
                                            "0",
                                          str_detect(string = Item_prefab,
                                                     pattern = paste0("(",
                                                                      paste("RP[GDK]",
                                                                            "M70",
                                                                            paste0("PaperMap_01_folded_",
                                                                                   c("FIA",
                                                                                     "USSR"),
                                                                                   collapse = "|"),
                                                                            "CacheNote",
                                                                            "PersonalBelongings",
                                                                            "Mine",
                                                                            "Mortars",
                                                                            "CombatBoots_US",
                                                                            "JungleBoots",
                                                                            "(Jacket|Pants)_M88",
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
                                                                            "Pilot",
                                                                            "TAZ83",
                                                                            "Tanker",
                                                                            "RGD5",
                                                                            "NSV",
                                                                            "M6[09]",
                                                                            "UK59",
                                                                            "PK",
                                                                            "KPVT",
                                                                            "1911",
                                                                            "Rocket",
                                                                            "Bandit",
                                                                            "_Trench",
                                                                            "M433",
                                                                            "DP_M406",
                                                                            "KLMK",
                                                                            "SovietHarness",
                                                                            "Type56",
                                                                            "_M16",
                                                                            "_G3",
                                                                            "_U[Zz][Ii]",
                                                                            "_AK(_)?(M|74|47)",
                                                                            "39_AK_",
                                                                            "_M14",
                                                                            "_M3A1",
                                                                            "_V[Zz]58",
                                                                            "_6B2[^_]",
                                                                            "_6B3",
                                                                            "_SPP",
                                                                            "M995",
                                                                            "Lifchik",
                                                                            "Launcher_FGM148_Javelin",
                                                                            "ACE_FragBanana",
                                                                            "Radio_RF10",
                                                                            "IGLA",
                                                                            "[Mm][Oo][Dd][Ee][Rr][Nn]",
                                                                            "Kornet",
                                                                            "vz61_straight",
                                                                            "2B14",
                                                                            "6T5",
                                                                            "ZU23",
                                                                            "Khattabka",
                                                                            sep = ")|("),
                                                                      ")")) ~ 
                                            "0",
                                          is.na(m_bEnabled) &
                                            str_detect(string = Item_prefab,
                                                     pattern = paste0("(",
                                                                      paste("Beret",
                                                                            "FlatCap",
                                                                            "Turtleneck",
                                                                            "Denim",
                                                                            "Hood_",
                                                                            sep = ")|("),
                                                                      ")")) ~ 
                                            "1",
                                          str_detect(string = file_name,
                                                     pattern = "US(SR)?") & 
                                            str_detect(string = Item_prefab,
                                                       pattern = "Radio") ~ 
                                            "0",
                                          .default = m_bEnabled))
          })) %>%
  keep(~ length(.x) > 0) ->
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
  dplyr::filter(str_detect(context,
                           negate = T,
                           pattern = paste(collapse = "|",
                                           Data[["Problematic SCR_EntityCatalogMultiListEntry"]]))) %>%
  mutate(Item_GUID = str_match(string = context, 
                               pattern = '[A-Za-z0-9_]+Item "\\{([A-Z0-9]+)\\}\"')[, 2],
         is_base_file = str_detect(string = folder_name,
                                   pattern = "^CIV|FIA|US|USSR$") &
           str_detect(string = file_name,
                      pattern = "InventoryItems_EntityCatalog_(CIV|FIA|US|USSR)\\.conf$")) %>% {
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
  arrange(context) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(base_context = str_extract(string = context,
                                    pattern = '.*[A-Za-z0-9_]+Item "\\{[A-Z0-9]+\\}"'),
         base_order = vanilla_order) %>%
  dplyr::select(group_name,
                Item_GUID,
                base_context,
                base_order) ->
  Data[["InventoryItems_EntityCatalog"]][["Canonical_Base_Context_Map"]]


### Compare raw and modified dataframes using LEFT JOIN to catch modified/added values ####
## Flatten the 'raw' nested list into a single long dataframe -- ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]] %>%
  purrr::map_dfr(.f = ~ dplyr::bind_rows(.x), .id = "folder_name") %>%
  tidyr::pivot_longer(cols = -c(folder_name, file_name, Item_GUID, m_sEntityPrefab, Item_prefab),
                      names_to = "property",
                      values_to = "raw_value") -> 
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw_long"]]


## Flatten the 'modified' nested list into a single long dataframe -- ####
Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]] %>%
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
  left_join(x = .,
            y = Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
              dplyr::filter(! is.na(Item_GUID)) %>%
              dplyr::select(Item_GUID, is_base_file) %>%
              distinct() %>%
              # adding step to make sure item flagged once as is_base_file are kept as such if another mod used the same Item_GUID
              group_by(Item_GUID) %>% 
              summarise(is_base_file = any(is_base_file)),
            by = "Item_GUID") %>%
  arrange(desc(is_base_file)) %>%
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
  dplyr::filter(! is.na(parent_GUID)) %>%
  group_by(parent_GUID) %>%
  mutate(entry_order = min(vanilla_order, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(entry_order,
          vanilla_order) %>%
  
  dplyr::select(! c(folder_name,
                    file_name,
                    is_base_file,
                    m_sEntityPrefab)) %>% 
  distinct() ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]]

# -- 4. Generating Custom _CIV Prefabs & Injecting into EntityCatalog -- ####
## 4.0 Setup: Hex Generator & Output Path ####
file.path(Paths[["Outputs"]]$path,
          "DynamicLootReblance") ->
  Paths[["Outputs"]][["DynamicLootReblance"]]

if(!dir.exists(Paths[["Outputs"]][["DynamicLootReblance"]])) {
  dir.create(path = Paths[["Outputs"]][["DynamicLootReblance"]],
             recursive = TRUE,
             showWarnings = FALSE)
}

## 4.1 Filter and Prepare File Paths & GUIDs ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  # Support both m_sEntityPrefab and m_sEntityID items (e.g. ProjectSonarRadio)
  dplyr::filter(property %in% c("m_sEntityPrefab", "m_sEntityID")) %>%
  distinct(value) %>%
  dplyr::filter(str_detect(string = value,
                           pattern = paste(read_lines(file.path(Paths$Inputs$path,
                                                "DL_LootSystem",
                                                "Should_be_civilian_patterns_list.txt")),
                           collapse = "|"))) %>%
  mutate(
    orig_guid = str_extract(string = value, pattern = "(?<=^\\{)[A-Z0-9]+(?=\\})"),
    orig_path = str_remove(string = value, pattern = "^\\{[A-Z0-9]+\\}"),
    
    new_path = str_replace(string = orig_path, pattern = "\\.et$", replacement = "_CIV.et"),
    new_guid = purrr::map_chr(.x = row_number(), .f = ~ generate_enfusion_guid()),
    inner_id = purrr::map_chr(.x = row_number(), .f = ~ generate_enfusion_guid()),
    
    new_prefab_string = paste0("{", new_guid, "}", new_path),
    orig_prefab_string = paste0("{", orig_guid, "}", orig_path) 
  ) ->
  Data[["DL_LootSytem"]][["Prefabs_to_clone"]]

## 4.2 Generate and Save .et and .et.meta files ####
Data[["DL_LootSytem"]][["Prefabs_to_clone"]] %>%
  purrr::pwalk(.f = function(orig_guid, orig_path, new_path, new_guid, inner_id, ...) {
    
    relative_dir <- dirname(str_remove(string = new_path, pattern = "^/"))
    full_out_dir <- file.path(Paths[["Outputs"]][["DynamicLootReblance"]], relative_dir)
    
    if(!dir.exists(full_out_dir)) {
      dir.create(path = full_out_dir, recursive = TRUE, showWarnings = F)
    }
    
    et_file_path <- file.path(full_out_dir, basename(new_path))
    et_content <- paste0(
      'GenericEntity : "{', orig_guid, '}', orig_path, '" {\n',
      ' ID "', inner_id, '"\n',
      '}\n'
    )
    writeLines(text = et_content, con = et_file_path)
    
    meta_file_path <- paste0(et_file_path, ".meta")
    meta_content <- paste0(
      'MetaFileClass {\n',
      ' Name "{', new_guid, '}', new_path, '"\n',
      ' Configurations {\n',
      '  EntityTemplateResourceClass PC {\n  }\n',
      '  EntityTemplateResourceClass XBOX_ONE : PC {\n  }\n',
      '  EntityTemplateResourceClass XBOX_SERIES : PC {\n  }\n',
      '  EntityTemplateResourceClass PS4 : PC {\n  }\n',
      '  EntityTemplateResourceClass PS5 : PC {\n  }\n',
      '  EntityTemplateResourceClass HEADLESS : PC {\n  }\n',
      '  EntityTemplateResourceClass Xbox : PC {\n  }\n',
      ' }\n',
      '}\n'
    )
    writeLines(text = meta_content, con = meta_file_path)
  })


## 4.3 Inject New Prefabs into the Changed_Properties Dataframe ####
Data[["InventoryItems_EntityCatalog"]][["All_Item_References"]] %>%
  # Support both m_sEntityPrefab and m_sEntityID items (e.g. ProjectSonarRadio)
  dplyr::filter(property %in% c("m_sEntityPrefab", "m_sEntityID")) %>%
  dplyr::inner_join(y = Data[["DL_LootSytem"]][["Prefabs_to_clone"]],
                    by = join_by(value == orig_prefab_string)) %>%
  dplyr::select(group_name,
                Item_GUID,
                property,
                mod_value = new_prefab_string,
                context,
                vanilla_order) %>%
  mutate(parent_GUID = str_extract(string = context,
                                   pattern = '(?<=SCR_EntityCatalogMultiListEntry "\\{)[A-Z0-9]+(?=\\}")')) %>%
  group_by(parent_GUID) %>%
  mutate(entry_order = min(vanilla_order, na.rm = TRUE)) %>%
  ungroup() %>%
  dplyr::select(group_name,
                Item_GUID,
                property,
                mod_value,
                context,
                vanilla_order,
                parent_GUID,
                entry_order) %>%
  distinct() ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Prefab_Overrides"]]

Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
  dplyr::filter(! (Item_GUID %in% Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Prefab_Overrides"]]$Item_GUID & 
                     property %in% c("m_sEntityPrefab", "m_sEntityID"))) %>%
  bind_rows(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Prefab_Overrides"]]) %>%
  arrange(entry_order,
          vanilla_order) ->
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
  
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["Changed_Properties"]] %>%
    dplyr::filter(group_name == grp) ->
    grp_changes
  
  if (nrow(grp_changes) == 0) {
    message(paste("No changes detected for:", grp, "- Skipping file generation."))
    next
  }
  
  Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]] <- new.env(parent = emptyenv())
  
  for (i in 1:nrow(grp_changes)) {
    add_to_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
                path = str_split(grp_changes$context[i], " > ")[[1]], 
                prop = grp_changes$property[i], 
                val = grp_changes$mod_value[i])
  }
  rm(i)
  
  file_out <- file.path(Paths[["Outputs"]][["CIV_DL EntityCatalog"]],
                        paste0("InventoryItems_EntityCatalog_", grp, "_for_CIV_DL.conf"))
  con <- file(file_out, "w")
  
  write_tree(env = Data[["InventoryItems_EntityCatalog"]][["tree_env"]][[grp]], 
             indent_level = "", 
             file_con = con, 
             is_root = TRUE, 
             override = Data[["InventoryItems_EntityCatalog"]][["Vanilla_inherits"]][[grp]])
  
  close(con)
  message(paste("Saved Arma Reforger MINIMAL override for:", grp))
  rm(con,
     file_out,
     grp_changes)
}
rm(grp)
