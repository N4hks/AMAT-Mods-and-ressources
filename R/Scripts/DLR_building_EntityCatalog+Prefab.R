# Loading Input files ####
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
                                         pattern = '(?<=/)[A-Za-z0-9_]+(?=\\.et$)')) %>%
        dplyr::select(where(~ !all(is.na(.x))))) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["raw"]]

## Adjusting values ####
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
                                                         "HELICOPTER",
                                                         "ROCKET_LAUNCHER",
                                                         "LETHAL_THROWABLE") ~ 
                                        "0",
                                      str_detect(string = m_sEntityPrefab,
                                                 pattern = paste("AK74",
                                                                 "UK59",
                                                                 "RPG",
                                                                 "PK",
                                                                 "NSV",
                                                                 "KPVT",
                                                                 "Mortars",
                                                                 "M70",
                                                                 "VOG25",
                                                                 "PersonalBelongings",
                                                                 "Mine",
                                                                 "RearmingKit",
                                                                 sep = "|")) ~ "0",
                                      str_detect(string = file_name,
                                                 pattern = "USSR") & 
                                        str_detect(string = m_sEntityPrefab,
                                                   pattern = "/Radios/") ~ 
                                        "1",
                                      m_eItemType == "WEAPON_ATTACHMENT" &
                                        str_detect(string = m_sEntityPrefab,
                                                   negate = T,
                                                   pattern = paste(sep = "|",
                                                                   "Bayonet_6Kh4",
                                                                   "PSO1",
                                                                   "PBS4")) ~ 
                                        "0"))) ->
  Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]]


## To check the result ####  
View(Data[["InventoryItems_EntityCatalog"]][["Items property df"]][["modified"]] %>%
       list_rbind())
