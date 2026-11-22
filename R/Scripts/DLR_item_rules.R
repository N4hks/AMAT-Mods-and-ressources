# =====================================================================
# DLR_item_rules.R
# ---------------------------------------------------------------------
# Rules that turn the "military" values of an item into its CIV_DL values
# (m_iSupplyCost and m_bEnabled).
#
# The two case_when() blocks below are copied VERBATIM from the former
# DLR_building_EntityCatalog_Prefab.R (lines 144-274), so that results stay
# identical during the refactor. Only the wrapper function is new.
#
# Input : the `items` data frame (needs columns file_name, Item_prefab,
#         m_eItemType, m_iSupplyCost, m_bEnabled)
# Output: the same data frame with m_iSupplyCost / m_bEnabled rewritten.
# =====================================================================

apply_civ_dl_rules <- function(item_property_df) {

  # Safety check: initialise the columns as NA if no catalog defines them
  for (col in c("m_bEnabled", "m_iSupplyCost", "m_eItemType")) {
    if (!col %in% colnames(item_property_df)) {
      item_property_df[[col]] <- NA_character_
    }
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
                   m_bEnabled = case_when(m_eItemType %in% c("MORTARS",
                                                             "HELICOPTER") ~ 
                                            "0",
                                          str_detect(string = Item_prefab,
                                                     pattern = paste("Raincoat",
                                                                     "Fisherman",
                                                                     "Kolobok",
                                                                     sep = "|")) ~ 
                                            "1",
                                          m_eItemType == "WEAPON_ATTACHMENT" &
                                            str_detect(string = Item_prefab,
                                                       negate = T,
                                                       pattern = paste0("(",
                                                                        paste("Bayonet(?!_M7)",
                                                                              "_PU",
                                                                              paste0("Suppressor_(",
                                                                                     paste(sep = "|",
                                                                                           "APB",
                                                                                           "Bramit",
                                                                                           "AUG",
                                                                                           "VZ61",
                                                                                           "T4AUG"),
                                                                                     ")"),
                                                                              paste0("Optic_(",
                                                                                     paste(sep = "|",
                                                                                           "PSO1",
                                                                                           "1P29",
                                                                                           "PU",
                                                                                           "VZ61",
                                                                                           "T4AUG",
                                                                                           "P1N51",
                                                                                           "P1N58"),
                                                                                     ")"),
                                                                              sep = ")|("),
                                                                        ")")) ~ 
                                            "0",
                                          str_detect(string = Item_prefab,
                                                     pattern = paste0("(",
                                                                      paste("RP[GDK]",
                                                                            "M70",
                                                                            # paste0("PaperMap_01_folded_",
                                                                            #        c("FIA",
                                                                            #          "USSR"),
                                                                            #        collapse = "|"),
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
                                                                            "(?<!AKS74U)_Bandit",
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
                                                                            "vz61_(straight|32rnd)",
                                                                            "2B14",
                                                                            "6T5",
                                                                            "ZU23",
                                                                            "Khattabka",
                                                                            "TTsKO",
                                                                            # "ARY_Launcher_M79_Wood_M406",
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
                                                     pattern = "US(?!=SR)") & 
                                            str_detect(string = Item_prefab,
                                                       pattern = "Radio") ~ 
                                            "0",
                                          .default = m_bEnabled))
}
