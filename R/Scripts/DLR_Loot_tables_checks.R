file.path(Paths$Inputs$path,
          "DL_LootSystem",
          "DL_LootSystem Loot table 2026.08.05-10.01.53.txt") %>%
  read_lines() %>%
  tibble(line = .) %>%
  separate(col = line,
           sep = "(  SCRIPT       : DL_LootSystem: \\[)|(\\]\\()|( \\{)|(\\}\\) Item )|( = )",
           into = c("Empty",
                    "Source_faction",
                    "unidentified",
                    "Loot_categories",
                    "Prefab_path",
                    "Weight")) %>%
  dplyr::select(! Empty) %>%
  mutate(Weight = str_remove(string = Weight,
                             pattern = " weight$") %>%
           as.numeric(),
         Item_mod = str_extract(string = Prefab_path,
                                pattern = "(?<=^\\{).+(?=\\})"),
         Item_type = str_remove(string = Prefab_path,
                      "^\\{.+\\}Prefabs(/(LCJ|Modded))?/") %>%
           str_extract(pattern = "^[A-Za-z]+/[A-Za-z]+(?=/)")) %>%
  separate(col = Item_type,
           sep = "/",
           into = c("Item_type",
                    "Item_subtype")) %>% # Extract the second column,
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
                                                 .default = NA),
                                false = Item_subtype)) %>% {
                                  . -> data
                                  
                                  data %>%
                                    dplyr::filter(Item_type != "LCJ") %>%
                                    bind_rows(data %>% 
                                                dplyr::filter(Item_type == "LCJ") %>%
                                                dplyr::select(! Item_type) %>%
                                                distinct() %>%
                                                left_join(by = join_by(Item_subtype),
                                                          y = data %>%
                                                            dplyr::filter(Item_type != "LCJ") %>%
                                                            dplyr::select(Item_type,
                                                                          Item_subtype) %>%
                                                            distinct()))
                                } %>%
  distinct() %>%
  dplyr::relocate(Item_subtype,
                  .after = everything()) %>%
  pivot_longer(cols = Loot_categories,
               values_transform = ~ .x %>% str_split(pattern = ","),
               values_to = "Loot_category") %>%
  unnest(Loot_category) %>%
  arrange(Item_type,
          Item_subtype) %>%
  mutate(active = T) %>%
  pivot_wider(names_from = Loot_category,
              values_from = active,
              values_fill  = F) ->
  Data[["DL_LootSytem"]][["Loot tables"]][["2026.08.05-10.01.53"]][["full"]]


# View(Data[["DL_LootSytem"]][["Loot tables"]][["2026.08.05-10.01.53"]])

# Data[["DL_LootSytem"]][["Loot tables"]][["2026.08.05-10.01.53"]]   %>% 
#   ggplot()+
#   geom_histogram(aes(x = Weight))+
#   # geom_density(aes(x = Weight))+
#   # geom_boxplot(aes(y = Item_subtype,
#   #              x = Weight))+
#   facet_wrap(facets = vars(Item_type,
#                            Item_subtype))


  



        