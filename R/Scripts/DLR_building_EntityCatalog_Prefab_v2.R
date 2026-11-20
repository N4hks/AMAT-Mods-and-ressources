# =====================================================================
# DLR_building_EntityCatalog_Prefab_v2.R
# ---------------------------------------------------------------------
# Builds the CIV_DL EntityCatalogs (CIV / FIA / USSR overrides) for the
# Dynamic Loot Rebalance mod.
#
#   1. Parse every catalog .conf (base game + mods)      -> refs
#   2. ONE data frame, one row per (catalog, item)        -> items   <== base of everything
#   3. Apply the CIV_DL rules (DLR_item_rules.R)          -> items$m_*_new
#   4. Clone prefabs as "_CIV" with PERSISTENT GUIDs      -> items$prefab_civ
#   5. Write catalogs, prefab clones, faction files       -> Outputs
#   6. List items that are NOT in CIV/FIA/USSR            -> DL_extra_candidates.tsv
#   7. (optional) deploy to the Workbench addon
#
# Requires 0-Env_prep.R (parse_enfusion_conf, add_to_tree, write_tree,
# generate_enfusion_guid, Paths) and DLR_item_rules.R.
# =====================================================================

source(file.path("R", "Scripts", "DLR_item_rules.R"))

# -- 0. Settings -- ####
CORE_GROUPS            <- c("CIV", "FIA", "USSR")  # catalogs that get a CIV_DL override
VANILLA_MOD            <- "ArmaReforger"           # folder under .../Copy/ with the base game catalogs
INCLUDE_OTHER_FACTIONS <- TRUE                     # TRUE: also parse US/UK/... (needed for the candidate list)
                                                   # FALSE: former behaviour (paths with US/UK skipped)
DEPLOY                 <- FALSE                    # TRUE: copy the result into the Workbench addon at the end
EXCLUDE_CONTEXT_PATTERNS <- character()            # regexes on the context (former "Problematic ..." list)

VANILLA_INHERITS <- c(
  CIV  = "{9D7E5804BB2E9B28}Configs/EntityCatalog/CIV/InventoryItems_EntityCatalog_CIV.conf",
  FIA  = "{E908001749419691}Configs/EntityCatalog/FIA/InventoryItems_EntityCatalog_FIA.conf",
  USSR = "{C53421647C3D0D2E}Configs/EntityCatalog/USSR/InventoryItems_EntityCatalog_USSR.conf")

ADDON_DIR       <- file.path("C:", "Users", Sys.info()[["user"]], "Documents", "My Games",
                             "ArmaReforgerWorkbench", "addons", "Dynamic Loot Rebalance")
DL_INPUT_DIR    <- file.path(Paths$Inputs$path, "DL_LootSystem")
MOD_FILES_DIR   <- file.path(Paths$Inputs$path, "DL_Mod_files")   # faction files, same layout as the addon
OUT_DIR         <- file.path(Paths$Outputs$path, "DynamicLootReblance")
CATALOG_OUT_DIR <- file.path(OUT_DIR, "Configs", "EntityCatalog", "CIV_DL")
REGISTRY_PATH   <- file.path(DL_INPUT_DIR, "DL_GUID_registry.csv")

ITEM_PATTERN         <- '[A-Za-z0-9_]+Item "\\{([A-Z0-9]+)\\}"'
ITEM_CONTEXT_PATTERN <- '^.*?[A-Za-z0-9_]+Item "\\{[A-Z0-9]+\\}"'
PARENT_PATTERN       <- '(?<=SCR_EntityCatalogMultiListEntry "\\{)[A-Z0-9]+(?=\\}")'

# -- Helpers -- ####
first_non_na <- function(x) x[!is.na(x)][1]

copy_tree <- function(from, to) {
  rel <- list.files(from, recursive = TRUE)
  walk(rel, \(r) {
    dir.create(dirname(file.path(to, r)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(from, r), file.path(to, r), overwrite = TRUE)
  })
  invisible(length(rel))
}

## GUID registry: kind / key / guid / id2 -- persisted in REGISTRY_PATH ####
#  kind "clone"   : key = original prefab path, guid = GUID of the _CIV prefab, id2 = inner entity ID
#  kind "catalog" : key = group (CIV/FIA/USSR), guid = GUID of the catalog .conf, id2 = object ID in CIV_DL.conf
new_guids <- function(n, taken = character()) {
  out <- character()
  while (length(out) < n) {
    g <- generate_enfusion_guid()
    if (!g %in% c(taken, out)) out <- c(out, g)
  }
  out
}

ensure_registry <- function(registry, kind, keys) {
  missing <- setdiff(unique(keys), registry$key[registry$kind == kind])
  if (length(missing) == 0) return(registry)
  ids <- new_guids(2 * length(missing), taken = c(registry$guid, registry$id2))
  bind_rows(registry,
            tibble(kind = kind,
                   key  = missing,
                   guid = ids[seq_along(missing)],
                   id2  = ids[length(missing) + seq_along(missing)]))
}

# First run only: recover the GUIDs already deployed in the Workbench addon,
# so that nothing already referenced changes.
bootstrap_registry <- function(addon_dir) {
  empty <- tibble(kind = character(), key = character(), guid = character(), id2 = character())
  read_all <- \(f) paste(read_lines(f), collapse = "\n")
  
  # (a) _CIV prefab clones: GUID from the .meta, inner ID from the .et
  metas <- list.files(file.path(addon_dir, "Prefabs"),
                      pattern = "_CIV\\.et\\.meta$", recursive = TRUE, full.names = TRUE)
  clones <- if (length(metas) == 0) empty else {
    tibble(meta = metas) %>%
      mutate(txt_meta   = map_chr(meta, read_all),
             txt_et     = map_chr(str_remove(meta, "\\.meta$"),
                                  \(f) if (file.exists(f)) read_all(f) else NA_character_),
             guid       = str_match(txt_meta, 'Name "\\{([0-9A-F]+)\\}')[, 2],
             clone_path = str_match(txt_meta, 'Name "\\{[0-9A-F]+\\}([^"]+)"')[, 2],
             id2        = str_match(txt_et, '(?m)^\\s*ID "([0-9A-F]+)"')[, 2]) %>%
      dplyr::filter(!is.na(guid), !is.na(clone_path), !is.na(id2)) %>%
      transmute(kind = "clone",
                key  = str_replace(clone_path, "_CIV\\.et$", ".et"),
                guid, id2)
  }
  
  # (b) catalog .conf GUIDs referenced by the faction conf
  faction_conf <- file.path(addon_dir, "Configs", "Factions", "CIV_DL.conf")
  catalogs <- empty
  if (file.exists(faction_conf)) {
    m <- str_match_all(read_all(faction_conf),
                       paste0('SCR_EntityCatalogMultiList "\\{([0-9A-F]+)\\}" : "\\{([0-9A-F]+)\\}',
                              'Configs/EntityCatalog/CIV_DL/InventoryItems_EntityCatalog_',
                              '([A-Za-z0-9]+)_for_CIV_DL\\.conf"'))[[1]]
    if (nrow(m) > 0) catalogs <- tibble(kind = "catalog", key = m[, 4], guid = m[, 3], id2 = m[, 2])
  }
  
  if (nrow(clones) == 0) warning("Bootstrap: no deployed _CIV prefab found in ", addon_dir,
                                 " -> ALL clone GUIDs will be new.")
  bind_rows(clones, catalogs)
}

## Writers ####
write_catalog_override <- function(changes, group, out_dir) {
  if (nrow(changes) == 0) {
    message("No changes for ", group, " - skipped.")
    return(invisible(NULL))
  }
  tree <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(changes))) {
    add_to_tree(env  = tree,
                path = str_split(changes$context[i], " > ")[[1]],
                prop = changes$property[i],
                val  = changes$new[i])
  }
  file_out <- file.path(out_dir, paste0("InventoryItems_EntityCatalog_", group, "_for_CIV_DL.conf"))
  con <- file(file_out, "w")
  on.exit(close(con))
  write_tree(env = tree, indent_level = "", file_con = con,
             is_root = TRUE, override = VANILLA_INHERITS[[group]])
  message("Saved override: ", basename(file_out), " (", nrow(changes), " changes)")
  invisible(file_out)
}

write_conf_meta <- function(group, guid, out_dir) {
  file_conf <- paste0("InventoryItems_EntityCatalog_", group, "_for_CIV_DL.conf")
  configs   <- c("PC", paste0(c("XBOX_ONE", "XBOX_SERIES", "PS4", "PS5", "HEADLESS"), " : PC"))
  writeLines(c("MetaFileClass {",
               paste0(' Name "{', guid, '}Configs/EntityCatalog/CIV_DL/', file_conf, '"'),
               " Configurations {",
               unlist(lapply(configs, \(cfg) c(paste0("  CONFResourceClass ", cfg, " {"), "  }"))),
               " }",
               "}"),
             file.path(out_dir, paste0(file_conf, ".meta")))
}

# Regression helper: compares two override files on (Item_GUID, property, value)
# usage: compare_override_files("old/..._CIV_for_CIV_DL.conf", "new/..._CIV_for_CIV_DL.conf")
compare_override_files <- function(old_file, new_file) {
  tidy <- \(f) parse_enfusion_conf(f) %>%
    mutate(Item_GUID = str_match(context, ITEM_PATTERN)[, 2]) %>%
    dplyr::filter(!is.na(Item_GUID)) %>%
    dplyr::select(Item_GUID, property, value)
  old <- tidy(old_file)
  new <- tidy(new_file)
  list(only_in_old = anti_join(old, new, by = c("Item_GUID", "property", "value")),
       only_in_new = anti_join(new, old, by = c("Item_GUID", "property", "value")))
}


# -- 1. Load and parse the catalog files -- ####
catalog_files <- file.path("addons", "CopyEntityCatalogPlugin", "Configs", "EntityCatalog") %>%
  list.files(full.names = TRUE,
             recursive  = TRUE,
             pattern    = paste0("(",
                                 paste("Inventory", "Ammunition", "Attachment", "Backpack", "Clothing",
                                       "Deployable", "Equipment", "Explosive", "Shops", "Vest", "Weapons",
                                       sep = "|"),
                                 ").*\\.conf$")) %>%
  tibble(file_path = .)

if (!INCLUDE_OTHER_FACTIONS) {
  catalog_files <- catalog_files %>% dplyr::filter(!str_detect(file_path, "U[SK](?!SR)"))
}

catalog_files <- catalog_files %>%
  mutate(source_mod     = str_extract(file_path, "(?<=/Copy/)[^/]+"),
         faction_folder = basename(dirname(file_path)),
         file_name      = basename(file_path),
         # catalog a file belongs to = faction token found in its file name (as before)
         group_name     = str_extract(file_name, "(?<=^|_)(USSR|US|UK|FIA|CIV)(?=_|\\.conf$)"),
         catalog_group  = coalesce(group_name, paste(source_mod, faction_folder, sep = "/")))

refs <- catalog_files %>%
  mutate(parsed = map(file_path,
                      \(f) parse_enfusion_conf(f) %>% dplyr::select(any_of(c("context", "property", "value"))))) %>%
  unnest(parsed) %>%
  mutate(line_no = row_number(), .by = file_path)

if (length(EXCLUDE_CONTEXT_PATTERNS) > 0) {
  refs <- refs %>% dplyr::filter(!str_detect(context, paste(EXCLUDE_CONTEXT_PATTERNS, collapse = "|")))
}


# -- 2. THE items data frame: one row per (catalog_group, Item_GUID) -- ####
## 2.1 One row per item *per file* (values + where it sits in the .conf tree) ####
item_rows <- refs %>%
  mutate(Item_GUID = str_match(context, ITEM_PATTERN)[, 2]) %>%
  dplyr::filter(!is.na(Item_GUID))

item_vals <- item_rows %>%
  summarise(value = first(value), .by = c(file_path, Item_GUID, property)) %>%
  pivot_wider(names_from = property, values_from = value)

for (col in c("m_sEntityPrefab", "m_sEntityID", "m_eItemType", "m_iSupplyCost", "m_bEnabled")) {
  if (!col %in% colnames(item_vals)) item_vals[[col]] <- NA_character_
}

item_ctx <- item_rows %>%
  summarise(ctx_item    = first(str_extract(context, ITEM_CONTEXT_PATTERN)),
            ctx_arsenal = first_non_na(if_else(property %in% c("m_iSupplyCost", "m_eItemType"),
                                               context, NA_character_)),
            first_line  = min(line_no),
            .by = c(file_path, Item_GUID))

items_file <- item_vals %>%
  left_join(item_ctx,      by = c("file_path", "Item_GUID")) %>%
  left_join(catalog_files, by = "file_path") %>%
  mutate(is_base = source_mod == VANILLA_MOD) %>%
  arrange(catalog_group, desc(is_base), file_name, first_line) %>%
  mutate(src_order = row_number())

## 2.2 Resolve duplicates: base game first, then mods; first non-NA value wins ####
agg_cols <- setdiff(colnames(items_file),
                    c("source_mod", "file_path", "is_base", "src_order", "first_line",
                      "catalog_group", "Item_GUID"))

items <- items_file %>%
  summarise(source_mods  = paste(unique(source_mod), collapse = " + "),
            source_files = paste(unique(file_name),  collapse = " + "),
            n_defs       = n(),
            is_vanilla   = any(is_base),
            item_order   = min(src_order),
            across(all_of(agg_cols), first_non_na),
            .by = c(catalog_group, Item_GUID)) %>%
  mutate(prefab_prop = if_else(!is.na(m_sEntityPrefab), "m_sEntityPrefab", "m_sEntityID"),
         prefab      = coalesce(m_sEntityPrefab, m_sEntityID),
         prefab_guid = str_match(prefab, "^\\{([A-Z0-9]+)\\}")[, 2],
         prefab_path = str_remove(prefab, "^\\{[A-Z0-9]+\\}"),
         Item_prefab = str_extract(prefab_path, "(?<=/)[A-Za-z0-9_\\-]+(?=\\.et$)"),
         parent_GUID = str_extract(ctx_item, PARENT_PATTERN),
         in_core     = catalog_group %in% CORE_GROUPS) %>%
  dplyr::select(-m_sEntityPrefab, -m_sEntityID) %>%
  mutate(entry_order = min(item_order), .by = c(catalog_group, parent_GUID)) %>%
  arrange(catalog_group, entry_order, item_order)

stopifnot(nrow(items) > 0)
if (!any(items$is_vanilla & items$in_core)) {
  warning("No vanilla item found in CIV/FIA/USSR: check VANILLA_MOD (currently '", VANILLA_MOD, "').")
}
message("items: ", nrow(items), " rows; core catalogs: ",
        paste(items %>% dplyr::filter(in_core) %>% count(catalog_group) %>%
                mutate(txt = paste0(catalog_group, "=", n)) %>% pull(txt), collapse = ", "))


# -- 3. Apply the CIV_DL rules (m_iSupplyCost, m_bEnabled) -- ####
rules_applied <- apply_civ_dl_rules(items)

items <- items %>%
  mutate(m_iSupplyCost_new = rules_applied$m_iSupplyCost,
         m_bEnabled_new    = rules_applied$m_bEnabled)


# -- 4. _CIV prefab clones with persistent GUIDs -- ####
civilian_patterns <- read_lines(file.path(DL_INPUT_DIR, "Should_be_civilian_patterns_list.txt")) %>%
  str_trim() %>%
  str_subset("\\S") %>%
  unique()

items <- items %>%
  mutate(needs_clone = in_core & !is.na(prefab) &
           str_detect(prefab, paste(civilian_patterns, collapse = "|")))

registry <- if (file.exists(REGISTRY_PATH)) {
  read_csv(REGISTRY_PATH, col_types = cols(.default = col_character()))
} else {
  message("No GUID registry yet: bootstrapping it from the Workbench addon.")
  bootstrap_registry(ADDON_DIR)
}

clones <- items %>%
  dplyr::filter(needs_clone) %>%
  distinct(prefab, prefab_guid, prefab_path) %>%
  mutate(clone_path = str_replace(prefab_path, "\\.et$", "_CIV.et"))

registry <- registry %>%
  ensure_registry("clone",   clones$prefab_path) %>%
  ensure_registry("catalog", CORE_GROUPS)

dir.create(dirname(REGISTRY_PATH), recursive = TRUE, showWarnings = FALSE)
write_csv(registry, REGISTRY_PATH)   # saved BEFORE any output so GUIDs survive an interrupted run

clones <- clones %>%
  left_join(registry %>% dplyr::filter(kind == "clone") %>%
              dplyr::select(prefab_path = key, clone_guid = guid, clone_id = id2),
            by = "prefab_path") %>%
  mutate(prefab_civ = paste0("{", clone_guid, "}", clone_path))

items <- items %>% left_join(clones %>% dplyr::select(prefab, prefab_civ), by = "prefab")


# -- 5. Outputs -- ####
## 5.1 _CIV prefabs (.et + .et.meta) ####
prefab_dir <- file.path(OUT_DIR, "Prefabs")
list.files(prefab_dir, recursive = TRUE, full.names = TRUE) %>% file.remove()

clones %>%
  pwalk(function(prefab_guid, prefab_path, clone_path, clone_guid, clone_id, ...) {
    out_dir <- file.path(OUT_DIR, dirname(clone_path))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    et_file <- file.path(out_dir, basename(clone_path))
    
    writeLines(paste0('GenericEntity : "{', prefab_guid, '}', prefab_path, '" {\n',
                      ' ID "', clone_id, '"\n',
                      '}\n'),
               et_file)
    
    writeLines(paste0('MetaFileClass {\n',
                      ' Name "{', clone_guid, '}', clone_path, '"\n',
                      ' Configurations {\n',
                      '  EntityTemplateResourceClass PC {\n  }\n',
                      '  EntityTemplateResourceClass XBOX_ONE : PC {\n  }\n',
                      '  EntityTemplateResourceClass XBOX_SERIES : PC {\n  }\n',
                      '  EntityTemplateResourceClass PS4 : PC {\n  }\n',
                      '  EntityTemplateResourceClass PS5 : PC {\n  }\n',
                      '  EntityTemplateResourceClass HEADLESS : PC {\n  }\n',
                      '  EntityTemplateResourceClass Xbox : PC {\n  }\n',
                      ' }\n',
                      '}\n'),
               paste0(et_file, ".meta"))
  })

## 5.2 Catalog overrides: what changed, derived straight from `items` ####
changes <- bind_rows(
  items %>% dplyr::filter(in_core, !is.na(prefab), !is.na(parent_GUID)) %>%
    transmute(catalog_group, Item_GUID, entry_order, item_order,
              context = ctx_item, property = prefab_prop,
              old = prefab, new = prefab_civ, ctx_fallback = FALSE),
  items %>% dplyr::filter(in_core, !is.na(prefab), !is.na(parent_GUID)) %>%
    transmute(catalog_group, Item_GUID, entry_order, item_order,
              context = coalesce(ctx_arsenal, ctx_item), property = "m_iSupplyCost",
              old = m_iSupplyCost, new = m_iSupplyCost_new, ctx_fallback = is.na(ctx_arsenal)),
  items %>% dplyr::filter(in_core, !is.na(prefab), !is.na(parent_GUID)) %>%
    transmute(catalog_group, Item_GUID, entry_order, item_order,
              context = ctx_item, property = "m_bEnabled",
              old = m_bEnabled, new = m_bEnabled_new, ctx_fallback = FALSE)) %>%
  dplyr::filter(!is.na(new), new != "NA", is.na(old) | old != new) %>%
  arrange(catalog_group, entry_order, item_order)

if (any(changes$ctx_fallback)) {
  warning(sum(changes$ctx_fallback), " m_iSupplyCost value(s) had no SCR_ArsenalItem block: ",
          "written at item level. Check `changes |> dplyr::filter(ctx_fallback)`.")
}

dir.create(CATALOG_OUT_DIR, recursive = TRUE, showWarnings = FALSE)
list.files(CATALOG_OUT_DIR, full.names = TRUE) %>% file.remove()

for (grp in CORE_GROUPS) {
  write_catalog_override(changes %>% dplyr::filter(catalog_group == grp), grp, CATALOG_OUT_DIR)
  write_conf_meta(grp, registry$guid[registry$kind == "catalog" & registry$key == grp], CATALOG_OUT_DIR)
}

# ## 5.3 Faction files (FactionManager, CIV_DL.conf + metas) ####
# # Put the files you use in the mod in MOD_FILES_DIR (same sub-folders as in the addon):
# #   Prefabs/MP/Managers/Factions/FactionManager_Base.et (+ .meta)
# #   Configs/Factions/CIV_DL.conf (+ .meta)
# if (dir.exists(MOD_FILES_DIR)) {
#   copy_tree(MOD_FILES_DIR, OUT_DIR)
#   
#   faction_conf <- file.path(OUT_DIR, "Configs", "Factions", "CIV_DL.conf")
#   if (file.exists(faction_conf)) {
#     # Only m_aEntityCatalogs is regenerated (from the registry); the rest of the file is kept as is
#     catalog_entries <- registry %>%
#       dplyr::filter(kind == "catalog", key %in% CORE_GROUPS) %>%
#       arrange(match(key, CORE_GROUPS)) %>%
#       mutate(txt = sprintf(paste0('  SCR_EntityCatalogMultiList "{%s}" : "{%s}Configs/EntityCatalog/CIV_DL/',
#                                   'InventoryItems_EntityCatalog_%s_for_CIV_DL.conf" {\n  }\n'),
#                            id2, guid, key)) %>%
#       pull(txt)
#     faction_parts <- str_split_fixed(paste(read_lines(faction_conf), collapse = "\n"),
#                                      "\\n[ \\t]*m_aEntityCatalogs[ \\t]*\\{", 2)
#     write_file(paste0(faction_parts[1, 1], "\n m_aEntityCatalogs {\n",
#                       paste(catalog_entries, collapse = ""), " }\n}\n"),
#                faction_conf)
#   }
# } else {
#   warning("MOD_FILES_DIR not found (", MOD_FILES_DIR, "): faction files not exported.")
# }


# -- 6. Candidates for the future "extra" catalog -- ####
# Items found in other catalogs (US, UK, other mods...) but absent from CIV/FIA/USSR.
# Mark the ones you want with an "x" in the `include` column.
core_prefab_paths <- items %>% dplyr::filter(in_core) %>% distinct(prefab_path)

candidates <- items %>%
  dplyr::filter(!in_core, !is.na(prefab_path)) %>%
  anti_join(core_prefab_paths, by = "prefab_path") %>%
  summarise(Item_prefab   = first(Item_prefab),
            m_eItemType   = first_non_na(m_eItemType),
            m_iSupplyCost = first_non_na(m_iSupplyCost),
            catalogs      = paste(sort(unique(catalog_group)), collapse = " ; "),
            source_mods   = paste(sort(unique(source_mods)),  collapse = " ; "),
            .by = prefab_path) %>%
  mutate(include = "") %>%
  arrange(m_eItemType, Item_prefab)

write_tsv(candidates, file.path(Paths$Outputs$path, "DL_extra_candidates.tsv"))
message("Candidates for the extra catalog: ", nrow(candidates), " -> DL_extra_candidates.tsv")


# -- 7. Keep the objects for interactive exploration -- ####
Data[["InventoryItems_EntityCatalog"]] <- list(items = items, changes = changes,
                                               clones = clones, candidates = candidates)


# -- 8. Deploy to the Workbench addon (DEPLOY <- TRUE) -- ####
if (DEPLOY) {
  list.files(file.path(ADDON_DIR, "Prefabs"),
             pattern = "_CIV\\.et(\\.meta)?$", recursive = TRUE, full.names = TRUE) %>%
    file.remove()
  walk(c("Prefabs", "Configs"),
       \(d) copy_tree(file.path(OUT_DIR, d), file.path(ADDON_DIR, d)))
  message("Deployed to ", ADDON_DIR)
}
