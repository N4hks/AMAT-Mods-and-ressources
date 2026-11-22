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
#   5. Write catalogs + prefab clones; copy the files you edit in the Workbench -> Outputs
#   6. List items that are NOT in CIV/FIA/USSR            -> DL_extra_candidates.tsv
#   7. (optional) deploy the WHOLE mod, Outputs -> Workbench addon
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
OUT_DIR         <- file.path(Paths$Outputs$path, "DynamicLootReblance")
CATALOG_OUT_DIR <- file.path(OUT_DIR, "Configs", "EntityCatalog", "CIV_DL")
REGISTRY_PATH   <- file.path(DL_INPUT_DIR, "DL_GUID_registry.csv")

# Files you edit in the Enfusion Workbench. The Workbench stays their single source of truth: they are
# copied FROM the addon INTO the Outputs at every run (their .meta too), so that the whole mod can then
# be deployed from the Outputs. Paths are relative to the addon folder.
WORKBENCH_FILES <- c("Configs/Systems/DL_LootSystem.conf",
                     "Configs/Factions/CIV_DL.conf",
                     "Prefabs/MP/Managers/Factions/FactionManager_Base.et")

# Items to be found in URBAN/RURAL areas (see section 4b)
URBAN_RURAL_PATTERNS_FILE <- file.path(DL_INPUT_DIR, "Should_be_urban_rural_patterns.txt")
URBAN_RURAL_ITEM_TYPE     <- "TORSO"   # must be in commonItemTypes of DL_LootSystem.conf, and no natural
# item type of the other (POLICE/MEDICAL/...) items may be there

ITEM_PATTERN         <- '[A-Za-z0-9_]+Item "\\{([A-Z0-9]+)\\}"'
ITEM_CONTEXT_PATTERN <- '^.*?[A-Za-z0-9_]+Item "\\{[A-Z0-9]+\\}"'
PARENT_PATTERN       <- '(?<=SCR_EntityCatalogMultiListEntry "\\{)[A-Z0-9]+(?=\\}")'

# -- Helpers -- ####
first_non_na <- function(x) x[!is.na(x)][1]

# Copies the files `rel` (paths relative to `from`) into `to`, only when they differ
# (so the Workbench / Git are not bothered by untouched files). Returns the number of files written.
copy_files <- function(rel, from, to) {
  src  <- file.path(from, rel)
  dst  <- file.path(to, rel)
  md5  <- \(f) unname(tools::md5sum(f))
  same <- file.exists(dst) & md5(src) == md5(dst)
  same[is.na(same)] <- FALSE
  for (i in which(!same)) {
    dir.create(dirname(dst[i]), recursive = TRUE, showWarnings = FALSE)
    file.copy(src[i], dst[i], overwrite = TRUE)
  }
  invisible(sum(!same))
}

copy_tree <- function(from, to) copy_files(list.files(from, recursive = TRUE), from, to)

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

# A clone only matters when another faction can carry the same prefab: Dynamic Loot keeps the FIRST
# faction that has a given ResourceName. Prefabs found only in CIV catalogs (the CIV and CIV_USSR
# factions are blacklisted) have nothing to collide with, so they keep their original prefab.
carried_elsewhere <- items %>%
  dplyr::filter(catalog_group != "CIV", !is.na(prefab_path)) %>%
  distinct(prefab_path)

matches_civilian <- items$in_core & !is.na(items$prefab) &
  str_detect(items$prefab, paste(civilian_patterns, collapse = "|"))

items <- items %>%
  mutate(needs_clone = matches_civilian & prefab_path %in% carried_elsewhere$prefab_path)

message("Clones: ", n_distinct(items$prefab_path[items$needs_clone]), " prefabs; skipped because only in CIV catalogs: ",
        n_distinct(items$prefab_path[matches_civilian & !items$needs_clone]))

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


# -- 4b. Item type: put the whitelisted items in URBAN/RURAL -- ####
# Dynamic Loot tags an entry from its m_eItemType (and its faction) ONLY, never from the prefab.
#  - items matching Should_be_urban_rural_patterns.txt get URBAN_RURAL_ITEM_TYPE, a type listed in
#    commonItemTypes => tags URBAN + RURAL (+ whatever other list that type is in)
#  - every other item keeps its own type: the police / medical / industrial / commercial lists of
#    DL_LootSystem.conf decide its tags (so its type must NOT be in commonItemTypes)
# Patterns are matched against the prefab as it appears in the CIV_DL catalog, i.e. the
# "{GUID}..._CIV.et" clone when there is one, the original prefab otherwise.
# One regex per line; empty lines and lines starting with # are ignored.
# Done AFTER the rules of section 3, which rely on the natural item type.
urban_rural_patterns <- if (file.exists(URBAN_RURAL_PATTERNS_FILE)) {
  read_lines(URBAN_RURAL_PATTERNS_FILE) %>%
    str_trim() %>%
    str_subset("\\S") %>%
    str_subset("^#", negate = TRUE) %>%
    unique()
} else {
  warning("Missing ", URBAN_RURAL_PATTERNS_FILE, ": no item is moved to ", URBAN_RURAL_ITEM_TYPE, ".")
  character()
}

items <- items %>%
  mutate(prefab_final    = coalesce(prefab_civ, prefab),
         enabled_final   = coalesce(m_bEnabled_new, m_bEnabled, "1") != "0",
         in_urban_rural  = in_core & enabled_final & !is.na(prefab_final) &
           (if (length(urban_rural_patterns) > 0) {
             coalesce(str_detect(prefab_final, paste(urban_rural_patterns, collapse = "|")), FALSE)
           } else FALSE),
         m_eItemType_new = if_else(in_urban_rural & (is.na(m_eItemType) | m_eItemType != URBAN_RURAL_ITEM_TYPE),
                                   URBAN_RURAL_ITEM_TYPE, NA_character_))

unused_patterns <- urban_rural_patterns[!map_lgl(urban_rural_patterns, \(p) {
  any(str_detect(items$prefab_final[items$in_core & items$enabled_final], p), na.rm = TRUE)
})]
if (length(unused_patterns) > 0) {
  warning("URBAN/RURAL patterns matching no enabled item: ", paste(unused_patterns, collapse = " ; "))
}

message("URBAN/RURAL whitelist: ", sum(items$in_urban_rural), " entries -> ", URBAN_RURAL_ITEM_TYPE,
        " (", sum(!is.na(items$m_eItemType_new)), " type changes)")
message("Enabled entries keeping their own type (tags come from the police/medical/industrial/commercial lists):")
print(items %>% dplyr::filter(in_core, enabled_final, !in_urban_rural) %>% count(m_eItemType, sort = TRUE))


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
no_type_context <- items %>% dplyr::filter(in_core, !is.na(m_eItemType_new), is.na(ctx_arsenal))
if (nrow(no_type_context) > 0) {
  warning(nrow(no_type_context), " item(s) have no SCR_ArsenalItem block: m_eItemType cannot be written for ",
          paste(head(no_type_context$Item_prefab, 10), collapse = ", "), " ...")
}

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
              old = m_bEnabled, new = m_bEnabled_new, ctx_fallback = FALSE),
  items %>% dplyr::filter(in_core, !is.na(prefab), !is.na(parent_GUID), !is.na(ctx_arsenal)) %>%
    transmute(catalog_group, Item_GUID, entry_order, item_order,
              context = ctx_arsenal, property = "m_eItemType",
              old = m_eItemType, new = m_eItemType_new, ctx_fallback = FALSE)) %>%
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

## 5.3 Files edited in the Workbench: addon -> Outputs ####
# DL_LootSystem.conf, CIV_DL.conf and FactionManager_Base.et are NOT generated here: they are copied
# as they currently are in the addon, so the Outputs always hold the complete mod (section 8 deploys it).
workbench_files <- c(WORKBENCH_FILES, paste0(WORKBENCH_FILES, ".meta"))
workbench_found <- file.exists(file.path(ADDON_DIR, workbench_files))
if (!all(workbench_found)) {
  warning("Not found in the Workbench addon (not copied): ",
          paste(workbench_files[!workbench_found], collapse = ", "))
}
n_wb <- copy_files(workbench_files[workbench_found], from = ADDON_DIR, to = OUT_DIR)
message("Workbench files copied to the Outputs: ", n_wb, " updated, ",
        sum(workbench_found) - n_wb, " unchanged")

# The faction must reference the catalogs written above (GUIDs come from the registry)
faction_conf <- file.path(OUT_DIR, "Configs", "Factions", "CIV_DL.conf")
if (file.exists(faction_conf)) {
  faction_txt  <- paste(read_lines(faction_conf), collapse = "\n")
  catalog_refs <- registry %>%
    dplyr::filter(kind == "catalog", key %in% CORE_GROUPS) %>%
    mutate(ref = paste0("{", guid, "}Configs/EntityCatalog/CIV_DL/InventoryItems_EntityCatalog_",
                        key, "_for_CIV_DL.conf"))
  unreferenced <- catalog_refs$key[!str_detect(faction_txt, fixed(catalog_refs$ref))]
  if (length(unreferenced) > 0) {
    warning("CIV_DL.conf does not reference the generated catalog(s): ", paste(unreferenced, collapse = ", "),
            " -> check m_aEntityCatalogs in the Workbench.")
  }
}


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


# -- 8. Deploy the WHOLE mod: Outputs -> Workbench addon (DEPLOY <- TRUE) -- ####
# Everything under OUT_DIR (catalogs, _CIV prefabs, and the files copied from the Workbench in 5.3).
# Only files that differ are written; _CIV prefabs that are no longer generated are removed.
if (DEPLOY) {
  clone_pattern <- "_CIV\\.et(\\.meta)?$"
  out_clones    <- list.files(file.path(OUT_DIR,   "Prefabs"), pattern = clone_pattern, recursive = TRUE)
  addon_clones  <- list.files(file.path(ADDON_DIR, "Prefabs"), pattern = clone_pattern, recursive = TRUE)
  stale         <- setdiff(addon_clones, out_clones)
  file.remove(file.path(ADDON_DIR, "Prefabs", stale))
  
  n_written <- copy_tree(OUT_DIR, ADDON_DIR)
  message("Deployed to ", ADDON_DIR, ": ", n_written, " file(s) written, ", length(stale), " stale _CIV file(s) removed.")
}