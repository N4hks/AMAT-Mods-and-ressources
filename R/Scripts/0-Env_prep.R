# --------------------------------------- ####
# -- Setting Function(s) --  ####
## -- For Install + load packages -- ####
c( # "jsonlite",
  "tidyverse") |>
  lapply(  FUN = \(packagename) {
    if(! require(packagename,
                 character.only = TRUE)) {
      install.packages(pkgs = packagename,
                       ask = FALSE)
    }
    library(packagename,
            character.only = TRUE)
  })

## -- For reading Enfusion Conf files -- ####
parse_enfusion_conf <- function(file_path) {
  # Read lines and strip leading/trailing whitespace
  lines <- readLines(file_path, warn = FALSE) %>% str_trim()
  
  block_stack <- character()
  records <- list()
  
  for (line in lines) {
    # Skip empty lines and comments
    if (line == "" || str_starts(line, "//") || str_starts(line, "/\\*")) next
    
    # Case 1: Opening block (e.g., SCR_EntityCatalogEntry "{5D53...}" { )
    if (str_detect(line, "\\{\\s*$")) {
      header <- str_remove(line, "\\s*\\{\\s*$") %>% str_squish()
      block_stack <- c(block_stack, header)
    } 
    # Case 2: Closing block ( } )
    else if (str_detect(line, "^\\}")) {
      if (length(block_stack) > 0) {
        block_stack <- block_stack[-length(block_stack)]
      }
    } 
    # Case 3: Property line (e.g., m_sEntity "{ABC...}Prefab.et" or m_iID 10)
    else if (str_detect(line, "^m_")) {
      parts <- str_split_fixed(line, "\\s+", 2)
      prop_name <- parts[1, 1]
      prop_val <- parts[1, 2] %>% 
        str_remove_all('^"|"$') %>% # Remove outer quotes
        str_trim()
      
      # Build hierarchical context string
      context_path <- paste(block_stack, collapse = " > ")
      current_block <- ifelse(length(block_stack) > 0, tail(block_stack, 1), "Root")
      
      records[[length(records) + 1]] <- tibble(
        file_name = basename(file_path),
        context = context_path,
        block = current_block,
        property = prop_name,
        value = prop_val
      )
    }
  }
  
  bind_rows(records)
}

## -- Recursive function to build tree environment (updates existing properties to prevent duplicates) -- ####
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

## -- Define recursive function to write out the tree structure -- ####
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
## -- Function to generate 16-character Enfusion GUIDs & IDs -- ####
generate_enfusion_guid <- function() {
  paste0(sample(c(0:9, LETTERS[1:6]), 16, replace = TRUE), collapse = "")
}

# --------------------------------------- ####
# -- Pre-allocate list to receive data -- ####
c("Data",
  "Paths") %>%
  walk(~ if (!exists(.x,
                     envir = .GlobalEnv)) {
    assign(.x, list(),
           envir = .GlobalEnv)
  })

# -- Setting usefull Paths -- ####
file.path(".",
          "R",
          "Inputs") ->
  Paths[["Inputs"]]$path

file.path(".",
          "R",
          "Outputs") ->
  Paths[["Outputs"]]$path


# -- Seting sourcing of the code at project startup -- ####
readLines("./.Rprofile") %>%
  append("source(\"R/Scripts/0-Env_prep.R\")") %>%
  unique() %>%
  writeLines("./.Rprofile")
