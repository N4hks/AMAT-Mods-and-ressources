# --------------------------------------- ####
# -- Setting Function(s) --  ####
## -- For Install + load packages -- ####
c( # "jsonlite",
  "fs",
  "jsonlite",
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
# Same output as before (file_name, context, block, property, value) but built
# from pre-allocated vectors and ONE tibble per file instead of one tibble per
# property line (much faster on the ~300 catalog files).
parse_enfusion_conf <- function(file_path) {
  lines <- str_trim(readLines(file_path, warn = FALSE))
  n     <- length(lines)
  
  keep     <- logical(n)
  context  <- character(n)
  block    <- character(n)
  property <- character(n)
  value    <- character(n)
  block_stack <- character()
  
  for (i in seq_len(n)) {
    line <- lines[i]
    
    # Skip empty lines and comments
    if (line == "" || startsWith(line, "//") || startsWith(line, "/*")) next
    
    if (grepl("\\{\\s*$", line)) {
      # Case 1: opening block, e.g. SCR_EntityCatalogEntry "{5D53...}" {
      block_stack <- c(block_stack,
                       str_squish(sub("\\s*\\{\\s*$", "", line)))
      
    } else if (startsWith(line, "}")) {
      # Case 2: closing block
      if (length(block_stack) > 0) block_stack <- block_stack[-length(block_stack)]
      
    } else if (startsWith(line, "m_")) {
      # Case 3: property line, e.g. m_sEntityPrefab "{ABC...}Prefab.et" or m_iID 10
      sp <- regexpr("\\s+", line)
      if (sp > 0) {
        prop_name <- substr(line, 1, sp - 1)
        prop_val  <- substring(line, sp + attr(sp, "match.length"))
      } else {
        prop_name <- line
        prop_val  <- ""
      }
      keep[i]     <- TRUE
      context[i]  <- paste(block_stack, collapse = " > ")
      block[i]    <- if (length(block_stack) > 0) block_stack[length(block_stack)] else "Root"
      property[i] <- prop_name
      value[i]    <- str_trim(str_remove_all(prop_val, '^"|"$'))
    }
  }
  
  tibble(file_name = rep(basename(file_path), sum(keep)),
         context   = context[keep],
         block     = block[keep],
         property  = property[keep],
         value     = value[keep])
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
if (!file.exists("./.Rprofile")) file.create("./.Rprofile")
readLines("./.Rprofile") %>%
  append("source(\"R/Scripts/0-Env_prep.R\")") %>%
  unique() %>%
  writeLines("./.Rprofile")
