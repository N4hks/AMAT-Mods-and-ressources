# Install + load packages ####
lapply(X = c("tidyverse", "jsonlite"),
       FUN = \(packagename) {
         if(! require(packagename,
                      character.only = TRUE)) {
           install.packages(pkgs = packagename,
                            ask = FALSE)
         }
         library(packagename,
                 character.only = TRUE)
       })


# Pre-allocate list to receive data ####
list() ->
  Data

list() ->
  Paths


# Setting usefull Paths ####
file.path(".",
          "R",
          "Inputs") ->
Paths[["Inputs"]]$path

# Setting Function(s) ####
## For reading Enfusion Conf files
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
