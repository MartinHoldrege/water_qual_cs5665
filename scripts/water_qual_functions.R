# Martin Holdrege

# script started 9/7/19


# safely  -----------------------------------------------------------------

# returns NULL when whatNWISsites throws an error. 
safely_whatNWISsites <- safely(.f = whatNWISsites, otherwise = NULL, 
                               quiet = FALSE)

safely_readNWISqw <- safely(.f = readNWISqw, otherwise = NULL, 
                               quiet = FALSE)

# cover/value pairs from strings ------------------------------------------

extract_cover_value_pairs <- function(x) {
  # args:
  #  x--string output from GEE that pairs cover type (two digit) with num of cells
  # returns:
  #   named numeric vector where names are cover class, values are cover names
  
  stopifnot(is.character(x),
            length(x) == 1) # not a vectorized function at the moment. 
  
  # cover/value pairs
  cov_val_pairs <- str_extract_all(x, "\\d+=\\d+\\.\\d+") %>% 
    unlist()
  
  if (length(cov_val_pairs) == 0) {
    return(NULL)
  }

  values <- str_extract(cov_val_pairs, "\\d+\\.\\d+$")
  values <- as.numeric(values)
  
  category <- str_extract(cov_val_pairs, "^\\d+")
  cover_lab <- paste0("cover", category)
  
  out <- values
  names(out) <- cover_lab
  out
}

# testing function

if (FALSE) {
  x <- "{71=19.07843137254902, 21=60.54901960784315, 43=129.45098039215685}"
  extract_cover_value_pairs(x)
}
