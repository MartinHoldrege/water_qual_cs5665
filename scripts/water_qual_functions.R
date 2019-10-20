# Martin Holdrege

# script started 9/7/19


# safely  -----------------------------------------------------------------

# returns NULL when whatNWISsites throws an error. 
safely_whatNWISsites <- safely(.f = whatNWISsites, otherwise = NULL, 
                               quiet = FALSE)

safely_readNWISqw <- safely(.f = readNWISqw, otherwise = NULL, 
                               quiet = FALSE)
