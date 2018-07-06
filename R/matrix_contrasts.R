#' Matrix of contrast weights
#'
#' This function assembles multiple vectors of contrasts weights into a matrix. All pairwise contrasts between habitats on each island are computed separately. Contrasts between habitats across islands are not considered.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @return A matrix of contrast weights. Each row is a contrast. Each column is a habitat on a given island. Each row is filled with zeros, except the two habitats to compare, which are given values 1 and -1.
#' @author Raphael Scherrer
#' @export

# Function to create a matrix of contrast weights between habitats on each island
matrix_contrasts <- function(specdata) {
  
  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)
  
  # Matrix of contrast weights between habitats within islands
  W <- lapply(levels(specdata$island), function(curr.island) {
    
    # What habitats are on this island?
    curr.habitats <- levels(droplevels(with(specdata, habitat[island == curr.island])))
    
    # All pairwise comparisons
    contrasts <- combn(curr.habitats, 2)
    
    # For each contrast (column)...
    W <- apply(contrasts, 2,  function(curr.contrast) {
      
      # Locate first habitat
      i <- grep(paste0(curr.island, ".*", curr.contrast[1]), groups)
      
      # Locate second habitat
      j <- grep(paste0(curr.island, ".*", curr.contrast[2]), groups)
      
      # Create a vector of contrast weights
      W <- rep(0, length(groups))
      W[i] <- 1
      W[j] <- -1
      
      return(W)
      
    })
    
    # Transpose to have each row corresponding to one contrast
    W <- t(W)
    
    return(W)
    
  })
  
  # Merge all vectors of contrast weights into a single matrix
  W <- do.call("rbind", W)
  
  return(W)
  
}
