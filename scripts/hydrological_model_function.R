
# Predicting shifts in trait distributions due to environmental change

# Write a function to run the hydrological model

hydro_model <- function(P_R_vec, Ev_vec, min_depth, max_depth) {
  
  depth <- vector(mode = "double", length = length(P_R_vec))
  
  for (i in 2:length(depth) ) {
    
    # the precipitation or evaporation data is missing then use the previous value for depth
    if (is.na(P_R_vec[i]) | is.na(Ev_vec[i]) ) {
      
      x <- depth[i-1]
      
    } else {
      
      # calculate the water balance at t:
      # depth at t-1 + (P_R i.e. rainfall) - (Ev i.e. evaporation)
      x <- depth[i-1] + P_R_vec[i] - Ev_vec[i]
      
      # if water balance exceeds maximum pool depth, then use the max pool depth
      x <- ifelse(x > max_depth, max_depth, x)
      
      # if water balance is less than minimum depth, then use the min pool depth
      x <- ifelse(x < min_depth, min_depth, x)
      
    }
    
    depth[i] <- x
    
  }
  
  return(depth)
  
}

### END
