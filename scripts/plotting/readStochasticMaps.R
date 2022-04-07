
readStochasticMaps <- function(map_paths, 
                               burnin = 0.1,
                               combine_runs = TRUE) {
  
  # read in map logs and remove burnin
  maps <- vector("list", length = length(map_paths))
  for (i in 1:length(map_paths)) {
    message(paste0("Reading in map log file ", i, "\n",
                   "This may take a while."))
    maps[[i]] <- read.table(file = map_paths[i], 
                            sep = "\t",
                            header = T,
                            row.names = 1)
    end_burnin <- round(burnin*nrow(maps[[i]]), digits = 0)
    maps[[i]] <- maps[[i]][c(end_burnin : nrow(maps[[i]])),]
  }
  
  # combine log files 
  if (combine_runs == T) {
    maps <- list(do.call(rbind, maps))
  }
  
  # get maps from output
  maps_only <- vector("list", length = length(maps))
  for (i in 1:length(maps)) {
    maps_only[[i]] <- vector("list", length = nrow(maps[[i]]))
    message(paste0("Converting trace ", 
                   i, 
                   " to a list of SIMMAP objects",
                   "\n",
                   "This may take a while."))
    for (j in 1:nrow(maps[[i]])){
      maps_only[[i]][[j]] <- 
        phytools::read.simmap(text = maps[[i]][j,"simmap"])
    }
  }
  
  # reverse branch order
  # revbayes outputs the maps in reverse order along branches 
  for (i in 1:length(maps_only)) {
    for (j in 1:length(maps_only[[i]])) {
      for (k in 1:length(maps_only[[i]][[j]]$maps))
        maps_only[[i]][[j]]$maps[[k]] <- rev(maps_only[[i]][[j]]$maps[[k]])
    }
  }
  
  return(maps_only)
  
}