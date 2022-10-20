library(RevGadgets)
library(ape)

setwd("~/Documents/carex_chromo")
source("scripts/plotting/plot_simmap.R")
source("scripts/plotting/readStochasticMaps.R")

##### READ IN MAPS #####
my_maps <- 
  readStochasticMaps(map_paths = c("output/chromohisse/chromohisse_ana_hidden_mrm_maps1.log",
                                   "output/chromohisse/chromohisse_ana_hidden_mrm_maps2.log"))

# unlist list of maps
maps <- my_maps[[1]]

##### PROCESS TO SHOW EITHER HIDDEN OR OBSERVED STATES #####
convert_to_observed <- function(n) {
  if (n > 72) {
    return(n - 72)
  } else return(n)
}

convert_to_AB <- function(n) {
  if (n > 72) {
    return("B")
  } else return("A")
}

maps_observed <- maps
maps_AB       <- maps

for (i in 1:length(maps)) {
  for (j in 1:length(maps[[i]]$maps)){
    these_names <- as.integer(names(maps_observed[[i]]$maps[[j]]))
    
    observed_names <- as.character(unlist(lapply(these_names, convert_to_observed)))
    ab_names <- unlist(lapply(these_names, convert_to_AB))
    
    names(maps_observed[[i]]$maps[[j]]) <- observed_names
    names(maps_AB[[i]]$maps[[j]]) <- ab_names
  }
}

maps_AB_reversed <- maps_AB
# 
# for (i in 1:length(maps_AB_reversed)) {
#   for (j in 1:length(maps_AB_reversed[[i]]$maps)) {
#     maps_AB_reversed[[i]]$maps[[j]] <- rev(maps_AB_reversed[[i]]$maps[[j]])
#   }
# }

maps_observed_reversed <- maps_observed

# for (i in 1:length(maps_observed_reversed)) {
#   for (j in 1:length(maps_observed_reversed[[i]]$maps)) {
#     maps_observed_reversed[[i]]$maps[[j]] <- rev(maps_observed_reversed[[i]]$maps[[j]])
#   }
# }

# read in chromosome counts: 

chromo_data <- read.table("~/Documents/carex_chromo/data/carex_chromosome_counts_withoutSiderosticta.tsv",
                        sep = "\t")

chromo_data_vec <- chromo_data$V2
names(chromo_data_vec) <- chromo_data$V1

dat <- chromo_data_vec[maps_AB_reversed[[1]]$tip.label]

##### PLOT HIDDEN #####
col_vec <- colFun(2)
names(col_vec) <- c("A", "B")
pdf("~/Desktop/tip_data.pdf", height = 40, width = 10)
plot_simmap(time_tree = maps_AB_reversed[[1]], 
            tree = maps_AB_reversed[[1]], 
            nt = 10001,
            simmaps = maps_AB_reversed, 
            states = c("A", "B"),
            show.tip.label = FALSE,
            #text = NULL,
            #cex = dat/max(dat),
            plot_pie = F,
            lwd = 4,
            label.cex = 0.5,
            colors = col_vec)
tiplabels(pch = 20, 
          adj = c(1, 0.5),
          cex = (dat/max(dat)*10))
# legend(x = 0, y = 200, legend = names(col_vec), col = col_vec, pch = 20)
dev.off()

##### PLOT OBSERVED #####
states <- as.character(1:72)
col_vec <- colorRampPalette(c("purple","green"))(72)
names(col_vec) <- states
pdf("figures/chromohisse_simmap_ana_hidden_mrm_OBSERVED.pdf", height = 40, width = 10)
plot_simmap(time_tree = maps_observed_reversed[[1]], 
            tree = maps_observed_reversed[[1]], 
            nt = 10001,
            simmaps = maps_observed_reversed, 
            states = states,
            show.tip.label = TRUE,
            lend = 2,
            plot_pie = F,
            lwd = 4,
            label.cex = 0.5,
            colors = col_vec)
legend(x = 0, y = 200, legend = names(col_vec), col = col_vec, pch = 20)
dev.off()


