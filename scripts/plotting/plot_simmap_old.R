library(plotrix)
library(phytools)
setwd("~/Documents/carex_chromo/")
 
character_file <- "output/stochastic_chromohisse_maps_character.tree"

write_pdf <- TRUE

if (write_pdf) {
  pdf("figures/chromohisse_simmap.pdf", height = 40, width = 10)
}

sim <- read.simmap(file=character_file, format="phylip")

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

sim_observed <- sim
sim_AB <- sim
for (i in 1:length(sim$maps)){
  these_names <- as.integer(names(sim_observed$maps[[i]]))
  
  observed_names <- as.character(unlist(lapply(these_names, convert_to_observed)))
  ab_names <- unlist(lapply(these_names, convert_to_AB))
  
  names(sim_observed$maps[[i]]) <- observed_names
  names(sim_AB$maps[[i]]) <- ab_names
}

## first plot observed

colors <- as.character(c(0:72))
pal_fun <- colorRampPalette(RevGadgets::colFun(2))
cols <- setNames( c("white",pal_fun(72)), colors )

plotSimmap(sim_observed, 
           cols, 
           fsize=0.5, 
           lwd=4, 
           split.vertical=TRUE, 
           ftype="i")
legend.gradient(c(10,300), cols = RevGadgets::colFun(2), limits = c(1,72))
# add legend
colorbar.plot(x = 10, 
              y = 300, 
              adj.x = 1,
              adj.y = 1,
              col = pal_fun(72), 
              strip = seq(from = 0, to = 72, by = 1), 
              strip.width = 0.05, 
              strip.length = 0.5, 
              horizontal = F)


# now plot hidden
colors <- c("A", "B")
cols <- setNames( RevGadgets::colFun(2), colors)
plotSimmap(sim_AB, 
           cols, 
           fsize=0.5, 
           lwd=2, 
           split.vertical=TRUE, 
           ftype="i")


if (write_pdf) {
  dev.off()
}
