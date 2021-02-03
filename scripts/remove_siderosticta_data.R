library(ape)
data <- read.table(file = "~/Documents/carex_chromo/data/carex_chromosome_counts.tsv")
tree <- read.tree(file = "~/Documents/carex_chromo/data/Carex_withoutSiderosticta.tree")
data_trimmed <- data[data[,1] %in% tree$tip.label,]
write.table(data_trimmed, 
            file = "~/Documents/carex_chromo/data/carex_chromosome_counts_withoutSiderosticta.tsv",
            quote=FALSE, sep='\t', col.names = F, row.names = F)
