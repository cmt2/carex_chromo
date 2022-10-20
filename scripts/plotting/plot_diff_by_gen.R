#disploidy vs no change
library(RevGadgets)
setwd("~/Documents/carex_chromo/")


chromohisse <- readTrace(c("output/chromohisse_ana_hidden_mrm_run1.log",
                           "output/chromohisse_ana_hidden_mrm_run2.log"))
chromohisse <- combineTraces(chromohisse)

length(chromohisse)


chromohisse[[1]]$disploidy_a <- chromohisse[[1]]$clado_fission_a + chromohisse[[1]]$clado_fusion_a 

chromohisse[[1]]$disploidy_b <- chromohisse[[1]]$clado_fission_b + chromohisse[[1]]$clado_fusion_b 

chromohisse[[1]]$total_no_change_a <- chromohisse[[1]]$chi + chromohisse[[1]]$clado_no_change_a

chromohisse[[1]]$total_no_change_b <- chromohisse[[1]]$chi + chromohisse[[1]]$clado_no_change_b

chromohisse[[1]]$diff_a <- chromohisse[[1]]$disploidy_a - chromohisse[[1]]$total_no_change_a
chromohisse[[1]]$diff_b <- chromohisse[[1]]$disploidy_b - chromohisse[[1]]$total_no_change_b

plotTrace(chromohisse, vars = c("diff_a", "diff_b"))
