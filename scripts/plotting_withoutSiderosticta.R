library(RevGadgets)
library(coda)
library(ggpubr)
library(gridExtra)
library(grid)

##### read in data #####
t1 <- readTrace(paths = "~/Documents/carex_chromo/output/tp_dirichlet_withoutSiderosticta_run1.log")
t2 <- readTrace(paths = "~/Documents/carex_chromo/output/tp_dirichlet_withoutSiderosticta_run2.log")

##### process traces #####
trace1 <- t1[[1]]
trace2 <- t2[[1]]

tracec <- rbind(removeBurnin(t1, 0.1)[[1]], removeBurnin(t2, 0.1)[[1]])
tc <- list(tracec)

ess1 <- effectiveSize(as.mcmc(trace1))
ess2 <- effectiveSize(as.mcmc(trace2))
essc <- effectiveSize(as.mcmc(tracec))

t1 <- removeBurnin(t1, 0.1)
t2 <- removeBurnin(t2, 0.1)

##### set up data frame tables #####
sum_clado_t1 <- summarizeTrace(t1, vars = c("clado_fission","clado_fusion","clado_no_change")) 
sum_clado_t1_df <- data.frame(mean = c(round(sum_clado_t1[[1]][[1]]$mean, digits = 3), 
                                       round(sum_clado_t1[[2]][[1]]$mean, digits = 3), 
                                       round(sum_clado_t1[[3]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_clado_t1[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_t1[[2]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_t1[[3]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_clado_t1[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_t1[[2]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_t1[[3]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_clado_t1_df) <- names(sum_clado_t1)

sum_clado_t2 <- summarizeTrace(t2, vars = c("clado_fission","clado_fusion","clado_no_change")) 
sum_clado_t2_df <- data.frame(mean = c(round(sum_clado_t2[[1]][[1]]$mean, digits = 3), 
                                       round(sum_clado_t2[[2]][[1]]$mean, digits = 3), 
                                       round(sum_clado_t2[[3]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_clado_t2[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_t2[[2]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_t2[[3]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_clado_t2[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_t2[[2]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_t2[[3]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_clado_t2_df) <- names(sum_clado_t2)

sum_clado_tc <- summarizeTrace(tc, vars = c("clado_fission","clado_fusion","clado_no_change")) 
sum_clado_tc_df <- data.frame(mean = c(round(sum_clado_tc[[1]][[1]]$mean, digits = 3), 
                                       round(sum_clado_tc[[2]][[1]]$mean, digits = 3), 
                                       round(sum_clado_tc[[3]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_clado_tc[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_tc[[2]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_clado_tc[[3]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_clado_tc[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_tc[[2]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_clado_tc[[3]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_clado_tc_df) <- names(sum_clado_tc)

sum_ana_t1 <- summarizeTrace(t1, vars = c("delta","gamma")) 
sum_ana_t1_df <-   data.frame(mean = c(round(sum_ana_t1[[1]][[1]]$mean, digits = 3), 
                                       round(sum_ana_t1[[2]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_ana_t1[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_ana_t1[[2]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_ana_t1[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_ana_t1[[2]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_ana_t1_df) <- names(sum_ana_t1)

sum_ana_t2 <- summarizeTrace(t2, vars = c("delta","gamma")) 
sum_ana_t2_df <-   data.frame(mean = c(round(sum_ana_t2[[1]][[1]]$mean, digits = 3), 
                                       round(sum_ana_t2[[2]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_ana_t2[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_ana_t2[[2]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_ana_t2[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_ana_t2[[2]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_ana_t2_df) <- names(sum_ana_t2)

sum_ana_tc <- summarizeTrace(tc, vars = c("delta","gamma")) 
sum_ana_tc_df <-   data.frame(mean = c(round(sum_ana_tc[[1]][[1]]$mean, digits = 3), 
                                       round(sum_ana_tc[[2]][[1]]$mean, digits = 3)), 
                              quantile_2.5 = c(round(sum_ana_tc[[1]][[1]]$quantile_2.5, digits = 3),
                                               round(sum_ana_tc[[2]][[1]]$quantile_2.5, digits = 3)), 
                              quantile_97.5 = c(round(sum_ana_tc[[1]][[1]]$quantile_97.5, digits = 3),
                                                round(sum_ana_tc[[2]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_ana_tc_df) <- names(sum_ana_tc)

##### create diff variables #####
t1[[1]]$diff_fission_nochange <- t1[[1]]$clado_fission-t1[[1]]$clado_no_change
t1[[1]]$diff_fusion_nochange <- t1[[1]]$clado_fusion-t1[[1]]$clado_no_change

t2[[1]]$diff_fission_nochange <- t2[[1]]$clado_fission-t2[[1]]$clado_no_change
t2[[1]]$diff_fusion_nochange <- t2[[1]]$clado_fusion-t2[[1]]$clado_no_change

tc[[1]]$diff_fission_nochange <- tc[[1]]$clado_fission-tc[[1]]$clado_no_change
tc[[1]]$diff_fusion_nochange <- tc[[1]]$clado_fusion-tc[[1]]$clado_no_change

##### produce summary plots #####
pdf("~/Documents/carex_chromo/convergence_withoutSiderosticta.pdf")

##### plot traces #####
par(mfrow=c(3,2))
plot(trace1$clado_fission, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fission 1 ESS: ", round(ess1["clado_fission"], digits = 2)))
plot(trace2$clado_fission, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fission 2 ESS: ", round(ess2["clado_fission"], digits = 2)))

plot(trace1$clado_fusion, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fusion 1 ESS: ", round(ess1["clado_fusion"], digits = 2)))
plot(trace2$clado_fusion, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fusion 2 ESS: ", round(ess2["clado_fusion"], digits = 2)))

plot(trace1$clado_no_change, 
     type = "l",
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_no_change 1 ESS: ", round(ess1["clado_no_change"], digits = 2)))
plot(trace2$clado_no_change, 
     type = "l",
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_no_change 2 ESS: ", round(ess2["clado_no_change"], digits = 2)))

par(mfrow=c(2,2))

plot(trace1$gamma, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Gamma (chromosome gains) 1\nESS: ", round(ess1["gamma"], digits = 2)))
plot(trace2$gamma, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Gamma (chromosome gains) 2\nESS: ", round(ess2["gamma"], digits = 2)))

plot(trace1$delta, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Delta (chromosome losses) 1\nESS: ", round(ess1["delta"], digits = 2)))
plot(trace2$delta, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Delta (chromosome losses) 2\nESS: ", round(ess2["delta"], digits = 2)))

par(mfrow=c(1,1))

p1 <- plotTrace(t1, vars = c("clado_fission","clado_fusion",
                             "clado_no_change"))
p2 <- plotTrace(t2, vars = c("clado_fission","clado_fusion",
                             "clado_no_change"))
pc <- plotTrace(tc, vars = c("clado_fission","clado_fusion",
                             "clado_no_change"))
grid.arrange(p1[[1]] + ggtitle("Trace 1"), 
             p2[[1]] + ggtitle("Trace 2"), 
             pc[[1]] + ggtitle("Combined Trace"))

p1_diff <- plotTrace(t1, vars = c("diff_fission_nochange","diff_fusion_nochange"))
p2_diff <- plotTrace(t2, vars = c("diff_fission_nochange","diff_fusion_nochange"))
pc_diff <- plotTrace(tc, vars = c("diff_fission_nochange","diff_fusion_nochange"))

grid.arrange(p1_diff[[1]] + ggtitle("Trace 1"), 
             p2_diff[[1]] + ggtitle("Trace 2"), 
             pc_diff[[1]] + ggtitle("Combined Trace"))

table1 <- tableGrob(sum_clado_t1_df, theme = ttheme_default(base_size = 10))
text1 <- textGrob("Trace 1", just = "left")
table2 <- tableGrob(sum_clado_t2_df, theme = ttheme_default(base_size = 10))
text2 <- textGrob("Trace 2", just = "left")
tablec <- tableGrob(sum_clado_tc_df, theme = ttheme_default(base_size = 10))
textc <- textGrob("Combined Trace", just = "left")

layout <- rbind(c(1, 2, 2, 2),
                c(3, 4, 4, 4),
                c(5, 6, 6, 6))
grid.arrange(grobs = list(text1, table1,
                          text2, table2,
                          textc, tablec),
             layout_matrix = layout)

p1 <- plotTrace(t1, vars = c("gamma","delta")) 
p2 <- plotTrace(t2, vars = c("gamma","delta")) 
pc <- plotTrace(tc, vars = c("gamma","delta")) 

grid.arrange(p1[[1]] + ggtitle("Trace 1"), 
             p2[[1]] + ggtitle("Trace 2"), 
             pc[[1]] + ggtitle("Combined Trace"))

table1 <- tableGrob(sum_ana_t1_df, theme = ttheme_default(base_size = 10))
text1 <- textGrob("Trace 1", just = "left")
table2 <- tableGrob(sum_ana_t2_df, theme = ttheme_default(base_size = 10))
text2 <- textGrob("Trace 2", just = "left")
tablec <- tableGrob(sum_ana_t3_df, theme = ttheme_default(base_size = 10))
textc <- textGrob("Combined Trace", just = "left")

layout <- rbind(c(1, 2, 2, 2),
                c(3, 4, 4, 4),
                c(5, 6, 6, 6))
grid.arrange(grobs = list(text1, table1,
                          text2, table2,
                          textc, tablec),
             layout_matrix = layout)

dev.off()
