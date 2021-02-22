library(RevGadgets)
library(coda)
library(ggpubr)
library(gridExtra)
library(grid)

t1 <- readTrace(paths = "~/Documents/carex_chromo/output/tp_dirichlet_withoutSiderosticta_run1.log")
t2 <- readTrace(paths = "~/Documents/carex_chromo/output/tp_dirichlet_withoutSiderosticta_run2.log")

trace1 <- t1[[1]]
trace2 <- t2[[1]]

tracec <- rbind(removeBurnin(t1, 0.1)[[1]], removeBurnin(t2, 0.1)[[1]])
tc <- list(tracec)

ess1 <- effectiveSize(as.mcmc(trace1))
ess2 <- effectiveSize(as.mcmc(trace2))
essc <- effectiveSize(as.mcmc(tracec))

t1 <- removeBurnin(t1, 0.1)
t2 <- removeBurnin(t2, 0.1)

pdf("~/Documents/carex_chromo/convergence_withoutSiderosticta2.pdf")
trace <- trace2
ess <- ess2
t <- t2

sum <- summarizeTrace(t, vars = c("clado_fission","clado_fusion",
                                  "clado_no_change")) 
sum_df <- data.frame(mean = c(round(sum[[1]][[1]]$mean, digits = 3), 
                              round(sum[[2]][[1]]$mean, digits = 3), 
                              round(sum[[3]][[1]]$mean, digits = 3)), 
                     quantile_2.5 = c(round(sum[[1]][[1]]$quantile_2.5, digits = 3),
                                      round(sum[[2]][[1]]$quantile_2.5, digits = 3),
                                      round(sum[[3]][[1]]$quantile_2.5, digits = 3)), 
                     quantile_97.5 = c(round(sum[[1]][[1]]$quantile_97.5, digits = 3),
                                       round(sum[[2]][[1]]$quantile_97.5, digits = 3),
                                       round(sum[[3]][[1]]$quantile_97.5, digits = 3)))
row.names(sum_df) <- names(sum)

par(mfrow=c(3,1))
plot(trace$clado_fission, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fission ESS: ", round(ess["clado_fission"], digits = 2)))
plot(trace$clado_fusion, 
     type = "l", 
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_fusion ESS: ", round(ess["clado_fusion"], digits = 2)))
plot(trace$clado_no_change, 
     type = "l",
     yaxt = 'n',
     ylab = NA,
     xlab = NA,
     main = paste0("Clado_no_change ESS: ", round(ess["clado_no_change"], digits = 2)))

par(mfrow=c(1,1))

plotTrace(t, vars = c("clado_fission","clado_fusion",
                                             "clado_no_change"))
grid.newpage()
grid.table(sum_df)


t[[1]]$diff_fission_nochange <- t[[1]]$clado_fission-t[[1]]$clado_no_change
t[[1]]$diff_fusion_nochange <- t[[1]]$clado_fusion-t[[1]]$clado_no_change


plotTrace(t,
          vars = c("diff_fission_nochange","diff_fusion_nochange"))


dev.off()
