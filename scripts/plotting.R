library(RevGadgets)
library(coda)
t <- readTrace(path = "~/Documents/carex_chromo/output/tp_dirichlet.log")
trace <- t[[1]]

ess <- effectiveSize(as.mcmc(trace))

pdf("~/Documents/carex_chromo/convergence_orig.pdf")
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
plotTrace(removeBurnin(t, 0.1), vars = c("clado_fission","clado_fusion",
                                             "clado_no_change"))

t[[1]]$diff_fission_nochange <- t[[1]]$clado_fission-t[[1]]$clado_no_change
t[[1]]$diff_fusion_nochange <- t[[1]]$clado_fusion-t[[1]]$clado_no_change

plotTrace(removeBurnin(t, 0.1),
          vars = c("diff_fission_nochange","diff_fusion_nochange"))
dev.off()
