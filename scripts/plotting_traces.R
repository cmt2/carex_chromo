library(RevGadgets)
library(coda)
library(ggpubr)
library(gridExtra)
library(grid)


makeCarexPlots <- function(trace_dir, plot_dir, data_dir) {
        print(trace_dir)
        ##### read in data #####
        path <- paste0(data_dir, trace_dir)
        log <- list.files(path)[grep("log",list.files(path))]
        nruns <- length(log)
        if (nruns == 1) {
                full_path <- paste0(path,"/",log)
                tc <- readTrace(full_path)
        } else if (nruns > 1) {
                full_paths <- paste0(path,"/",log)
                traces <- readTrace(full_paths)
                tc <- list(do.call(rbind,traces))
        }
       
        ##### process traces #####
        tracec <- tc[[1]]
        
        ess1 <- effectiveSize(as.mcmc(tracec))
        
        ##### set up data frame tables #####
        sum_clado <- summarizeTrace(tc, vars = c("clado_fission","clado_fusion","clado_no_change")) 
        sum_clado_df <- data.frame(mean = c(round(sum_clado[[1]][[1]]["mean"], digits = 3), 
                                               round(sum_clado[[2]][[1]]["mean"], digits = 3), 
                                               round(sum_clado[[3]][[1]]["mean"], digits = 3)), 
                                      quantile_2.5 = c(round(sum_clado[[1]][[1]]["quantile_2.5"], digits = 3),
                                                       round(sum_clado[[2]][[1]]["quantile_2.5"], digits = 3),
                                                       round(sum_clado[[3]][[1]]["quantile_2.5"], digits = 3)), 
                                      quantile_97.5 = c(round(sum_clado[[1]][[1]]["quantile_97.5"], digits = 3),
                                                        round(sum_clado[[2]][[1]]["quantile_97.5"], digits = 3),
                                                        round(sum_clado[[3]][[1]]["quantile_97.5"], digits = 3)))
        row.names(sum_clado_df) <- names(sum_clado)
        

        if (grepl("noRHO", trace_dir)) {
                sum_ana <- summarizeTrace(tc, vars = c("delta","gamma")) 
                sum_ana_df <-   data.frame(mean = c(round(sum_ana[[1]][[1]]["mean"], digits = 3), 
                                                    round(sum_ana[[2]][[1]]["mean"], digits = 3)), 
                                           quantile_2.5 = c(round(sum_ana[[1]][[1]]["quantile_2.5"], digits = 3),
                                                            round(sum_ana[[2]][[1]]["quantile_2.5"], digits = 3)), 
                                           quantile_97.5 = c(round(sum_ana[[1]][[1]]["quantile_97.5"], digits = 3),
                                                             round(sum_ana[[2]][[1]]["quantile_97.5"], digits = 3)))
                row.names(sum_ana_df) <- names(sum_ana)
        } else {
                sum_ana <- summarizeTrace(tc, vars = c("delta","gamma", "rho")) 
                sum_ana_df <-   data.frame(mean = c(round(sum_ana[[1]][[1]]["mean"], digits = 3), 
                                                    round(sum_ana[[2]][[1]]["mean"], digits = 3),
                                                    round(sum_ana[[3]][[1]]["mean"], digits = 3)), 
                                           quantile_2.5 = c(round(sum_ana[[1]][[1]]["quantile_2.5"], digits = 3),
                                                            round(sum_ana[[2]][[1]]["quantile_2.5"], digits = 3),
                                                            round(sum_ana[[3]][[1]]["quantile_2.5"], digits = 3)), 
                                           quantile_97.5 = c(round(sum_ana[[1]][[1]]["quantile_97.5"], digits = 3),
                                                             round(sum_ana[[2]][[1]]["quantile_97.5"], digits = 3),
                                                             round(sum_ana[[3]][[1]]["quantile_97.5"], digits = 3)))
                row.names(sum_ana_df) <- names(sum_ana)
        }
        
        ##### create diff variables #####
        tc[[1]]$diff_fission_nochange <- tc[[1]]$clado_fission-tc[[1]]$clado_no_change
        tc[[1]]$diff_fusion_nochange <- tc[[1]]$clado_fusion-tc[[1]]$clado_no_change    
        
        
        ##### produce summary plots #####
        pdf_path <- paste0(plot_dir, trace_dir, "_summaryplots.pdf")
        pdf(pdf_path)
        
        ##### plot traces #####
        par(mfrow=c(3,1))
        plot(tracec$clado_fission, 
             type = "l", 
             yaxt = 'n',
             ylab = NA,
             xlab = NA,
             main = paste0("Clado_fission Combined ESS: ", round(ess1["clado_fission"], digits = 2)))
        
        plot(tracec$clado_fusion, 
             type = "l", 
             yaxt = 'n',
             ylab = NA,
             xlab = NA,
             main = paste0("Clado_fusion Combined ESS: ", round(ess1["clado_fusion"], digits = 2)))

        plot(tracec$clado_no_change, 
             type = "l",
             yaxt = 'n',
             ylab = NA,
             xlab = NA,
             main = paste0("Clado_no_change ESS: ", round(ess1["clado_no_change"], digits = 2)))
     
        #par(mfrow=c(2,2))
        
        plot(tracec$gamma, 
             type = "l", 
             yaxt = 'n',
             ylab = NA,
             xlab = NA,
             main = paste0("Gamma (chromosome gains)\nCombined ESS: ", round(ess1["gamma"], digits = 2)))
        
        plot(tracec$delta, 
             type = "l", 
             yaxt = 'n',
             ylab = NA,
             xlab = NA,
             main = paste0("Delta (chromosome losses)\nCombined ESS: ", round(ess1["delta"], digits = 2)))
        if (!grepl("noRHO", trace_dir)) {
                plot(tracec$rho, 
                     type = "l", 
                     yaxt = 'n',
                     ylab = NA,
                     xlab = NA,
                     main = paste0("Rho (polyploidy)\nCombined ESS: ", round(ess1["rho"], digits = 2)))     
        }

        par(mfrow=c(1,1))
        
        print(plotTrace(tc, vars = c("clado_fission","clado_fusion",
                                     "clado_no_change")))
        
        print(plotTrace(tc, vars = c("diff_fission_nochange","diff_fusion_nochange")))
        
        tablec <- tableGrob(sum_clado_df, theme = ttheme_default(base_size = 10))
        textc <- textGrob("Combined Trace", just = "left")
        
        layout <- rbind(c(1, 2, 2, 2))
        grid.arrange(grobs = list(textc, tablec),
                     layout_matrix = layout)
        
        if (grepl("noRHO", trace_dir)) {
                print(plotTrace(tc, vars = c("gamma","delta")))     
        } else {
                p1 <- plotTrace(tc, vars = c("gamma","delta"))[[1]]
                p2 <- plotTrace(tc, vars = c("rho"))[[1]]
                layout <- rbind(c(1),
                                c(2))
                grid.arrange(grobs = list(p1,p2),
                             layout_matrix = layout)
        }
        
        tablec <- tableGrob(sum_ana_df, theme = ttheme_default(base_size = 10))
        textc <- textGrob("Combined Trace", just = "left")
        
        layout <- rbind(c(1, 2, 2, 2))
        grid.arrange(grobs = list(textc, tablec),
                     layout_matrix = layout)
        
        dev.off()
}


data_dir <- "~/Desktop/ChromoSSE_Carex_output/"
plot_dir <- "~/Desktop/plots/"
trace_dirs <- c("withoutSiderosticta_noRHO","withoutSiderosticta_RHO",  
                "withSiderosticta_noRHO", "withSiderosticta_RHO")
trace_dirs <- trace_dirs[4]
lapply(trace_dirs, makeCarexPlots, plot_dir, data_dir)
