library(RevGadgets)
setwd("~/Documents/carex_chromo/")

t <- readTrace(c("output/ChromoSSE_Carex_output/withoutSiderosticta_noRHO/tp_dirichlet_withoutSiderosticta_run1.log",
                 "output/ChromoSSE_Carex_output/withoutSiderosticta_noRHO/tp_dirichlet_withoutSiderosticta_run2.log"))

vars <- c("delta", "gamma", "relative_clado[1]", "relative_clado[2]", 
          "relative_clado[3]", "total_speciation", "turnover")

s <- summarizeTrace(t, vars = vars)

vals <- vector()
for (i in 1:length(s)) {
  vals <- append(vals, s[[i]][[3]]["mean"])
}

lines <- paste0(vars,".setValue(",vals, ")")
