# check convergence of non-normal parameters 
# based on Vehtari et al 2020: 
# http://www.stat.columbia.edu/~gelman/research/published/rhat.pdf

# input your trace file path and parameter name

file_path <- "~/Downloads/chromohisse_ana_hidden_mrm_run2.log"
param_name <- "delta_b"
burnin <- 1000 # number of generations 

#### execute this code to print ESS values 
trace <- read.table(file_path, sep = "\t", header = T)
param <- trace[ , which(colnames(trace) == param_name)][(burnin+1):nrow(trace)]
param_ordered <-  order(param)
param_transformed <- qnorm((param_ordered - 3/8) / (length(param_ordered) + 1/4))

ESS_standard <- coda::effectiveSize(param)
ESS_ordered <- coda::effectiveSize(param_ordered)
ESS_transformed <-coda::effectiveSize(param_transformed)

cat(paste0(
  "Standard ESS value: ", round(ESS_standard, digits = 2), "\n",
  "Transformed ESS Value: ", round(ESS_transformed, digits = 2)
))
