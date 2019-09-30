
library(dplyr)
library(zoo)
library(evallib)

data <- readRDS("data/prepped.rds")
source("R/evallib.R")

niter <- 10

regions <- unique(data$regionname)
names(regions) <- regions

results <- lapply(regions, function(region){
      sub <- data[data$regionname == region,]
      bootstrappedROC(sub$combined, as.numeric(sub$major_actual | sub$minor_actual),
                     parallel = TRUE, n = niter)
   })

results <- tibble::tibble(regionname = regions,
                          score = sapply(results,function(x) x$auc$score),
                          lower = sapply(results,function(x) x$auc$quantiles[1]),
                          upper = sapply(results,function(x) x$auc$quantiles[2]))

names(results) <- c("Region Name", "AUC", "0.25th Quantile","97.5th Quantile")

