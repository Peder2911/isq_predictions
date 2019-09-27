
library(dplyr)
library(zoo)
library(evallib)

data <- readRDS("data/prepped.rds")
source("R/evallib.R")

region_summary <- data %>%
   group_by(regionname) %>%
   summarize(auc = bootstrappedROC(combined,as.numeric(major_actual | minor_actual),
                                   parallel = TRUE, n = 10000)[2])

