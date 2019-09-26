
library(dplyr)
library(zoo)

data <- readRDS("data/prepped.rds")
source("R/evallib.R")

region_summary <- data %>%
   group_by(regionname) %>%
   summarize(auc = aucFromPA(combined,as.numeric(major_actual | minor_actual)))

