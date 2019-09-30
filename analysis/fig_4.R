
library(ggplot2)
library(evallib)

data <- readRDS("data/prepped.rds")

# =================================================

data$either <- data$major_actual | data$minor_actual

plt <- cintervalplot(data$combined, data$either, draws = 2000, parallel = TRUE)

