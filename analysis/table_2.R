library(dplyr)
library(zoo)
library(evallib)

data_full <- readRDS("data/prepped.rds")
source("R/evallib.R")

either <- function(data,a,b){
   as.numeric(data[[as.character(substitute(a))]] | 
              data[[as.character(substitute(b))]])
}

data <- data_full %>% filter(year>2010)

incidence_auc_1 <- bootstrappedROC(data_full$combined, either(data_full,
                                                        major_actual,
                                                        minor_actual),
                                   parallel = TRUE, n = 10000)


onset_auc <- bootstrappedROC(data$combined, either(data, 
                                             onset_major_actual,
                                             onset_minor_actual),
                             parallel = TRUE, n = 10000)

term_auc <- bootstrappedROC(data$combined,either(data,
                                           term_major_actual,
                                           term_minor_actual),
                            parallel = TRUE, n = 10000)

incidence_auc_2 <- bootstrappedROC(data$combined,either(data,
                                                  major_actual,
                                                  minor_actual),
                                   parallel = TRUE, n = 10000)

p50 <- list(tpr = withConfmat(either(data,major_pred_50,minor_pred_50),
                         either(data,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data,major_pred_50,minor_pred_50),
                          either(data,major_actual,minor_actual), fallout))

p30 <- list(tpr = withConfmat(either(data,major_pred_30,minor_pred_30),
                         either(data,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data,major_pred_30,minor_pred_30),
                          either(data,major_actual,minor_actual), fallout))

