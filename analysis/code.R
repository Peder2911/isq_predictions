
library(dplyr)
library(zoo)
library(evallib)

## @knitr table_2

data_t2_full <- readRDS("data/prepped.rds")

either <- function(data_t2,a,b){
   as.numeric(data_t2[[as.character(substitute(a))]] | 
              data_t2[[as.character(substitute(b))]])
}

data_t2 <- data_t2_full %>% filter(year>2010)

incidence_auc_1 <- bootstrappedROC(data_t2_full$combined, either(data_t2_full,
                                                        major_actual,
                                                        minor_actual),
                                   parallel = TRUE, n = niter)


onset_auc <- bootstrappedROC(data_t2$combined, either(data_t2, 
                                             onset_major_actual,
                                             onset_minor_actual),
                             parallel = TRUE, n = niter)

term_auc <- bootstrappedROC(data_t2$combined,either(data_t2,
                                           term_major_actual,
                                           term_minor_actual),
                            parallel = TRUE, n = niter)

incidence_auc_2 <- bootstrappedROC(data_t2$combined,either(data_t2,
                                                  major_actual,
                                                  minor_actual),
                                   parallel = TRUE, n = niter)

p50 <- list(tpr = withConfmat(either(data_t2,major_pred_50,minor_pred_50),
                         either(data_t2,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data_t2,major_pred_50,minor_pred_50),
                          either(data_t2,major_actual,minor_actual), fallout))

p30 <- list(tpr = withConfmat(either(data_t2,major_pred_30,minor_pred_30),
                         either(data_t2,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data_t2,major_pred_30,minor_pred_30),
                          either(data_t2,major_actual,minor_actual), fallout))

tab <- tibble::tibble(
   AUC = c(incidence_auc_2$auc$score,
         onset_auc$auc$score,
         term_auc$auc$score),
   `0.25th quantile` = c(incidence_auc_2$auc$quantiles[1],
                         onset_auc$auc$quantiles[1],
                         term_auc$auc$quantiles[1]),
   `97.5th quantile` = c(incidence_auc_2$auc$quantiles[2],
                         onset_auc$auc$quantiles[2],
                         term_auc$auc$quantiles[2]))

row.names(tab) <- c("Incidence", "Onset", "Termination")

capt <- glue::glue("Bootstrap AUC scores ({niter} random draws)")
class(capt) <- "character"
xtable::xtable(tab, caption = capt)

## @knitr table_4

library(dplyr)
library(zoo)
library(evallib)

data_t4 <- readRDS("data/prepped.rds")
source("R/evallib.R")

regions <- unique(data_t4$regionname)
names(regions) <- regions

results <- lapply(regions, function(region){
      sub <- data_t4[data_t4$regionname == region,]
      bootstrappedROC(sub$combined, as.numeric(sub$major_actual | sub$minor_actual),
                     parallel = TRUE, n = niter)
   })
tee <- results

results <- tibble::tibble(regionname = regions,
                          score = sapply(results,function(x) x$auc$score),
                          lower = sapply(results,function(x) x$auc$quantiles[1]),
                          upper = sapply(results,function(x) x$auc$quantiles[2]))

names(results) <- c("Region Name", "AUC", "0.25th Quantile","97.5th Quantile")
xtable::xtable(results, caption = capt)

## @knitr figure_4

data_f4 <- readRDS("data/prepped.rds")

data_f4$either <- data_f4$major_actual | data_f4$minor_actual
plt <- cintervalplot(data_f4$combined, data_f4$either, draws = niter, parallel = TRUE)

auc <- plt$results$auc
dgts <- 4
aucCaption <- glue("AUC: {round(auc$score, dgts)} ({round(auc$quantiles[1], dgts)} - {round(auc$quantiles[2], dgts)})")

plt$plot + 
   labs(x = "Specificity",
        y = "Sensitivity",
        title = "Bootstrap ROC curve",
        subtitle = aucCaption, 
        caption = glue("({niter} random draws)"))
