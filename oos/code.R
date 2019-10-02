## @knitr table_2

data_t2_full <- readRDS("data/prepped.rds")

either <- function(data_t2,a,b){
   as.numeric(data_t2[[as.character(substitute(a))]] | 
              data_t2[[as.character(substitute(b))]])
}

data_t2 <- data_t2_full %>% filter(year>2006)

incidence_auc_1 <- bootstrappedROC(data_t2_full$combined, either(data_t2_full,
                                                        conflict2,
                                                        conflict1),
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
