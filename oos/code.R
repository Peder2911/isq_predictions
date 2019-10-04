# ================================================

## @knitr prep

source("util.R")


dat <- read_dta("data/predactual_01_09.dta") %>%
   mutate(minor_actual = as.numeric(conflict == 1),
          major_actual = as.numeric(conflict == 2)) %>%
   select(year, gwcode = gwno,
          minor_prob = c1, major_prob = c2,combined = sim,
          minor_actual, major_actual) %>%
   mutate(minor_pred_50 = as.numeric(minor_prob > 0.5),
          minor_pred_30 = as.numeric(minor_prob > 0.3),
          major_pred_50 = as.numeric(major_prob > 0.5),
          major_pred_30 = as.numeric(major_prob > 0.3))

dat <- dat[complete.cases(dat),]


pred_actual <- dat %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50) %>%
   mutate(either_actual = as.numeric(minor_actual | major_actual))

## @knitr table_2


incidence01_09 <- list()
incidence01_09$score <- aucFromPA(pred_actual$combined, pred_actual$either_actual)
incidence01_09$quantiles <- pROC::ci.auc(pred_actual$either_actual, pred_actual$combined)

subset <- pred_actual %>% filter(year > 2006)

onset <- bootstrappedROC(subset$combined, as.numeric(subset$onset_minor_actual |
                                                     subset$onset_major_actual),
                         res = 0.1, draws = niter,
                         parallel = TRUE)[[2]]

term <- bootstrappedROC(subset$combined, as.numeric(subset$term_minor_actual |
                                                     subset$term_major_actual),
                         res = 0.1, draws = niter,
                         parallel = TRUE)[[2]]

#incidence <- bootstrappedROC(subset$combined, subset$either_actual,
#                             parallel = TRUE)[[2]]
incidence <- list()
incidence$score <- aucFromPA(subset$combined, subset$either_actual)
incidence$quantiles <- pROC::ci.auc(subset$either_actual, subset$combined)

getMetric <- function(data,thresh,metric){ 
   tpr = withConfmat(as.numeric(data$combined > thresh), 
                      either(data, major_actual, minor_actual), metric)
}

getRates <- function(data, functions = list(recall = recall, fallout = fallout), digits = NULL){ 
   lapply(list(th_05 = 0.5, th_03 = 0.3),
          function(th){
             lapply(functions,
                    function(met) {
                       m <- getMetric(data,th,met)
                       if(!is.null(digits)){m <- round(m,digits = digits)}
                       m
                    })
                })
}


rates <- lapply(list(full = pred_actual,post07 = subset), 
                function(dat){
                   getRates(dat)
                })


res <- tibble(Type = c("Incidence",
                       "Incidence",
                       "Onset",
                       "Termination"),
              Score = c(incidence01_09$score,
                      incidence$score,
                      onset$score,
                      term$score),
              `0.25th` = c(incidence01_09$quantiles[1],
                           incidence$quantiles[1],
                           onset$quantiles[1],
                           term$quantiles[1]),
              `97.5th` = c(incidence01_09$quantiles[3],
                           incidence$quantiles[3],
                           onset$quantiles[2],
                           term$quantiles[2]),
              `TPR > 0.5` = c(rates$full$th_05$recall,
                      rates$post07$th_05$recall,
                      NA, NA),
              `FPR > 0.5` = c(rates$full$th_05$fallout,
                      rates$post07$th_05$fallout,
                      NA, NA), 
              `TPR > 0.3` = c(rates$full$th_03$recall,
                      rates$post07$th_03$recall,
                      NA, NA),
              `FPR > 0.3` = c(rates$full$th_03$fallout,
                      rates$post07$th_03$fallout,
                      NA, NA) 
              )

fixnames <- names(res)
fixnames[1] <- ""
fixnames[c(5,7)] <- "TPR"
fixnames[c(6,8)] <- "FPR"

kable(res, digits = 3, booktabs = TRUE, col.names = fixnames) %>%
   add_header_above(c(" " = 1, "AUC" = 3, "0.5" = 2, "0.3" = 2)) %>%
   pack_rows("01-09",1,1) %>%
   pack_rows("07-09",2,4)

## @knitr table_4

plt <- cintervalplot(pred_actual$combined, pred_actual$either_actual, res = 0.001)
plt$plot

## @knitr summaryfigures

summaryRates <- getRates(diq, functions = list(fallout = fallout, 
                          recall = recall, 
                          precision = precision, 
                          accuracy = accuracy),
                         digits = 3)


