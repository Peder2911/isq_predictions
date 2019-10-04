
## @knitr table_2

data_t2_full <- readRDS("data/prepped.rds")

either <- function(data_t2,a,b){
   as.numeric(data_t2[[as.character(substitute(a))]] | 
              data_t2[[as.character(substitute(b))]])
}

data_t2 <- data_t2_full %>% filter(year>2010)

incidence_auc_1 <- aucWithCI(data_t2_full$combined, either(data_t2_full,
                                                        major_actual,
                                                        minor_actual))


onset_auc <- aucWithCI(data_t2$combined, either(data_t2, 
                                             onset_major_actual,
                                             onset_minor_actual))

term_auc <- aucWithCI(data_t2$combined,either(data_t2,
                                           term_major_actual,
                                           term_minor_actual))

incidence_auc_2 <- aucWithCI(data_t2$combined,either(data_t2,
                                                  major_actual,
                                                  minor_actual))

p50 <- list(tpr = withConfmat(either(data_t2,major_pred_50,minor_pred_50),
                         either(data_t2,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data_t2,major_pred_50,minor_pred_50),
                          either(data_t2,major_actual,minor_actual), fallout))

p30 <- list(tpr = withConfmat(either(data_t2,major_pred_30,minor_pred_30),
                         either(data_t2,major_actual,minor_actual), recall),
            fpr = withConfmat(either(data_t2,major_pred_30,minor_pred_30),
                          either(data_t2,major_actual,minor_actual), fallout))

tab <- tibble::tibble(
   AUC = c(incidence_auc_2$score,
         onset_auc$score,
         term_auc$score),
   `0.25th` = c(incidence_auc_2$quantiles[1],
                         onset_auc$quantiles[1],
                         term_auc$quantiles[1]),
   `97.5th` = c(incidence_auc_2$quantiles[2],
                         onset_auc$quantiles[2],
                         term_auc$quantiles[2]),
   `TPR_5` = c(round(p50$tpr, digits = 3),rep("-",2)),
   `FPR_5` = c(round(p50$fpr, digits = 3),rep("-",2)),
   `TPR_3` = c(round(p30$tpr, digits = 3),rep("-",2)),
   `FPR_3` = c(round(p30$fpr, digits = 3),rep("-",2)))


fnames <- names(tab)
fnames[c(4,6)] <- "TPR"
fnames[c(5,7)] <- "FPR"

row.names(tab) <- c("Incidence", "Onset", "Termination")

knitr::kable(tab,booktabs = TRUE, digits = 3, col.names = fnames) %>%
   add_header_above(c("","","Quantiles" = 2,"0.5" = 2, "0.3" = 2))

## @knitr table_4

library(dplyr)
library(zoo)
library(evallib)

data_t4 <- readRDS("data/prepped.rds")
source("R/evallib.R")

regions <- unique(data_t4$regionname)
names(regions) <- regions

region_results <- lapply(regions, function(region){
      sub <- data_t4[data_t4$regionname == region,]
      res <- aucWithCI(sub$combined, as.numeric(sub$major_actual | sub$minor_actual))
      getMetric <- function(data,metric,thresh){
         withConfmat(as.numeric(data[[paste0("major_pred_",thresh)]]|data[[paste0("minor_pred_",thresh)]]),
                     as.numeric(data$major_actual|data$minor_actual), metric)
      }
      res$q1 <- res$quantiles[1]
      res$q2 <- res$quantiles[2]
      res$quantiles <- NULL
      res$tpr_50 <- getMetric(sub,recall,"50") 
      res$fpr_50 <- getMetric(sub,fallout,"50") 
      res$tpr_30 <- getMetric(sub,recall,"30") 
      res$fpr_30 <- getMetric(sub,fallout,"30") 
      res
   }) %>%
   bind_rows()
region_results <- cbind(regions,region_results)
row.names(region_results) <- NULL

fnames <- c("Region","AUC", "0.25th","97.5th","TPR","FPR","TPR","FPR")

kable(region_results, format = "latex", booktabs = TRUE, digits = 3, col.names = fnames)%>%
   #kable_styling(latex_options = c("hold_position")) %>%
   add_header_above(c("","","Quantiles" = 2,".50" = 2, ".30" = 2))

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

## @knitr confmat

data_confmat <- readRDS("data/prepped.rds")

regions <- unique(data_confmat$regionname)
names(regions) <- regions
regions <- c(".*",regions)

results <- lapply(c("0.5"=0.5,"0.3"=0.3), function(threshold){
   results <- lapply(regions, function(region){
      sub <- data_confmat[str_detect(data_confmat$regionname,region),]
      confmat(as.numeric(sub$combined > threshold),
              as.numeric(sub$major_actual | sub$minor_actual)) %>%
         as.numeric()
   }) %>%
      bind_rows() %>%
      t()
}) %>%
   do.call(cbind, .) %>%
   as.data.frame()
names(results) <- rep(c("TN","FN","FP","TP"),2)
regions[1] <- "All"
row.names(results) <- regions 


kable(results, format = "latex", booktabs = TRUE, row.names = TRUE) %>%
   kable_styling(latex_options = c("hold_position")) %>%
   add_header_above(c("","P > 0.5" = 4, "P > 0.3" = 4))

## @knitr table_3

data_t3 <- readRDS("data/prepped.rds")

con <- dbConnect(SQLite(),"data/isq.sqlite")
latestConflicts <- dbGetQuery(con,"SELECT * FROM acd") %>%
   mutate(gwcode = as.numeric(gwcode_loc)) %>%
   group_by(gwcode) %>% 
   summarize(latestyear = max(year))

dbDisconnect(con)

top50 <- data_t3 %>%
   ungroup() %>%
   filter(year == 2018) %>%
   merge(latestConflicts, "gwcode") %>%
   mutate(`Observed (2018)` = case_when(
      major_actual == 1 ~ "Major",
      minor_actual == 1 ~ "Minor",
      TRUE ~ "None"
      ),
      `Latest conflict` = ifelse(!is.na(latestyear),
                                 as.character(latestyear),
                                 " - ")
   ) %>%
   select(Country = name,
          `Latest conflict`,
          `Observed (2018)`,
          Either = combined,
          Minor = minor_prob,
          Major = major_prob) %>%
   arrange(-Either) %>%
   head(50)

kable(top50, "latex", booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position"),
                 font_size = 7) %>%
   add_header_above(c("","Actual" = 2,"Predicted" = 3))

## @knitr figure_5
con <- dbConnect(SQLite(),"data/isq.sqlite")
nCountries <- dbGetQuery(con, "SELECT DISTINCT gwcode FROM isq") %>%
   unlist() %>%
   length()

propInConflict <- dbGetQuery(con, "
   SELECT DISTINCT year, gwcode_loc, MAX(intensity_level) AS intensity_level FROM acd
   WHERE year > 1969
   GROUP BY year, gwcode_loc") %>%
   group_by(year) %>%
   summarize(countries = length(unique(gwcode_loc)),
             minor = sum(intensity_level == 1),
             major = sum(intensity_level == 2),
             inminor = (countries - major) / nCountries,
             inmajor = (countries - minor) / nCountries,
             inconflict = countries / nCountries) %>%
   select(year,inmajor,inminor,inconflict) %>%
   gather(var,val,-year)

propPredicted <- dbGetQuery(con,"
   SELECT year,minor,major,combined FROM isq
   WHERE year > 2017 AND year < 2050") %>%
   group_by(year) %>%
   summarise(#major50 = sum(major > 0.5) / nCountries,
             major30 = sum(major > 0.3) / nCountries,
             #minor50 = sum(minor > 0.5) / nCountries,
             minor30 = sum(minor > 0.3) / nCountries,
             #comb50 = sum(combined > 0.5) / nCountries,
             comb30 = sum(combined > 0.3) / nCountries) %>%
   gather(var,val,-year)

both <- rbind(propInConflict, propPredicted)

ggplot(both,aes(x=year,y=val * 100,color=var)) + 
   geom_line() + 
   scale_x_continuous(breaks = seq(1970,2050,2), limits = c(1970,2048), expand = c(0,0)) +
   geom_vline(xintercept = 2018) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
   labs(x = "Year", y = "% In conflict")
dbDisconnect(con)

