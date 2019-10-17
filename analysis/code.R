
## @knitr prepData 
# Here data is retrieved from the database and prepared for analysis.

NITER <- 20
DB <- "data/pac.sqlite"

predictions <- " SELECT DISTINCT * FROM predictions_2010_2050 WHERE year <= 2018 " %>%
   getfrom(DB)

oos <- "SELECT * FROM predictions_2001_2009" %>% 
     getfrom(DB)

occurrence <- "SELECT * FROM acd" %>%
   getfrom(DB)

cinfo <- "SELECT countries.gwcode, name, regions.regionname FROM countries
          JOIN regions ON countries.isqregion = regions.isqregion" %>%
   getfrom(DB)

# ================================================
# This function "preps" the prediction data by adding binary
# values for thresholded predictions, merging with occurrence
# data (ACD) and country info data (for regions / naming),
# and adding an "either" column that is positive for both minor
# and major conflict.
prepPredictionData <- function(predictions,occurrence,cinfo){
   merge(predictions,occurrence,c("gwcode","year"),all.x = TRUE) %>%
      merge(cinfo,by="gwcode") %>%
      unique() %>%
      rename(minor_prob = minor,
             major_prob = major) %>%
      mutate(minor_actual = fixna(minor_actual),
             major_actual = fixna(major_actual),
             minor_pred_30 = as.numeric(minor_prob > 0.3),
             minor_pred_50 = as.numeric(minor_prob > 0.5),
             major_pred_30 = as.numeric(major_prob > 0.3),
             major_pred_50 = as.numeric(major_prob > 0.5),
             either_pred_30 = as.numeric(minor_pred_30 | major_pred_30),
             either_pred_50 = as.numeric(minor_pred_50 | major_pred_50),
             either_actual = as.numeric(minor_actual | major_actual))
}

predictions_2010_2018 <- prepPredictionData(predictions,occurrence,cinfo)
predictions_2001_2009 <- prepPredictionData(oos,occurrence,cinfo)


## @knitr rollingFlip

rollingFlip <- function(x,mode){
   flip <- function(x) as.numeric(!as.logical(x))
   mode_num <- switch(mode, onset = 1, term = 0)
   res <- sapply(1:length(x), function(i){
      if(i > 1){
         as.numeric(x[i-1] == flip(mode_num) &
                    x[i] == mode_num)
      } else {
         NA 
   }})
}

## @knitr onsetTerm

onsetAndTerm <- function(data,...){
   # Is data grouped?
   if(!is.null(groups(data)[[1]])) {
         grplen <- length(unique(data[[groups(data)[[1]]]]))
   } else {
      grplen <- 0
   }

   # If so... 
   if(grplen > 1){
      grpvar <- groups(data)[[1]] # Only supports one grp. variable 
      prev <<- 0
      res <- lapply(unique(data[[grpvar]]), function(grpval){
         nmbr <- as.character(grpval)
         sub <- arrange(data[data[[grpvar]] == grpval,],year)
         onsetAndTerm(sub, ...)}) %>%
         bind_rows()
      res

   # If not... 
   } else {
      variables <- sapply(substitute(list(...)),as.character)[-1]
      for(v in variables){
         data[[paste0("onset_",v)]] <- rollingFlip(data[[v]],"onset")
         data[[paste0("term_",v)]] <- rollingFlip(data[[v]],"term")
      }
      data
   }

}

predictions_2010_2018 <- predictions_2010_2018 %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50) %>%
   ungroup()

## @knitr table_2_prep

data_t2 <- predictions_2010_2018 %>% filter(year > 2010)

summaryTable <- function(data){
   onset_auc <- aucWithCI(data$combined, either(data, 
                                                onset_major_actual,
                                                onset_minor_actual))
   term_auc <- aucWithCI(data$combined,either(data,
                                              term_major_actual,
                                              term_minor_actual))
   incidence_auc <- aucWithCI(data$combined,either(data,
                                                   major_actual,
                                                   minor_actual))
   p50 <- list(tpr = withConfmat(
                            either(data,major_pred_50,minor_pred_50),
                            either(data,major_actual,minor_actual), recall),
               fpr = withConfmat(
                             either(data,major_pred_50,minor_pred_50),
                             either(data,major_actual,minor_actual), fallout))

   p30 <- list(tpr = withConfmat(
                            either(data,major_pred_30,minor_pred_30),
                            either(data,major_actual,minor_actual), recall),
               fpr = withConfmat(
                             either(data,major_pred_30,minor_pred_30),
                             either(data,major_actual,minor_actual), fallout))
   tibble::tibble(
      AUC = c(incidence_auc$score,
            onset_auc$score,
            term_auc$score),
      `0.25th` = c(incidence_auc$quantiles[1],
                            onset_auc$quantiles[1],
                            term_auc$quantiles[1]),
      `97.5th` = c(incidence_auc$quantiles[2],
                            onset_auc$quantiles[2],
                            term_auc$quantiles[2]),
      `TPR_5` = c(round(p50$tpr, digits = 3),rep("-",2)),
      `FPR_5` = c(round(p50$fpr, digits = 3),rep("-",2)),
      `TPR_3` = c(round(p30$tpr, digits = 3),rep("-",2)),
      `FPR_3` = c(round(p30$fpr, digits = 3),rep("-",2)))
}

renderSummaryTable <- function(table){
   fnames <- names(table)
   fnames[c(4,6)] <- "TPR"
   fnames[c(5,7)] <- "FPR"
   row.names(table) <- c("Incidence", "Onset", "Termination")
   knitr::kable(table,"latex",booktabs = TRUE, digits = 3, col.names = fnames) %>%
      add_header_above(c("","","DeLong Quantiles" = 2,"0.5" = 2, "0.3" = 2))
}

# These are rendered below:
tab <- summaryTable(data_t2)
tab_16_18 <- summaryTable(filter(data_t2, year %in% c(2016,2017,2018)))

## @knitr table_2_output
x <- renderSummaryTable(tab)
writeLines(x,glue("{TABLEFOLDER}/table_2_1.tex"))
x

## @knitr table_2_output_16_18
x <- renderSummaryTable(tab_16_18)
writeLines(x,glue("{TABLEFOLDER}/table_2_2.tex"))
x

## @knitr table_4_prep

data_t4 <- predictions_2010_2018

regions <- unique(data_t4$regionname)

region_results <- lapply(regions, function(region){
      # Build a list of results of several metrics

      sub <- data_t4[data_t4$regionname == region,]
      res <- aucWithCI(sub$combined, as.numeric(sub$major_actual | sub$minor_actual))
      getMetric <- function(data,metric,thresh){
         # Get metric using a confusion matrix of predictions,
         # given thresh, and actual outcomes. 
         withConfmat(as.numeric(data[[paste0("major_pred_",thresh)]]|
                                data[[paste0("minor_pred_",thresh)]]),
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

## @knitr table_4_output

columnNames <- c("Region","AUC", "0.25th","97.5th","TPR","FPR","TPR","FPR")

x <- knitr::kable(region_results, "latex", 
                  booktabs = TRUE, digits = 3, 
                  col.names = columnNames)%>%
   add_header_above(c("","","Quantiles" = 2,".50" = 2, ".30" = 2))

writeLines(x,glue("{TABLEFOLDER}/table_4.tex"))
x

## @knitr figure_4_prep

data_f4 <- predictions_2010_2018

curves <- lapply(list(oos = predictions_2001_2009, neo = predictions_2010_2018),
                 function(dat){
      list(pr = metricCurve(dat$combined,either(dat,minor_actual,major_actual),
                            precision,recall),
           roc = metricCurve(dat$combined,either(dat,minor_actual,major_actual),
                             fallout,recall))
   })

for(n in c("pr","roc")){
   curvenames <- c("01-09","10-18")
   for(i in c(1,2)){
      curves[[i]][[n]]$name <- curvenames[i]
   }
}

colors <- c("01-09" = "#0093c9",
            "10-18" = "#c8001f")

combroc <- ggplot(rbind(curves[[1]]$roc,curves[[2]]$roc), 
                  aes(x = fallout, y = recall, color = name))+
   geom_path(size = 1.5, alpha = 0.8) + 
   scale_color_manual(values=colors)

combpr <- ggplot(rbind(curves[[1]]$pr,curves[[2]]$pr), 
                  aes(x = precision, y = recall, color = name))+
   geom_path(size = 1.5, alpha = 0.8) + 
   scale_color_manual(values=colors)

aucs <- sapply(list(list(curves[[1]]$roc$fallout,curves[[1]]$roc$recall),
                    list(curves[[2]]$roc$fallout,curves[[2]]$roc$recall),
                    list(curves[[1]]$pr$precision,curves[[1]]$pr$recall),
                    list(curves[[2]]$pr$precision,curves[[2]]$pr$recall)),
               function(args){do.call(auc,args)})

aucs <- data.frame(Groups = c("01-09","10-18","01-09","10-18"),
                   AUC = abs(aucs))

## @knitr figure_4_output
# Deprecated?

auc <- plt$results$auc
dgts <- 4
aucCaption <- glue("AUC: {round(auc$score, dgts)} ({round(auc$quantiles[1], dgts)} - {round(auc$quantiles[2], dgts)})")

x <- plt$plot + 
   labs(x = "Specificity",
        y = "Sensitivity",
        title = "Bootstrap ROC curve",
        subtitle = aucCaption, 
        caption = glue("({NITER} random draws)"))

ggsave(glue("{PLOTFOLDER}/figure_4.{DEVICE}"), x,
       device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 

x

## @knitr figure_4_bothcurves

ggsave(glue("{PLOTFOLDER}/rocs.{DEVICE}"),combroc,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
ggsave(glue("{PLOTFOLDER}/precrec.{DEVICE}"),combpr,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 

grid.arrange(combroc+
                theme(legend.position="none")+
                labs(x="Fallout",y="Recall"),
             combpr +
                labs(x = "Precision",y="Recall", color = "") +
                theme(legend.position = "bottom") +
                scale_x_continuous(limits = c(0,1)),ncol = 2)

## @knitr figure_4_table

x <- kable(aucs,"latex",booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position")) %>%
   pack_rows("ROC Curve",1,2) %>%
   pack_rows("Precision-Recall Curve", 3,4)

writeLines(x,glue("{TABLEFOLDER}/curves_AUCs.tex"))

x

## @knitr confmat_prep

data_confmat <- predictions_2010_2018

regions <- unique(data_confmat$regionname)
names(regions) <- regions
regions <- c(".*",regions)

results <- lapply(c("0.5"=0.5,"0.3"=0.3), function(threshold){

   results <- lapply(regions, function(region){
      sub <- data_confmat[str_detect(data_confmat$regionname,region),]
      confmat(as.numeric(sub$combined > threshold),
              either(sub, major_actual, minor_actual)) %>%
         as.numeric()
   }) %>%
      bind_rows() %>%
      t()

}) %>%
   do.call(cbind, .) %>%
   as.data.frame()

## @knitr confmat_output

names(results) <- rep(c("TN","FN","FP","TP"),2)
regions[1] <- "All"
row.names(results) <- regions 

x <- knitr::kable(results, "latex", booktabs = TRUE, row.names = TRUE) %>%
   kable_styling(latex_options = c("hold_position")) %>%
   add_header_above(c("","P > 0.5" = 4, "P > 0.3" = 4))
writeLines(x,glue("{TABLEFOLDER}/conftable.tex"))
x

## @knitr table_3

data_t3 <- predictions_2010_2018 

latestConflicts <- occurrence %>%
   group_by(gwcode) %>% 
   summarize(latestyear = max(year))

before09 <- occurrence %>%
   select(gwcode,year) %>%
   filter(year < 2010) %>%
   group_by(gwcode) %>%
   summarize(before09 = max(year))

top50 <- data_t3 %>%
   filter(year == 2018) %>%
   merge(latestConflicts, "gwcode") %>%
   merge(before09, "gwcode") %>%
   mutate(`2018` = case_when(
      major_actual == 1 ~ "Major",
      minor_actual == 1 ~ "Minor",
      TRUE ~ "None"),
      `Before 2018` = ifelse(!is.na(latestyear),
                             as.character(latestyear),
                             " - "),
      `Before 2009` = ifelse(!is.na(before09),
                             as.character(before09),
                             " - ")) %>%
   select(Country = name,
          `Before 2018`,
          `Before 2009`,
          `2018`,
          Either = combined,
          Minor = minor_prob,
          Major = major_prob) %>%
   arrange(-Either) %>%
   head(50)

x <- kable(top50, "latex", booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position"),
                 font_size = 7) %>%
   add_header_above(c("","Latest observed" = 2,"Observed","Predicted 2018" = 3))
writeLines(x,glue("{TABLEFOLDER}/table_3.tex"))
x

## @knitr figure_5_prep

con <- dbConnect(SQLite(),DB)

# N countries is given by number of unique country-codes in prediction
# dataset
n_countries <- 169
nCountries <- dbGetQuery(con,"
                         SELECT * FROM ncountries")

propInConflict <- dbGetQuery(con, "
   SELECT DISTINCT year, gwcode, MAX(intensity_level) AS intensity_level FROM acd
   WHERE year > 1969
   GROUP BY year, gwcode") %>%
   merge(nCountries,"year") %>%
   group_by(year) %>%
   summarize(countries = length(unique(gwcode)),
             minor = sum(intensity_level == 1),
             major = sum(intensity_level == 2),
             #inminor = (countries - major) / n_countries,
             inmajor = (countries - minor) / max(n_countries),
             inconflict = countries / max(n_countries)) %>%
   select(year,
          inmajor,
          #inminor,
          inconflict) %>%
   gather(var,val,-year)

propPredicted <- dbGetQuery(con,"
   SELECT year,minor,major,combined FROM predictions_2010_2050 
   WHERE year > 2008 AND year < 2050") %>%
   merge(nCountries,"year") %>%
   group_by(year) %>%
   summarise(#major50 = sum(major > 0.5) / nCountries,
             #major30 = sum(major > 0.3) / nCountries,
             #inmajor = sum(major > 0.3) / max(n_countries),
             inmajor = mean(major),
             #minor50 = sum(minor > 0.5) / nCountries,
             #minor30 = sum(minor > 0.3) / nCountries,
             #inminor = sum(minor > 0.3) / max(n_countries),
             #comb50 = sum(combined > 0.5) / nCountries,
             #comb30 = sum(combined > 0.3) / nCountries) %>%
             inconflict = mean(combined)) %>%
             #inconflict = sum(combined > 0.3) / max(n_countries)) %>%
   gather(var,val,-year)

propInConflict$type <- "Actual"
propPredicted$type <- "Predicted"
both <- rbind(propInConflict, propPredicted)

both$var <- fct_recode(both$var,
                       Minor = "inminor",
                       Major = "inmajor",
                       Either = "inconflict")

dbDisconnect(con)

## @knitr figure_5_output

colors <- c(comb30 = "#800080",
            inconflict = "#bf08a4",
            Either = "#bf08a4",

            inmajor = "#7eb6ff",
            Major = "#7eb6ff",
            major30 = "#7eb6ff",

            inminor  = "#c8001f",
            Minor = "#c8001f",
            minor30 = "#c8001f")

fig5 <- ggplot(both,aes(x=year,y=val * 100,color=var, linetype = type)) + 
   geom_line(size = 2) + 
   scale_x_continuous(breaks = seq(1970,2050,2), 
                      limits = c(1970,2048), expand = c(0,0)) +
   scale_color_manual(values = colors) + 
   geom_vline(xintercept = 2018) +
   geom_vline(xintercept = 2009) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
   labs(x = "Year", y = "% In conflict", color = "", linetype = "") +
   theme(legend.position = "bottom")
ggsave(glue("{PLOTFOLDER}/figure_5.{DEVICE}"),fig5,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
fig5

## @knitr oos_prep

dat <- haven::read_dta("data/predactual_01_09.dta") %>%
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


## @knitr oos_table_2

predictions_2001_2009_nonfunc <- predictions_2001_2009 %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50) %>%
   ungroup()

predictions_2001_2009 <- pred_actual

incidence01_09 <- list()

incidence01_09$score <- aucFromPA(predictions_2001_2009$combined, 
                                  predictions_2001_2009$either_actual)

incidence01_09$quantiles <- pROC::ci.auc(predictions_2001_2009$either_actual, 
                                         predictions_2001_2009$combined)

subset <- predictions_2001_2009 %>% filter(year > 2006)

onset <- bootstrappedROC(subset$combined, as.numeric(subset$onset_minor_actual |
                                                     subset$onset_major_actual),
                         res = 0.1, draws = NITER,
                         parallel = TRUE)[[2]]

term <- bootstrappedROC(subset$combined, as.numeric(subset$term_minor_actual |
                                                    subset$term_major_actual),
                         res = 0.1, draws = NITER,
                         parallel = TRUE)[[2]]

incidence <- list()
incidence$score <- aucFromPA(subset$combined, subset$either_actual)
incidence$quantiles <- pROC::ci.auc(subset$either_actual, subset$combined)

getMetric <- function(data,thresh,metric){ 
   tpr = withConfmat(as.numeric(data$combined > thresh), 
                      either(data, major_actual, minor_actual), metric)
}

getRates <- function(data, functions = list(recall = recall, fallout = fallout), 
                     digits = NULL){ 
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


rates <- lapply(list(full = predictions_2001_2009,post07 = subset), 
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

## @knitr oos_table_4

plt <- cintervalplot(predictions_2001_2009$combined, 
                     predictions_2001_2009$either_actual, res = 0.001)
plt$plot

## @knitr summaryfigures

summaryRates <- getRates(diq, functions = list(fallout = fallout, 
                         recall = recall, 
                         precision = precision, 
                         accuracy = accuracy),
                         digits = 3)


