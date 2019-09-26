
library(ggplot2)

data <- readRDS("data/prepped.rds")
source("R/evallib.R")

# =================================================

data$either <- either(data,major_actual,minor_actual)

getROC <- function(data,outcome,pred){
   pROC::roc(data[[as.character(substitute(outcome))]],
       data[[as.character(substitute(pred))]])
}



comb_curve <- roc(data$combined, either(data,major_actual,minor_actual))
min_curve <- roc(data$minor_prob, data$minor_actual) 
max_curve <- roc(data$major_prob, data$major_actual) 

comb_curve$type <- "Either"
min_curve$type <- "Minor conflict"
max_curve$type <- "Major conflict"

colors <- c(Either = "#0095b6",
            `Minor conflict` = "#e88e5a",
            `Major conflict` = "#7bb661")


curves <- rbind(comb_curve,min_curve,max_curve)

plot <- ggplot(curves, aes(x = fallout, y = recall, color = factor(type)))+
   geom_line(size = 1.5) +
   scale_color_manual(values = colors) +
   labs(color = "Conflict type",
        y = "TPR",x = "FPR")

ciplot <- ggplot(curves) +
   geom_line(aes(x = fallout_mean, y = recall_mean, color = factor(type)))+
   geom_polygon(aes(x = fallout_lower,y = recall_lower) )
