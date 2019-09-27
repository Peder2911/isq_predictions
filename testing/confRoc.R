
library(evallib)
library(ggplot2)

tdata <- examplePredictions(n = 300, noise = 0.8)
res <- bootstrappedROC(tdata$pred, tdata$actual)

# =================================================

stacked <- data.frame(fallout = c(res$roc$fallout_025,rev(res$roc$fallout_975)),
                      recall = c(res$roc$recall_975,rev(res$roc$recall_025)))

cintervalplot <- ggplot(res$roc,aes (x = fallout_mean, y = recall_mean))+
   geom_line() +
   geom_polygon(aes(x = fallout,y = recall), data = stacked, alpha = 0.2)
