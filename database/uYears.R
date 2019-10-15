
cshp <- read_sf("cshapes/cshapes.shp")
# ================================================

start <- 1946
end <- 2011

years <- seq(start,end,1)

res <- sapply(years, function(year){
   sub <- cshp[cshp$COWSYEAR <= year & cshp$COWEYEAR >= year,]
   nrow(sub)
})

x <- data.frame(year = years, n_countries = res)
