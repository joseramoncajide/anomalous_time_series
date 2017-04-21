res5 <- lapply(res4, function(x) x[!(names(x) %in% c("date"))])

lst1 <- lapply(res5, function(x) ts(x, start = min(as.numeric(row.names(x))), end = max(as.numeric(row.names(x)))))
lst1 <- lapply(gadata, function(x) ts(x[-1], 
                                    start = c(min(year(x$date)), min(month(x$date))), 
                                    # end = c(max(year(x$date)), max(month(x$date))), 
                                    frequency = 365 
                                    ))
res4$site1
plot(lst1$`104268639`)
library(lubridate)

# lst1 <- lapply(res4, function(x) lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))), end = max(as.numeric(row.names(x))))))


# lst2 <- lapply(res4, function(x) {
#   x[] <- lapply(x, function(y) ts(y, start = min(as.numeric(row.names(x))),
#                                   end = max(as.numeric(row.names(x)))))
#   x})

# lst3 <- lapply(res4, function(x) xts(x, order.by = as.Date(sprintf("%s-01-01", row.names(x))))); class(lst3[[1]])






library(anomalousACM)
# Compute feature matrix for the real "dat0" data set
features0 <- tsmeasures(lst1$`104268639`, width = 24, window = 48)
# biplot on robust PCA for the "feature" matrix
biplot(features0)
hdr <- anomaly(features0, n = 1, plot = TRUE, method = "hdr")
plot(lst1$site2[, hdr$index])

library(AnomalyDetection)

res4$site1$date <- as.POSIXct(res4$site1$date)
res4$site2$date <- as.POSIXct(res4$site2$date)

names(gadata$`104268639`)
sapply(gadata$`104268639`, function(x) x[,c("date", "sessions")])

anom <- function(i,data) {
  res <- AnomalyDetectionTs(res4$site2[,c("date", i)], max_anoms=0.02, direction='both', plot=F, threshold = 'p95')
  return(res)
}


gadata$`104268639`
anomalies <- list()
for(i in 2:ncol(gadata$`104268639`) ) {
  # print(i)
  gadata$`104268639`$date <- as.POSIXct(gadata$`104268639`$date)
  df <- bind_cols(gadata$`104268639`[1], gadata$`104268639`[i])
  print(head(df))
  name <- paste('item:',i,sep='')
  anomalies[[name]] <- AnomalyDetectionTs(df, max_anoms=0.02, direction='both', plot=F, threshold = 'p95')
}

anomalies$`item:3`

foreach(i ncol(gadata$`104268639`))
cbind(gadata$`104268639`[1],gadata$`104268639`[2])

n <- 1:ncol(gadata$`104268639`)
ind <- data.frame(matrix(c(n, rep(NA, 3 - ncol(gadata$`104268639`)%%3)), byrow=F, nrow=3))
nonna <- sapply(ind, function(x) all(!is.na(x)))
ind <- ind[, nonna]

do.call(cbind, lapply(ind, function(i) gadata$`104268639`[, i]))

AnomalyDetectionTs(res4$site2[,c("date", i)], max_anoms=0.02, direction='both', plot=TRUE, threshold = 'p95')

res <- AnomalyDetectionTs(res4$site2[,c("date", "sessions")], max_anoms=0.02, direction='both', plot=TRUE, threshold = 'p95')
res$plot
res$anoms
