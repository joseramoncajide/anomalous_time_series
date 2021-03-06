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


library(changepoint)
m.data=c(rnorm(100,0,1),rnorm(100,1,1),rnorm(100,0,1),rnorm(100,0.2,1))
ts.plot(m.data,xlab='Index')

# m.pelt=cpt.mean(lst1$`104268639`[,1],method='PELT')
# plot(m.pelt,type='l',cpt.col='blue',xlab='Index',cpt.width=4)
# cpts(m.pelt)

library(AnomalyDetection)

res4$site1$date <- as.POSIXct(res4$site1$date)
res4$site2$date <- as.POSIXct(res4$site2$date)

names(gadata$`104268639`)
sapply(gadata$`104268639`, function(x) x[,c("date", "sessions")])

anom <- function(i,data) {
  res <- AnomalyDetectionTs(res4$site2[,c("date", i)], max_anoms=0.02, direction='both', plot=F, threshold = 'p95')
  return(res)
}



anomalies.views <- list()
anomalies.df <- data_frame()

for(j in 1:length(gadata)) {
  anomalies <- list()
  view <- names(gadata)[j]
  view.df <- gadata[[view]]
  for(i in 2:ncol(view.df) ) {
    # print(i)
    # gadata$`104268639`$date <- as.POSIXct(gadata$`104268639`$date, format="%Y-%m-%d")
    view.df$date <- as.POSIXct(view.df$date, format="%Y-%m-%d")
    df <- bind_cols(view.df[1], view.df[i])
    # df <- bind_cols(gadata$`104268639`[1], gadata$`104268639`[i])
    # df <- bind_cols(gadata$`104268639`[1], gadata$`104268639`[2])
    print(tail(df))
    colnames(df)[2]
    name <- paste(colnames(df)[2],sep='')
    anomalies[[name]] <- AnomalyDetectionTs(df, max_anoms=0.02, direction='both', plot=T, threshold = 'p95')
    num_anomalies <- nrow(anomalies[[name]]$anoms)
    if(num_anomalies > 0L) {
      anomalies[[name]]$anoms$date <- as.Date(as.character(as.POSIXct(anomalies[[name]]$anoms$timestamp)))
      # anomalies[[name]]$last <- anomalies[[name]]$anoms  %>% 
      anomalies$last <- anomalies[[name]]$anoms  %>% 
        select(date, anoms) %>% 
        filter(date >= alert_from) %>% 
        mutate(metric = name, view=view, value = anoms) %>% 
        select(view, date, metric, value)
      
      anomalies.df <- bind_rows(anomalies.df, anomalies$last)
      anomalies[[name]]$last <- anomalies$last
        
      # names(anomalies[[name]]$last)[2] <- name
    } else {
      anomalies[[name]]$last <- NULL
    }
  }
  anomalies.views[[view]] <- list(data=anomalies)
}
names(anomalies.views)
anomalies.views$`104268639`$data$last
anomalies.views$`132411798`$data$sessions
anomalies.views$`132411798`$data$bounceRate

anomalies.df

######################



anomalies <- list()
for(i in 2:ncol(gadata$`104268639`) ) {
  # print(i)
  gadata$`104268639`$date <- as.POSIXct(gadata$`104268639`$date, format="%Y-%m-%d")
  df <- bind_cols(gadata$`104268639`[1], gadata$`104268639`[i])
  # df <- bind_cols(gadata$`104268639`[1], gadata$`104268639`[2])
  print(tail(df))
  colnames(df)[2]
  name <- paste(colnames(df)[2],sep='')
  anomalies[[name]] <- AnomalyDetectionTs(df, max_anoms=0.02, direction='both', plot=T, threshold = 'p95')
  num_anomalies <- nrow(anomalies[[name]]$anoms)
  if(num_anomalies > 0L) {
    anomalies[[name]]$anoms$date <- as.Date(as.character(as.POSIXct(anomalies[[name]]$anoms$timestamp)))
    anomalies[[name]]$last <- anomalies[[name]]$anoms  %>% 
      select(date, anoms) %>% 
      filter(date >= alert_from)
  } else {
    anomalies[[name]]$last <- NULL
  }
}

anomalies$sessions$last
anomalies$bounceRate$last
anomalies$bounceRate
as.Date(anomalies$item2$anoms$timestamp)

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



library(changepoint)
m.data=c(rnorm(100,0,1),rnorm(100,1,1),rnorm(100,0,1),rnorm(100,0.2,1))
ts.plot(m.data,xlab='Index')

m.pelt=cpt.mean(m.data,method='PELT')
plot(m.pelt,type='l',cpt.col='blue',xlab='Index',cpt.width=4)
cpts(m.pelt)
