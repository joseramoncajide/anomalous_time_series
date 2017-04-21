Sys.setenv(GA_AUTH_FILE = "./.httr-oauth")
library(googleAnalyticsR)
library(googleAuthR)
library(lubridate)
library(parallel)
library(doParallel)
library(data.table)
library(tidyverse)
library(stringr)

# gar_auth()


ga_id <- '83321718' #EAM

numCores <- detectCores()
cl <- makeCluster(numCores - 6) 
registerDoParallel(cl) 

sessions.ga <- c(
  metrics = "sessions,users,pageviews,avgSessionDuration,percentNewSessions",
  dimensions = "date"
)

bounces.ga <- c(
  metrics = 'bounces',
  dimensions = 'date'
)

ga_queries <- list(site1 = bounces.ga, site2 = sessions.ga)

getGaData4 <- function(query, ga_id, date_range) {
  require(data.table)
  # service_token <- gar_auth_service(json_file="./ga-api-r-30a82c2d29e7.json")
  # googleAuthR::gar_auth()
  # ga_id <- view
  ga <- google_analytics_4(
    ga_id,
    date_range = as.character(date_range),
    metrics = as.vector(unlist(strsplit(query['metrics'], ","), use.names = F)),
    dimensions = as.vector(unlist(strsplit(query['dimensions'], ","), use.names = F))
  )
  return(as_data_frame(ga))
}



start.Date <- function(last_Day){
  
  days <- floor_date(seq(last_Day-365,last_Day,by='week'), 'week') # Ojo. Tiene que devolver una semana más.
  from.date <- days - 30
  # from.date <- days[weekdays(days+1)=='lunes']
  return(min(from.date))
  
}

today <- as.Date(format(Sys.Date(), "%Y-%m-%d"))

last_Day <- floor_date(today, "week")

first_Day <- start.Date(last_Day)

date_range <- c(first_Day ,last_Day)

getGaData4(bounces.ga, ga_id, date_range)
getGaData4(sessions.ga, ga_id, date_range)



res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate", "data.table", "dplyr"), .combine='list', .multicombine=TRUE, .inorder = T, .final = function(x) setNames(x, names(ga_queries)) ) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)

res4[[1]]


ga_account_list.df <- as_data_frame(ga_account_list())
ga_account_list.df <- ga_account_list.df %>% filter(str_detect(viewName, 'Global') & level == "PREMIUM" )
# ga_account_list.df$viewId
# ga_view_list(accountId = "80796254", webPropertyId = "UA-80796254-1") %>% View()

gadata <- google_analytics(id = ga_account_list.df$viewId, 
                           start=first_Day, end=last_Day, 
                           metrics = c("sessions", "bounceRate"), 
                           dimensions = c("date"))
names(gadata) <- ga_account_list.df$viewId
gadata$`104268639`
# merge.by.time <- function(a, b) {  
#   merge(a, b, by='month', suffixes=c('', ncol(a)))
# }
# 
# 
# res4 <- foreach(i=1:length(ga_queries), .packages = c("googleAnalyticsR","googleAuthR","lubridate", "data.table"), .combine='merge.by.time', .multicombine=TRUE, .inorder = T) %dopar% getGaData4(ga_queries[[i]], ga_id, date_range)
# 
# 
# 
