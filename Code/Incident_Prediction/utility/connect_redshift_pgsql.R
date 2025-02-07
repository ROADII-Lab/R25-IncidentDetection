# Create connection to SDC Redshift, now using Postgre/SQL which does not rely on Java

# additional detail from https://aws.amazon.com/blogs/big-data/connecting-r-with-amazon-redshift/
# install.packages("RPostgreSQL", dep = T)

TESTCONN = F # Set to T to test connection 

source('utility/get_packages.R')

library(DBI) 
library(RPostgres) 

# Specify username and password with a file called Redshift_Credentials.csv:
if(!file.exists('~/Redshift_Credentials.csv')){
  root_dir = path.expand('~/')
  stop(paste('Create a two-column, two-row Redshift_Credentials.csv file with you username and Redshift_password to store in the root directory. The root directory is', root_dir))
} else {
  cred <- read.csv('~/Redshift_Credentials.csv', stringsAsFactors = FALSE)
}

redshift_host <- "prod-dot-sdc-redshift-cluster.cbohtkrxsojv.us-east-1.redshift.amazonaws.com"
redshift_port <- "5439"
redshift_user <- cred$username
redshift_password <- cred$password
redshift_db <- "dot_sdc_redshift_db"

conn <- dbConnect(
  RPostgres::Postgres(),
  host=redshift_host,
  port=redshift_port,
  user=redshift_user,
  password=redshift_password,
  dbname=redshift_db)

if(TESTCONN){
  
  # time functions: strftime and to_timestamp
  # WHERE pub_millis BETWEEN to_date('01-APR-17','DD-MON-YY') AND to_date('03-APR-17','DD-MON-YY')
  
  # add to end to limit the rows of queried data: LIMIT 50000
  
  
  alert_query_MD <- "SELECT * FROM dw_waze.alert 
                    WHERE state='MD' 
                    AND pub_utc_timestamp BETWEEN to_timestamp('2017-04-01 00:00:00','YYYY-MM-DD HH24:MI:SS') 
                                          AND     to_timestamp('2017-04-30 23:59:59','YYYY-MM-DD HH24:MI:SS')
                      " # end query. Use LIMIT 5000 to limit
  
  
  system.time(results <- dbGetQuery(conn, alert_query_MD)) # 40 s
  
  
  format(object.size(results), "Gb") # Was 1.3 Gb for April 2017 Md data in previous version of database, now 0.7 Gb
  
  summary(duplicated(results$alert_uuid)) # Avg 8x duplicated uuids
  
  system('free -m') # 16 GB ram, 6 used in previous instance. Now 31 Gb free 
  
  # is the pub_utc_timestamp direclty converted from pub_millis?
  
  timetest <- as.POSIXct(as.numeric(results$pub_millis)[1:500]/1000, origin = "1970-01-01", tz="America/New_York") # Time zone will need to be correctly configured for other States.
  
  data.frame(timetest, results[1:500, c("pub_millis", "pub_utc_timestamp")]) # Yes, timestamp is just the time represented in UTC, 4 hrs ahead of Eastern; we do not have 'first pull time' as in previous files. 
  
  lm(timetest ~ results[1:500, "pub_utc_timestamp"])
  
  # Test query, limit to 5 results. First all alerts, then specifying MD
  
  alert_query <- "SELECT * FROM dw_waze.alert LIMIT 5"
  results <- dbGetQuery(conn, alert_query)
  
  alert_query_MD <- "SELECT * FROM dw_waze.alert 
                  WHERE state='MD'
                  LIMIT 5"
  results <- dbGetQuery(conn, alert_query_MD)
  
  dbDisconnect(conn)
  
}
